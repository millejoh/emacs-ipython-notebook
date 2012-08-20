;;; ein-notebook.el --- Notebook module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notebook.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notebook.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebook.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Coding rule about current buffer.
;; A lot of notebook and cell functions touches to current buffer and
;; it is not ideal to wrap all these functions by `with-current-buffer'.
;; Therefore, when the function takes `notebook' to the first argument
;; ("method" function), it is always assumed that the current buffer
;; is the notebook buffer.  **However**, functions called as callback
;; (via `url-retrieve', for example) must protect themselves by
;; calling from unknown buffer.

;;; Code:


(eval-when-compile (require 'cl))
(require 'ewoc)
(eval-when-compile (require 'auto-complete nil t))

(require 'ein-utils)
(require 'ein-log)
(require 'ein-node)
(require 'ein-kernel)
(require 'ein-cell)
(require 'ein-worksheet)
(require 'ein-completer)
(require 'ein-pager)
(require 'ein-events)
(require 'ein-notification)
(require 'ein-kill-ring)
(require 'ein-query)
(require 'ein-pytools)


;;; Configuration

(defcustom ein:notebook-enable-undo 'yes
  "Configure undo in notebook buffers.

`no' : symbol
    Do not use undo in notebook buffers.  It is the safest option.
`yes' : symbol
    Enable undo in notebook buffers.  You can't undo after
    modification of cell (execution, add, remove, etc.).  This
    is default.
`full' : symbol
    Enable full undo in notebook buffers.  It is powerful but
    sometime (typically after the cell specific commands) undo
    mess up notebook buffer.  Use it on your own risk.  When the
    buffer is messed up, you can just redo and continue editing,
    or save it once and reopen it if you want to be careful.

You need to reopen the notebook buffer to reflect the change of
this value."
  :type '(choice (const :tag "No" no)
                 (const :tag "Yes" yes)
                 (const :tag "Full" full))
  :group 'ein)

(defun ein:notebook-empty-undo-maybe ()
  "Empty `buffer-undo-list' if `ein:notebook-enable-undo' is `yes'."
  (when (eq ein:notebook-enable-undo 'yes)
    (setq buffer-undo-list nil)))

(defcustom ein:notebook-discard-output-on-save 'no
  "Configure if the output part of the cell should be saved or not.

`no' : symbol
    Save output. This is the default.
`yes' : symbol
    Always discard output.
a function
    This function takes two arguments, notebook and cell.  Return
    `t' to discard output and return `nil' to save.  For example,
    if you don't want to save image output but other kind of
    output, use `ein:notebook-cell-has-image-output-p'.

Note that using function needs EIN lisp API, which is not determined
yet.  So be careful when using EIN functions.  They may change."
  :type '(choice (const :tag "No" 'no)
                 (const :tag "Yes" 'yes)
                 ;; FIXME: this must be go to the customize UI after
                 ;; clarifying the notebook lisp API.
                 ;; (function :tag "Predicate" (lambda () t))
                 )
  :group 'ein)

(defun ein:notebook-cell-has-image-output-p (-ignore- cell)
  (ein:cell-has-image-ouput-p cell))

(defun ein:notebook-discard-output-p (notebook cell)
  "Return non-`nil' if the output must be discarded, otherwise save."
  (case ein:notebook-discard-output-on-save
    (no nil)
    (yes t)
    (t (funcall ein:notebook-discard-output-on-save notebook cell))))

;; As opening/saving notebook treats possibly huge data, define these
;; timeouts separately:

(defcustom ein:notebook-querty-timeout-open (* 60 1000) ; 1 min
  "Query timeout for opening notebook.
If you cannot open large notebook because of timeout error, try
to increase this value.  Setting this value to `nil' means to use
global setting.  For global setting and more information, see
`ein:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defcustom ein:notebook-querty-timeout-save (* 60 1000) ; 1 min
  "Query timeout for saving notebook.
Similar to `ein:notebook-querty-timeout-open', but for saving
notebook.  For global setting and more information, see
`ein:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defvar ein:notebook-after-rename-hook nil
  "Hooks to run after notebook is renamed successfully.
Current buffer for these functions is set to the notebook buffer.")


;;; Class and variable

(defvar ein:base-kernel-url "/")
;; Currently there is no way to know this setting.  Maybe I should ask
;; IPython developers for an API to get this from notebook server.

(defvar ein:notebook-pager-buffer-name-template "*ein:pager %s/%s*")
(defvar ein:notebook-buffer-name-template "*ein: %s/%s*")

(defvar ein:notebook-save-retry-max 1
  "Maximum retries for notebook saving.")

(defstruct ein:$notebook
  "Hold notebook variables.

`ein:$notebook-url-or-port'
  URL or port of IPython server.

`ein:$notebook-notebook-id' : string
  uuid string

`ein:$notebook-kernel' : `ein:$kernel'
  `ein:$kernel' instance.

`ein:$notebook-pager'
  Variable for `ein:pager-*' functions. See ein-pager.el.

`ein:$notebook-dirty' : boolean
  Set to `t' if notebook has unsaved changes.  Otherwise `nil'.

`ein:$notebook-metadata' : plist
  Notebook meta data (e.g., notebook name).

`ein:$notebook-name' : string
  Notebook name.

`ein:$notebook-nbformat' : integer
  Notebook file format version.

`ein:$notebook-nbformat-minor' : integer
  Notebook file format version.

`ein:$notebook-events' : `ein:$events'
  Event handler instance.

`ein:$notebook-worksheets' : list of `ein:worksheet'
  List of worksheets.
"
  url-or-port
  notebook-id
  kernel
  pager
  dirty
  metadata
  notebook-name
  nbformat
  nbformat-minor
  events
  worksheets
  )

;; FIXME: Remove `ein:%notebook%' when worksheet is fully implemented.
(ein:deflocal ein:%notebook% nil
  "Buffer local variable to store an instance of `ein:$notebook'.")
(define-obsolete-variable-alias 'ein:notebook 'ein:%notebook% "0.1.2")

(defmacro ein:notebook-with-cell (cell-p &rest body)
  "Execute BODY if in cell with a dynamically bound variable `cell'.
When CELL-P is non-`nil', it is called with the current cell object
and BODY will be executed only when it returns non-`nil'.  If CELL-P
is `nil', BODY is executed with any cell types."
  (declare (indent 1))
  `(let ((cell (ein:notebook-get-current-cell)))
     (if ,(if cell-p `(and cell (funcall ,cell-p cell)) 'cell)
         (progn ,@body)
       (ein:log 'warn "Not in cell"))))

(defmacro ein:notebook-with-cells-in-region (&rest body)
  "Similar to `ein:notebook-with-cell' but sets a list of cells to `cells'.
Cells are fetched by `ein:notebook-get-cells-in-region-or-at-point'."
  (declare (indent 0))
  `(let* ((cells (ein:notebook-get-cells-in-region-or-at-point)))
     (if cells
         (progn ,@body)
       (ein:log 'warn "Not in cell"))))

(defmacro ein:notebook-with-buffer (notebook &rest body)
  "Execute BODY with current buffer setting at the one of NOTEBOOK."
  ;; FIXME: MANY functions can use this macro.  Refactor them!
  (declare (indent 1))
  `(with-current-buffer (ein:notebook-buffer ,notebook)
     ,@body))

(defun ein:notebook-new (url-or-port notebook-id &rest args)
  (let ((notebook (apply #'make-ein:$notebook
                         :url-or-port url-or-port
                         :notebook-id notebook-id
                         args)))
    notebook))

(defun ein:notebook-del (notebook)
  "Destructor for `ein:$notebook'."
  (ein:log-ignore-errors
    (ein:kernel-del (ein:$notebook-kernel notebook))))

(defun ein:notebook-buffer (notebook)
  "Return the buffer that is associated with NOTEBOOK."
  ;; FIXME: Find a better way to define notebook buffer! (or remove this func)
  (loop for ws in (ein:$notebook-worksheets notebook)
        if (ein:worksheet-buffer ws) return it))

(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

(defun ein:notebook-url (notebook)
  (ein:notebook-url-from-url-and-id (ein:$notebook-url-or-port notebook)
                                    (ein:$notebook-notebook-id notebook)))

(defun ein:notebook-url-from-url-and-id (url-or-port notebook-id)
  (ein:url url-or-port "notebooks" notebook-id))

(defun ein:notebook-pop-to-current-buffer (&rest -ignore-)
  "Default callback for `ein:notebook-open'."
  (pop-to-buffer (current-buffer)))

(defun ein:notebook-open (url-or-port notebook-id &optional callback cbargs)
  "Open notebook of NOTEBOOK-ID in the server URL-OR-PORT.
Opened notebook instance is returned.  Note that notebook might not be
ready at the time when this function is executed.

After the notebook is opened, CALLBACK is called as::

  \(apply CALLBACK notebook CREATED CBARGS)

where the second argument CREATED indicates whether the notebook
is newly created or not.  When CALLBACK is specified, buffer is
**not** brought up by `pop-to-buffer'.  It is caller's
responsibility to do so.  The current buffer is set to the
notebook buffer when CALLBACK is called."
  (unless callback (setq callback #'ein:notebook-pop-to-current-buffer))
  (let ((buffer (ein:notebook-get-opened-buffer url-or-port notebook-id)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (when callback
            (apply callback ein:%notebook% nil cbargs))
          ein:%notebook%)
      (ein:notebook-request-open url-or-port notebook-id callback cbargs))))

(defun ein:notebook-request-open (url-or-port notebook-id
                                              &optional callback cbargs)
  "Request notebook of NOTEBOOK-ID to the server at URL-OR-PORT.
Return `ein:$notebook' instance.  Notebook may not be ready at
the time of execution.

CALLBACK is called as \(apply CALLBACK notebook t CBARGS).  The second
argument `t' indicates that the notebook is newly opened.
See `ein:notebook-open' for more information."
  (let ((url (ein:notebook-url-from-url-and-id url-or-port notebook-id))
        (notebook (ein:notebook-new url-or-port notebook-id)))
    (ein:log 'debug "Opening notebook at %s" url)
    (ein:query-singleton-ajax
     (list 'notebook-open url-or-port notebook-id)
     url
     :timeout ein:notebook-querty-timeout-open
     :parser #'ein:json-read
     :success (cons #'ein:notebook-request-open-callback-with-callback
                    (list notebook callback cbargs)))
    notebook))

(defun ein:notebook-request-open-callback-with-callback (packed &rest args)
  (let ((notebook (nth 0 packed))
        (callback (nth 1 packed))
        (cbargs (nth 2 packed)))
    (apply #'ein:notebook-request-open-callback notebook args)
    (when callback
      (with-current-buffer (ein:notebook-buffer notebook)
        (apply callback notebook t cbargs)))))

(defun* ein:notebook-request-open-callback (notebook &key status data
                                                     &allow-other-keys)
  (ein:log 'debug "URL-RETRIEVE nodtebook-id = %S, status = %S"
           (ein:$notebook-notebook-id notebook)
           status)
  (let ((notebook-id (ein:$notebook-notebook-id notebook)))
    (ein:notebook-bind-events notebook (ein:events-new))
    (ein:notebook-start-kernel notebook)
    (ein:notebook-from-json notebook data)
    (ein:notebook-put-opened-notebook notebook)
    (ein:notebook--check-nbformat data)
    (ein:log 'info "Notebook %s is ready"
             (ein:$notebook-notebook-name notebook))))

(defun ein:notebook-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))

(defun ein:notebook--different-number (n1 n2)
  (and (numberp n1) (numberp n2) (not (= n1 n2))))

(defun ein:notebook--check-nbformat (data)
  "Warn user when nbformat is changed on server side.
See https://github.com/ipython/ipython/pull/1934 for the purpose
of minor mode."
  ;; See `Notebook.prototype.load_notebook_success'
  ;; at IPython/frontend/html/notebook/static/js/notebook.js
  (destructuring-bind (&key nbformat orig_nbformat
                            nbformat_minor orig_nbformat_minor
                            &allow-other-keys)
      data
    (cond
     ((ein:notebook--different-number nbformat orig_nbformat)
      (ein:display-warning
       (format "Notebook major version updated (v%d -> v%d).
  To not update version, do not save this notebook."
               orig_nbformat nbformat)))
     ((ein:notebook--different-number nbformat_minor orig_nbformat_minor)
      (ein:display-warning
       (format "This notebook is version v%s.%s, but IPython
  server you are using only fully support up to v%s.%s.
  Some features may not be available."
               orig_nbformat orig_nbformat_minor
               nbformat nbformat_minor))))))


;;; Initialization.

(defun ein:notebook-bind-events (notebook events)
  "Bind events related to PAGER to the event handler EVENTS."
  (setf (ein:$notebook-events notebook) events)
  ;; As IPython support only supports whole-notebook saving, there is
  ;; no need for finer-level `set_dirty.Notebook'.  Keep this until
  ;; IPython supports finer-level saving.
  (ein:events-on events
                 'set_dirty.Notebook
                 (lambda (notebook data)
                   (setf (ein:$notebook-dirty notebook)
                         (plist-get data :value)))
                 notebook)
  ;; As calling multiple callbacks for this event does not make sense,
  ;; I amadding this in notebook instead of worksheet.
  (ein:events-on events
                 'maybe_reset_undo.Notebook
                 (lambda (-ignore- cell)
                   (ein:with-live-buffer (ein:cell-buffer cell)
                     (ein:notebook-empty-undo-maybe))))
  ;; Bind events for sub components:
  (setf (ein:$notebook-pager notebook)
        (ein:pager-new
         (format ein:notebook-pager-buffer-name-template
                 (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-name notebook))
         (ein:$notebook-events notebook))))

(define-obsolete-function-alias
  'ein:notebook-show-in-shared-output
  'ein:shared-output-show-code-cell-at-point "0.1.2")


;;; Kernel related things

(defun ein:notebook-start-kernel (notebook)
  (let* ((base-url (concat ein:base-kernel-url "kernels"))
         (kernel (ein:kernel-new (ein:$notebook-url-or-port notebook)
                                 base-url
                                 (ein:$notebook-events notebook))))
    (setf (ein:$notebook-kernel notebook) kernel)
    (ein:kernelinfo-init (ein:$kernel-kernelinfo kernel) (current-buffer))
    (ein:kernelinfo-setup-hooks kernel)
    (ein:pytools-setup-hooks kernel)
    (ein:kernel-start kernel
                      (ein:$notebook-notebook-id notebook))))

(defun ein:notebook-restart-kernel (notebook)
  (ein:kernel-restart (ein:$notebook-kernel notebook)))

(defun ein:notebook-restart-kernel-command ()
  "Send request to the server to restart kernel."
  (interactive)
  (if ein:%notebook%
      (when (y-or-n-p "Really restart kernel? ")
        (ein:notebook-restart-kernel ein:%notebook%))
    (ein:log 'error "Not in notebook buffer!")))

(define-obsolete-function-alias
  'ein:notebook-request-tool-tip-or-help-command
  'ein:pytools-request-tooltip-or-help "0.1.2")

(defun ein:notebook-complete-dot ()
  "Insert dot and request completion."
  (interactive)
  (if (and ein:%notebook% (ein:codecell-p (ein:get-cell-at-point)))
      (ein:completer-dot-complete)
    (insert ".")))

(defun ein:notebook-kernel-interrupt-command ()
  "Interrupt the kernel.
This is equivalent to do ``C-c`` in the console program."
  (interactive)
  (ein:kernel-interrupt (ein:$notebook-kernel ein:%notebook%)))

(defun ein:notebook-kernel-kill-command ()
  (interactive)
  (when (y-or-n-p "Really kill kernel?")
    (ein:kernel-kill (ein:$notebook-kernel ein:%notebook%))))

;; autoexec

(defun ein:notebook-execute-autoexec-cells (notebook)
  "Execute cells of which auto-execution flag is on."
  (interactive (list (or ein:%notebook% (error "Not in notebook buffer!"))))
  (mapc #'ein:worksheet-execute-autoexec-cells
        (ein:$notebook-worksheets notebook)))

(define-obsolete-function-alias
  'ein:notebook-eval-string
  'ein:shared-output-eval-string "0.1.2")

;; Followings are kernel related, but EIN specific

(defun ein:notebook-sync-directory (notebook)
  (ein:kernel-sync-directory (ein:$notebook-kernel notebook)
                             (ein:notebook-buffer notebook)))

(defun ein:notebook-sync-directory-command ()
  (interactive)
  (when ein:%notebook% (ein:notebook-sync-directory ein:%notebook%)))


;;; Persistance and loading

(defun ein:notebook-set-notebook-name (notebook name)
  "Check NAME and change the name of NOTEBOOK to it."
  (if (ein:notebook-test-notebook-name name)
      (setf (ein:$notebook-notebook-name notebook) name)
    (ein:log 'error "%S is not a good notebook name." name)
    (error "%S is not a good notebook name." name)))

(defun ein:notebook-test-notebook-name (name)
  (and (stringp name)
       (> (length name) 0)
       (not (string-match "[\\/\\\\]" name))))

(defun ein:notebook-from-json (notebook data)
  (destructuring-bind (&key metadata nbformat nbformat_minor
                            &allow-other-keys)
      data
    (setf (ein:$notebook-metadata notebook) metadata)
    (setf (ein:$notebook-nbformat notebook) nbformat)
    (setf (ein:$notebook-nbformat-minor notebook) nbformat_minor)
    (setf (ein:$notebook-notebook-name notebook) (plist-get metadata :name)))
  (setf (ein:$notebook-worksheets notebook)
        (mapcar (lambda (ws-data)
                  (ein:worksheet-from-json (ein:worksheet-new
                                            notebook
                                            (ein:$notebook-kernel notebook)
                                            (ein:$notebook-events notebook))
                                           ws-data))
                (or (plist-get data :worksheets)
                    (list :cells nil))))
  (ein:worksheet-render (nth 0 (ein:$notebook-worksheets notebook))))

(defun ein:notebook-to-json (notebook)
  "Return json-ready alist."
  (let ((worksheets (mapcar #'ein:worksheet-to-json
                            (ein:$notebook-worksheets notebook))))
    `((worksheets . ,(apply #'vector worksheets))
      (metadata . ,(ein:$notebook-metadata notebook)))))

(defun ein:notebook-save-notebook (notebook retry &optional callback cbarg)
  (let ((data (ein:notebook-to-json notebook)))
    (plist-put (cdr (assq 'metadata data))
               :name (ein:$notebook-notebook-name notebook))
    (push `(nbformat . ,(ein:$notebook-nbformat notebook)) data)
    (ein:aif (ein:$notebook-nbformat-minor notebook)
        ;; Do not set nbformat when it is not given from server.
        (push `(nbformat_minor . ,it) data))
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (ein:query-singleton-ajax
     (list 'notebook-save
           (ein:$notebook-url-or-port notebook)
           (ein:$notebook-notebook-id notebook))
     (ein:notebook-url notebook)
     :timeout ein:notebook-querty-timeout-save
     :type "PUT"
     :headers '(("Content-Type" . "application/json"))
     :cache nil
     :data (json-encode data)
     :error (cons #'ein:notebook-save-notebook-error notebook)
     :success (cons #'ein:notebook-save-notebook-workaround
                    (list notebook retry callback cbarg))
     :status-code
     `((204 . ,(cons (lambda (arg &rest rest)
                       (destructuring-bind (notebook callback cbarg)
                           arg
                         (apply #'ein:notebook-save-notebook-success
                                notebook rest)
                         (when callback
                           (apply callback cbarg rest))))
                     (list notebook callback cbarg)))))))

(defun ein:notebook-save-notebook-command ()
  "Save the notebook."
  (interactive)
  (ein:notebook-save-notebook ein:%notebook% 0))

(defun* ein:notebook-save-notebook-workaround (packed &rest args
                                                      &key
                                                      status
                                                      response-status
                                                      &allow-other-keys)
  ;; IPython server returns 204 only when the notebook URL is
  ;; accessed via PUT or DELETE.  As it seems Emacs failed to
  ;; choose PUT method every two times, let's check the response
  ;; here and fail when 204 is not returned.
  (unless (eq response-status 204)
    (destructuring-bind (notebook retry callback cbarg)
        packed
      (with-current-buffer (ein:notebook-buffer notebook)
        (if (< retry ein:notebook-save-retry-max)
            (progn
              (ein:log 'info "Retry saving... Next count: %s" (1+ retry))
              (ein:notebook-save-notebook notebook (1+ retry)
                                          callback cbarg))
          (ein:notebook-save-notebook-error notebook :status status)
          (ein:log 'info
            "Status code (=%s) is not 204 and retry exceeds limit (=%s)."
            response-status ein:notebook-save-retry-max))))))

(defun ein:notebook-save-notebook-success (notebook &rest ignore)
  (ein:log 'info "Notebook is saved.")
  (setf (ein:$notebook-dirty notebook) nil)
  (with-current-buffer (ein:notebook-buffer notebook)
    (set-buffer-modified-p nil))
  (ein:events-trigger (ein:$notebook-events notebook)
                      'notebook_saved.Notebook))

(defun* ein:notebook-save-notebook-error (notebook &key symbol-status
                                                   &allow-other-keys)
  (if (eq symbol-status 'user-cancel)
      (ein:log 'info "Cancel saving notebook.")
    (ein:log 'info "Failed to save notebook!")
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_save_failed.Notebook)))

(defun ein:notebook-rename-command (name)
  "Rename current notebook and save it immediately.

NAME is any non-empty string that does not contain '/' or '\\'."
  (interactive
   (list (read-string "Rename notebook: "
                      (let ((name (ein:$notebook-notebook-name ein:%notebook%)))
                        (unless (string-match "Untitled[0-9]+" name)
                          name)))))
  (ein:notebook-set-notebook-name ein:%notebook% name)
  (rename-buffer (ein:notebook-get-buffer-name ein:%notebook%))
  (ein:notebook-save-notebook
   ein:%notebook% 0
   (lambda (notebook &rest ignore)
     (with-current-buffer (ein:notebook-buffer notebook)
       (run-hooks 'ein:notebook-after-rename-hook)))
   ein:%notebook%))

(defun ein:notebook-rename-to-scratch-command (name)
  "Rename notebook based on `ein:scratch-notebook-name-template'
and save it immediately."
  (interactive
   (list (read-string "Rename notebook: "
                      (ein:scratch-notebook-name))))
  (ein:notebook-rename-command name))

(defun ein:notebook-close (notebook)
  "Close NOTEBOOK and kill its buffer."
  (let ((ein:notebook-kill-buffer-ask nil))
    ;; Let `ein:notebook-kill-buffer-callback' do its job.
    (kill-buffer (ein:notebook-buffer notebook))))

(defun ein:notebook-kill-kernel-then-close-command ()
  "Kill kernel and then kill notebook buffer.
To close notebook without killing kernel, just close the buffer
as usual."
  (interactive)
  (when (ein:notebook-ask-before-kill-buffer)
    (let ((kernel (ein:$notebook-kernel ein:%notebook%)))
      ;; If kernel is live, kill it before closing.
      (if (ein:kernel-live-p kernel)
          (ein:kernel-kill kernel #'ein:notebook-close (list ein:%notebook%))
        (ein:notebook-close ein:%notebook%)))))


;;; Opened notebooks

(defvar ein:notebook--opened-map (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT NOTEBOOK-ID) => notebook instance.")

(defun ein:notebook-get-opened-notebook (url-or-port notebook-id)
  (gethash (list url-or-port notebook-id) ein:notebook--opened-map))

(defun ein:notebook-get-opened-buffer (url-or-port notebook-id)
  (ein:aand (ein:notebook-get-opened-notebook url-or-port notebook-id)
            (ein:notebook-buffer it)))

(defun ein:notebook-put-opened-notebook (notebook)
  (puthash (list (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-id notebook))
           notebook
           ein:notebook--opened-map))

(defun ein:notebook-opened-notebooks ()
  "Return list of opened notebook instances."
  (let (notebooks)
    (maphash (lambda (k n) (if (ein:notebook-live-p n)
                               (push n notebooks)
                             (remhash k ein:notebook--opened-map)))
             ein:notebook--opened-map)
    notebooks))

(defun ein:notebook-opened-buffers ()
  "Return list of opened notebook buffers."
  (mapcar #'ein:notebook-buffer (ein:notebook-opened-notebooks)))

(defun ein:notebook-opened-buffer-names ()
  "Return list of opened notebook buffer names."
  (mapcar #'buffer-name (ein:notebook-opened-buffers)))


;;; Generic getter

(defun ein:get-url-or-port--notebook ()
  (when ein:%notebook% (ein:$notebook-url-or-port ein:%notebook%)))

(defun ein:get-notebook--notebook ()
  ein:%notebook%)

(defun ein:get-kernel--notebook ()
  (when (ein:$notebook-p ein:%notebook%)
    (ein:$notebook-kernel ein:%notebook%)))

(defalias 'ein:get-cell-at-point--notebook 'ein:notebook-get-current-cell)

(defun ein:get-traceback-data--notebook ()
  (ein:aand (ein:notebook-get-current-cell) (ein:cell-get-tb-data it)))


;;; Predicate

(defun ein:notebook-buffer-p ()
  "Return non-`nil' if current buffer is notebook buffer."
  ein:%notebook%)

(defun ein:notebook-live-p (notebook)
  "Return non-`nil' if NOTEBOOK has live buffer."
  (buffer-live-p (ein:notebook-buffer notebook)))

(defun ein:notebook-modified-p (&optional notebook)
  (unless notebook (setq notebook ein:%notebook%))
  (and (ein:$notebook-p notebook)
       (ein:notebook-live-p notebook)
       (or (ein:$notebook-dirty notebook)
           (loop for ws in (ein:$notebook-worksheets notebook)
                 when (ein:worksheet-modified-p ws)
                 return t))))


;;; Notebook mode

(defcustom ein:notebook-modes
  '(ein:notebook-mumamo-mode ein:notebook-python-mode ein:notebook-plain-mode)
  "Notebook modes to use \(in order of preference).

When the notebook is opened, mode in this value is checked one by one
and the first usable mode is used.  By default, MuMaMo is used when
it is installed.  If not, a simple mode derived from `python-mode' is
used.

Examples:

Avoid using MuMaMo even when it is installed::

  (setq ein:notebook-modes (delq 'ein:notebook-mumamo-mode ein:notebook-modes))

Do not use `python-mode'.  Use plain mode when MuMaMo is not installed::

  (setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-plain-mode))
"
  :type '(repeat (choice (const :tag "MuMaMo" ein:notebook-mumamo-mode)
                         (const :tag "Only Python" ein:notebook-python-mode)
                         (const :tag "Plain" ein:notebook-plain-mode)))
  :group 'ein)

(defun ein:notebook-choose-mode ()
  "Return usable (defined) notebook mode."
  ;; So try to load extra modules here.
  (when (require 'mumamo nil t)
    (require 'ein-mumamo))
  ;; Return first matched mode
  (loop for mode in ein:notebook-modes
        if (functionp mode)
        return mode))

(defvar ein:notebook-mode-map (make-sparse-keymap))

(let ((map ein:notebook-mode-map))
  (define-key map "\C-c\C-c" 'ein:notebook-execute-current-cell)
  (define-key map (kbd "M-RET")
    'ein:notebook-execute-current-cell-and-goto-next)
  (define-key map (kbd "C-c C-'") 'ein:notebook-turn-on-autoexec)
  (define-key map "\C-c\C-e" 'ein:notebook-toggle-output-command)
  (define-key map "\C-c\C-v" 'ein:notebook-set-collapsed-all-command)
  (define-key map "\C-c\C-l" 'ein:notebook-clear-output-command)
  (define-key map (kbd "C-c C-S-l") 'ein:notebook-clear-all-output-command)
  (define-key map (kbd "C-c C-;") 'ein:shared-output-show-code-cell-at-point)
  (define-key map "\C-c\C-k" 'ein:notebook-kill-cell-command)
  (define-key map "\C-c\M-w" 'ein:notebook-copy-cell-command)
  (define-key map "\C-c\C-w" 'ein:notebook-copy-cell-command)
  (define-key map "\C-c\C-y" 'ein:notebook-yank-cell-command)
  (define-key map "\C-c\C-a" 'ein:notebook-insert-cell-above-command)
  (define-key map "\C-c\C-b" 'ein:notebook-insert-cell-below-command)
  (define-key map "\C-c\C-t" 'ein:notebook-toggle-cell-type)
  (define-key map "\C-c\C-u" 'ein:notebook-change-cell-type)
  (define-key map "\C-c\C-s" 'ein:notebook-split-cell-at-point)
  (define-key map "\C-c\C-m" 'ein:notebook-merge-cell-command)
  (define-key map "\C-c\C-n" 'ein:notebook-goto-next-input-command)
  (define-key map "\C-c\C-p" 'ein:notebook-goto-prev-input-command)
  (define-key map (kbd "C-<up>") 'ein:notebook-goto-prev-input-command)
  (define-key map (kbd "C-<down>") 'ein:notebook-goto-next-input-command)
  (define-key map (kbd "C-c <up>") 'ein:notebook-move-cell-up-command)
  (define-key map (kbd "C-c <down>") 'ein:notebook-move-cell-down-command)
  (define-key map (kbd "M-<up>") 'ein:notebook-move-cell-up-command)
  (define-key map (kbd "M-<down>") 'ein:notebook-move-cell-down-command)
  (define-key map "\C-c\C-f" 'ein:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ein:completer-complete)
  (define-key map "\C-c\C-x" 'ein:tb-show)
  (define-key map "\C-c\C-r" 'ein:notebook-restart-kernel-command)
  (define-key map "\C-c\C-z" 'ein:notebook-kernel-interrupt-command)
  (define-key map "\C-c\C-q" 'ein:notebook-kill-kernel-then-close-command)
  (define-key map (kbd "C-:") 'ein:shared-output-eval-string)
  (define-key map "\C-c\C-o" 'ein:console-open)
  (define-key map "\C-x\C-s" 'ein:notebook-save-notebook-command)
  (define-key map "\C-x\C-w" 'ein:notebook-rename-command)
  (define-key map "\M-."          'ein:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein:pytools-jump-back-command)
  map)

(defun ein:notebook-mode ()
  (funcall (ein:notebook-choose-mode))
  (ein:complete-on-dot-install
   ein:notebook-mode-map 'ein:notebook-complete-dot))

(define-derived-mode ein:notebook-plain-mode fundamental-mode "ein:notebook"
  "IPython notebook mode without fancy coloring."
  (font-lock-mode))

(add-hook 'ein:notebook-plain-mode-hook 'ein:notebook-imenu-setup)

(define-derived-mode ein:notebook-python-mode python-mode "ein:python"
  "Use `python-mode' for whole notebook buffer.")

(add-hook 'ein:notebook-python-mode-hook 'ein:notebook-imenu-setup)

(set-keymap-parent ein:notebook-plain-mode-map ein:notebook-mode-map)
(set-keymap-parent ein:notebook-python-mode-map ein:notebook-mode-map)

(defun ein:notebook-open-in-browser (&optional print)
  "Open current notebook in web browser.
When the prefix argument (``C-u``) is given, print page is opened.
Note that print page is not supported in IPython 0.12.1."
  (interactive "P")
  (let ((url (apply #'ein:url
                    (ein:$notebook-url-or-port ein:%notebook%)
                    (ein:$notebook-notebook-id ein:%notebook%)
                    (if print (list "print")))))
    (message "Opening %s in browser" url)
    (browse-url url)))


;;; Buffer and kill hooks

(defcustom ein:notebook-kill-buffer-ask t
  "Whether EIN should ask before killing unsaved notebook buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

;; -- `kill-buffer-query-functions'
(defun ein:notebook-ask-before-kill-buffer ()
  "Return `nil' to prevent killing the notebook buffer.
Called via `kill-buffer-query-functions'."
  (not (and ein:notebook-kill-buffer-ask
            (ein:notebook-modified-p)
            (not (y-or-n-p "You have unsaved changes. Discard changes?")))))

(add-hook 'kill-buffer-query-functions 'ein:notebook-ask-before-kill-buffer)

;; -- `kill-emacs-query-functions'
(defun ein:notebook-ask-before-kill-emacs ()
  "Return `nil' to prevent killing Emacs when unsaved notebook exists.
Called via `kill-emacs-query-functions'."
  (let ((unsaved (ein:filter #'ein:notebook-modified-p
                             (ein:notebook-opened-notebooks))))
    (if (null unsaved)
        t
      (let ((answer
             (y-or-n-p
              (format "You have %s unsaved notebook(s). Discard changes?"
                      (length unsaved)))))
        ;; kill all unsaved buffers forcefully
        (when answer
          (mapc #'ein:notebook-close unsaved))
        answer))))

(add-hook 'kill-emacs-query-functions 'ein:notebook-ask-before-kill-emacs)

;; -- `kill-buffer-hook'
(defun ein:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  (when (ein:$notebook-p ein:%notebook%)
    (ein:notebook-del ein:%notebook%)))

(defun ein:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ein:notebook-kill-buffer-callback))

(add-hook 'ein:notebook-plain-mode-hook 'ein:notebook-setup-kill-buffer-hook)

;; Useful command to close notebooks.
(defun ein:notebook-kill-all-buffers ()
  "Close all opened notebooks."
  (interactive)
  (let* ((notebooks (ein:notebook-opened-notebooks))
         (unsaved (ein:filter #'ein:notebook-modified-p notebooks)))
    (if notebooks
        (if (y-or-n-p
             (format (concat "You have %s opened notebook(s). "
                             (when unsaved
                               (format "%s are UNSAVED. " (length unsaved)))
                             "Really kill all of them?")
                     (length notebooks)))
            (progn (ein:log 'info "Killing all notebook buffers...")
                   (mapc #'ein:notebook-close notebooks)
                   (ein:log 'info "Killing all notebook buffers... Done!"))
          (ein:log 'info "Canceled to kill all notebooks."))
      (ein:log 'info "No opened notebooks."))))

(provide 'ein-notebook)

;;; ein-notebook.el ends here
