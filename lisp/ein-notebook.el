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

(require 'ein-core)
(require 'ein-log)
(require 'ein-node)
(require 'ein-contents-api)
(require 'ein-kernel)
(require 'ein-kernelinfo)
(require 'ein-cell)
(require 'ein-cell-edit)
(require 'ein-cell-output)
(require 'ein-worksheet)
(require 'ein-scratchsheet)
(require 'ein-notification)
(require 'ein-completer)
(require 'ein-pager)
(require 'ein-events)
(require 'ein-notification)
(require 'ein-kill-ring)
(require 'ein-query)
(require 'ein-pytools)
(require 'ein-traceback)
(require 'ein-inspector)

;;; Configuration

(make-obsolete-variable 'ein:notebook-discard-output-on-save nil "0.2.0")

(defcustom ein:notebook-autosave-frequency 60
  "Sets the frequency (in seconds) at which the notebook is
automatically saved.

Autosaves are automatically enabled when a notebook is opened,
but can be controlled manually via `ein:notebook-enable-autosave'
and `ein:notebook-disable-autosave'.

If this parameter is changed than you must call
`ein:notebook-disable-autosave' and then
`ein:notebook-enable-autosave' on all open notebooks for the
changes to take effect.

"
  :type 'number
  :group 'ein)

(defcustom ein:notebook-create-checkpoint-on-save t
  "If non-nil a checkpoint will be created every time the
notebook is saved. Otherwise checkpoints must be created manually
via `ein:notebook-create-checkpoint'."
  :type 'boolean
  :group 'ein)

(defcustom ein:notebook-discard-output-on-save 'no
  "Configure if the output part of the cell should be saved or not.

.. warning:: This configuration is obsolete now.
   Use nbconvert (https://github.com/ipython/nbconvert) to
   strip output.

`no' : symbol
    Save output. This is the default.
`yes' : symbol
    Always discard output.
a function
    This function takes two arguments, notebook and cell.  Return
    `t' to discard output and return `nil' to save.  For example,
    if you don't want to save image output but other kind of
    output, use `ein:notebook-cell-has-image-output-p'.
"
  :type '(choice (const :tag "No" 'no)
                 (const :tag "Yes" 'yes)
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

(defcustom ein:notebook-querty-timeout-save (* 60 1000) ; 1 min
  "Query timeout for saving notebook.
Similar to `ein:notebook-querty-timeout-open', but for saving
notebook.  For global setting and more information, see
`ein:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defcustom ein:helm-kernel-history-search-key nil
  "Bind `helm-ein-kernel-history' to this key in notebook mode.

Example::

    (setq ein:helm-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ein:notebook-mode-map'."
  :type 'boolean
  :group 'ein)

(defcustom ein:anything-kernel-history-search-key nil
  "Bind `anything-ein-kernel-history' to this key in notebook mode.

Example::

    (setq ein:anything-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ein:notebook-mode-map'."
  :type 'boolean
  :group 'ein)

(defcustom ein:notebook-set-buffer-file-name nil
  "[DEPRECATED] Set `buffer-file-name' of notebook buffer. Currently does nothing."
  :type 'boolean
  :group 'ein)

(defvar ein:notebook-after-rename-hook nil
  "Hooks to run after notebook is renamed successfully.
Current buffer for these functions is set to the notebook buffer.")


;;; Class and variable

(defvar ein:base-kernel-url "/api/")
(defvar ein:create-session-url "/api/sessions")
;; Currently there is no way to know this setting.  Maybe I should ask IPython
;; developers for an API to get this from notebook server.
;;
;; 10April2014 (JMM) - The most recent documentation for the RESTful interface
;; is at:
;; https://github.com/ipython/ipython/wiki/IPEP-16%3A-Notebook-multi-directory-dashboard-and-URL-mapping


(defvar ein:notebook-pager-buffer-name-template "*ein:pager %s/%s*")
(defvar ein:notebook-buffer-name-template "*ein: %s/%s*")

(defvar ein:notebook-save-retry-max 1
  "Maximum retries for notebook saving.")

(defstruct ein:$notebook
  "Hold notebook variables.

`ein:$notebook-url-or-port'
  URL or port of IPython server.

`ein:$notebook-notebook-id' : string
  uuid string (as of ipython 2.0 this is the same is notebook-name).

`ein:$notebook-notebook-path' : string
  Path to notebook.

`ein:$notebook-kernel' : `ein:$kernel'
  `ein:$kernel' instance.

`ein:$notebook-kernelspec' : `ein:$kernelspec'
  Jupyter kernel specification for the notebook.

`ein:$notebook-kernelinfo' : `ein:kernelinfo'
  `ein:kernelinfo' instance.

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

`ein:$notebook-scratchsheets' : list of `ein:worksheet'
  List of scratch worksheets.

`ein:$notebook-api-version' : integer
   Major version of the IPython notebook server we are talking to.

`ein:$notebook-checkpoints'
  Names auto-saved checkpoints for content. Stored as a list
  of (<id> . <last_modified>) pairs.
"
  url-or-port
  notebook-id ;; In IPython-2.0 this is "[:path]/[:name].ipynb"
  notebook-path
  kernel
  kernelinfo
  kernelspec
  pager
  dirty
  metadata
  notebook-name
  nbformat
  nbformat-minor
  events
  worksheets
  scratchsheets
  api-version
  autosave-timer
  checkpoints)

(ein:deflocal ein:%notebook% nil
  "Buffer local variable to store an instance of `ein:$notebook'.")
(define-obsolete-variable-alias 'ein:notebook 'ein:%notebook% "0.1.2")



;;; Constructor

(defun ein:notebook-new (url-or-port notebook-path kernelspec &rest args)
  (if (or (stringp kernelspec) (symbolp kernelspec))
      (setq kernelspec (ein:get-kernelspec url-or-port kernelspec)))
  (let ((notebook (apply #'make-ein:$notebook
                         :url-or-port url-or-port
                         :kernelspec kernelspec
                         :notebook-path notebook-path
                         args)))
    notebook))


;;; Destructor

(defun ein:notebook-del (notebook)
  "Destructor for `ein:$notebook'."
  (ein:log-ignore-errors
    (ein:kernel-del (ein:$notebook-kernel notebook))))

(defun ein:notebook-close-worksheet (notebook ws)
  "Close worksheet WS in NOTEBOOK.  If WS is the last worksheet,
call notebook destructor `ein:notebook-del'."
  (symbol-macrolet ((worksheets (ein:$notebook-worksheets notebook))
                    (scratchsheets (ein:$notebook-scratchsheets notebook)))
    (cond
     ((ein:worksheet-p ws) (ein:worksheet-save-cells ws t))
     (t (setq scratchsheets (delq ws scratchsheets))))
    (unless (or (ein:filter (lambda (x)
                              (and (not (eq x ws))
                                   (ein:worksheet-has-buffer-p x)))
                            worksheets)
                scratchsheets)
      (ein:notebook-del notebook))))


;;; Notebook utility functions

(defun ein:notebook-update-url-or-port (new-url-or-port notebook)
  "Change the url and port the notebook is saved to. Calling
this will propagate the change to the kernel, trying to restart
the kernel in the process. Use case for this command is when
the jupyter server dies and restarted on a different port.

If you have enabled token or password security on server running
at the new url/port, then please be aware that this new url-port
combo must match exactly these url/port you used format
`ein:notebooklist-login'. For example, as far as Emacs and
jupyter are concerned, 'localhost:8888' and '127.0.0.1:8888' are
*not* the same URL."
  (interactive (list
                (ein:notebooklist-ask-url-or-port)
                (ein:get-notebook-or-error)))
  (message "Updating server info and restarting kernel for notebooklist %s"
           (ein:$notebook-notebook-name notebook))
  (setf (ein:$notebook-url-or-port notebook) new-url-or-port
        (ein:$kernel-url-or-port (ein:$notebook-kernel notebook)) new-url-or-port)
  (with-current-buffer (ein:notebook-buffer notebook)
    (ein:kernel-restart (ein:$notebook-kernel notebook))
    (rename-buffer (format ein:notebook-buffer-name-template
                           (ein:$notebook-url-or-port notebook)
                           (ein:$notebook-notebook-name notebook)))))

(defun ein:notebook-buffer (notebook)
  "Return the buffer that is associated with NOTEBOOK."
  ;; FIXME: Find a better way to define notebook buffer!
  ;;        For example, the last accessed buffer.
  (let ((first-buffer
         (lambda (ws-list)
           (loop for ws in ws-list if (ein:worksheet-buffer ws) return it))))
    (or (funcall first-buffer (ein:$notebook-worksheets    notebook))
        (funcall first-buffer (ein:$notebook-scratchsheets notebook)))))

(defun ein:notebook-buffer-list (notebook)
  "Return the buffers associated with NOTEBOOK's kernel.
The buffer local variable `default-directory' of these buffers
will be updated with kernel's cwd."
  (ein:filter #'identity
              (mapcar #'ein:worksheet-buffer
                      (append (ein:$notebook-worksheets notebook)
                              (ein:$notebook-scratchsheets notebook)))))

(defun ein:notebook--get-nb-or-error ()
  (or ein:%notebook% (error "Not in notebook buffer.")))

;;;###autoload
(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

(defun ein:notebook-name-getter (notebook)
  (cons #'ein:notebook-name notebook))


;;; Open notebook

(defun ein:notebook-url (notebook)
  (ein:notebook-url-from-url-and-id (ein:$notebook-url-or-port notebook)
                                    (ein:$notebook-api-version notebook)
                                    (ein:$notebook-notebook-path notebook)))

(defun ein:notebook-url-from-url-and-id (url-or-port api-version path)
  (cond ((= api-version 2)
         (ein:url url-or-port "api/notebooks" path))
        ((>= api-version 3)
         (ein:url url-or-port "api/contents" path))))

(defun ein:notebook-pop-to-current-buffer (&rest -ignore-)
  "Default callback for `ein:notebook-open'."
  (pop-to-buffer (current-buffer)))

;;; TODO - I think notebook-path is unnecessary (JMM).

(defun ein:notebook-open (url-or-port path &optional kernelspec callback cbargs)
  "Open notebook at PATH in the server URL-OR-PORT.
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
  (let ((buffer (ein:notebook-get-opened-buffer url-or-port path)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (ein:log 'info "Notebook %s is already opened."
                   (ein:$notebook-notebook-name ein:%notebook%))
          (when callback
            (apply callback ein:%notebook% nil cbargs))
          ein:%notebook%)
      (ein:log 'info "Opening notebook %s..." path)
      (ein:notebook-request-open url-or-port path kernelspec callback cbargs))))

(defun ein:notebook-request-open (url-or-port path &optional kernelspec callback cbargs)
  "Request notebook at PATH from the server at URL-OR-PORT.
Return an `ein:$notebook' instance.  Notebook kernel may not be ready to
receive messages after this call is executed.

CALLBACK is called as \(apply CALLBACK notebook t CBARGS).  The second
argument `t' indicates that the notebook is newly opened.
See `ein:notebook-open' for more information."
  (let ((notebook (ein:notebook-new url-or-port path kernelspec)))
    (ein:log 'debug "Opening notebook at %s" path)
    (ein:content-query-contents path url-or-port nil
                                (apply-partially #'ein:notebook-request-open-callback-with-callback
                                                 notebook callback cbargs))
    ;; (ein:query-singleton-ajax
    ;;  (list 'notebook-open url-or-port api-version path)
    ;;  url
    ;;  :timeout ein:notebook-querty-timeout-open
    ;;  :parser #'ein:json-read
    ;;  :success (apply-partially
    ;;            #'ein:notebook-request-open-callback-with-callback
    ;;            notebook callback cbargs))
    notebook))


(defun ein:notebook-request-open-callback-with-callback (notebook
                                                         callback
                                                         cbargs
                                                         content)
  (funcall #'ein:notebook-request-open-callback notebook content)
  (when callback
    (with-current-buffer (ein:notebook-buffer notebook)
      (apply callback notebook t cbargs))))

(defun ein:notebook-maybe-set-kernelspec (notebook content-metadata)
  (ein:aif (plist-get content-metadata :kernelspec)
      (let ((kernelspec (ein:get-kernelspec (ein:$notebook-url-or-port notebook)
                                            (plist-get it :name))))
        (setf (ein:$notebook-kernelspec notebook) kernelspec))))


(defun ein:notebook-request-open-callback (notebook content)
  (let ((notebook-path (ein:$notebook-notebook-path notebook)))
    (setf (ein:$notebook-api-version notebook) (ein:$content-ipython-version content)
          (ein:$notebook-notebook-name notebook) (ein:$content-name content))
    (ein:notebook-bind-events notebook (ein:events-new))
    (ein:notebook-maybe-set-kernelspec notebook (plist-get (ein:$content-raw-content content) :metadata))
    (ein:notebook-start-kernel notebook)
    (ein:notebook-from-json notebook (ein:$content-raw-content content)) ; notebook buffer is created here
    (setf (ein:$notebook-kernelinfo notebook)
          (ein:kernelinfo-new (ein:$notebook-kernel notebook)
                              (cons #'ein:notebook-buffer-list notebook)))
    (ein:notebook-put-opened-notebook notebook)
    (ein:notebook--check-nbformat (ein:$content-raw-content content))
    (ein:notebook-enable-autosaves notebook)
    (ein:log 'info "Notebook %s is ready"
             (ein:$notebook-notebook-name notebook))))


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

(defun ein:notebook-enable-autosaves (notebook)
  "Enable automatic, periodic saving for notebook."
  (interactive
   (let* ((notebook (or (ein:get-notebook)
                        (completing-read
                         "Select notebook [URL-OR-PORT/NAME]: "
                         (ein:notebook-opened-buffer-names)))))
     (list notebook)))
  (if (stringp notebook)
      (error "Fix me!")) ;; FIXME
  (setf (ein:$notebook-autosave-timer notebook)
        (run-at-time 0 ein:notebook-autosave-frequency #'ein:notebook-maybe-save-notebook notebook 0))
  (ein:log 'info "Enabling autosaves for %s with frequency %s seconds."
           (ein:$notebook-notebook-name notebook)
           ein:notebook-autosave-frequency))

(defun ein:notebook-disable-autosaves (notebook)
  "Disable automatic, periodic saving for current notebook."
  (interactive
   (let* ((notebook (or (ein:get-notebook)
                        (completing-read
                         "Select notebook [URL-OR-PORT/NAME]: "
                         (ein:notebook-opened-buffer-names)))))
     (list notebook)))
  (ein:log 'info "Disabling auto checkpoints for notebook %s" (ein:$notebook-notebook-name notebook))
  (when (ein:$notebook-autosave-timer notebook)
    (cancel-timer (ein:$notebook-autosave-timer notebook))))

(defun ein:notebook-bind-events (notebook events)
  "Bind events related to PAGER to the event handler EVENTS."
  (setf (ein:$notebook-events notebook) events)
  (ein:worksheet-class-bind-events events)
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

(defstruct ein:$kernelspec
  "Kernel specification as return by the Jupyter notebook server.

`ein:$kernelspec-name' : string
  Name used to identify the kernel (like python2, or python3).

`ein:$kernelspec-display-name' : string
  Name used to display kernel to user.

`ein:$kernelspec-language' : string
  Programming language supported by kernel, like 'python'.

`ein:$kernelspec-resources' : plist
  Resources, if any, used by the kernel.

`ein:$kernelspec-spec' : plist
  How to start the kernel from the command line. Not used by ein (yet).
"
  name
  display-name
  resources
  spec
  language)

(defvar ein:available-kernelspecs (make-hash-table :test #'equal))

(defun ein:kernelspec-for-nb-metadata (kernelspec)
  (let ((display-name (plist-get (ein:$kernelspec-spec kernelspec) :display_name)))
    `((:name . ,(ein:$kernelspec-name kernelspec))
      (:display_name . ,(format "%s" display-name)))))

(defun ein:get-kernelspec (url-or-port name)
  (let ((kernelspecs (gethash url-or-port ein:available-kernelspecs))
        (name (if (stringp name)
                  (intern (format ":%s" name))
                name)))
    (plist-get kernelspecs name)))

(defun ein:get-kernelspec-language (kernelspec)
  (plist-get (ein:$kernelspec-spec kernelspec) :language))

(defun ein:list-available-kernels (url-or-port)
  (let ((kernelspecs (gethash url-or-port ein:available-kernelspecs)))
    (if kernelspecs
        (loop for (key spec) on (ein:plist-exclude kernelspecs '(:default)) by 'cddr
              collecting (cons (ein:$kernelspec-name spec)
                               (ein:$kernelspec-display-name spec))))))

(defun ein:query-kernelspecs (url-or-port)
  "Query jupyter server for the list of available
kernels. Results are stored in ein:available-kernelspec, hashed
on server url/port."
  (unless (gethash url-or-port ein:available-kernelspecs)
    (ein:query-singleton-ajax
     (list 'ein:qeury-kernelspecs url-or-port)
     (ein:url url-or-port "api/kernelspecs")
     :type "GET"
     :timeout ein:content-query-timeout
     :parser 'ein:json-read
     :sync t
     :success (apply-partially #'ein:query-kernelspecs-success url-or-port)
     :error (apply-partially #'ein:query-kernelspecs-error))))

(defun* ein:query-kernelspecs-success (url-or-port &key data &allow-other-keys)
  (let ((ks (list :default (plist-get data :default)))
        (specs (ein:plist-iter (plist-get data :kernelspecs))))
    (setf (gethash url-or-port ein:available-kernelspecs)
          (ein:flatten (dolist (spec specs ks)
                         (let ((name (car spec))
                               (info (cdr spec)))
                           (push (list name (make-ein:$kernelspec :name (plist-get info :name)
                                                                  :display-name (plist-get (plist-get info :spec)
                                                                                           :display_name)
                                                                  :resources (plist-get info :resources)
                                                                  :language (plist-get (plist-get info :spec)
                                                                                       :language)
                                                                  :spec (plist-get info :spec)))
                                 ks)))))))

(defun* ein:query-kernelspecs-error (&key symbol-status response &allow-other-keys)
  (ein:log 'verbose
    "Error thrown: %S" (request-response-error-thrown response))
  (ein:log 'error
    "Kernelspc query call failed with status %s." symbol-status))

(defun ein:notebook-switch-kernel (notebook kernel-name)
  "Change the kernel for a running notebook. If not called from a
notebook buffer then the user will be prompted to select an opened notebook."
  (interactive
   (let* ((notebook (or (ein:get-notebook)
                        (completing-read
                         "Select notebook [URL-OR-PORT/NAME]: "
                         (ein:notebook-opened-buffer-names))))
          (kernel-name (completing-read
                        "Select kernel: "
                        (ein:list-available-kernels (ein:$notebook-url-or-port notebook)))))
     (list notebook kernel-name)))
  (setf (ein:$notebook-kernelspec notebook) (ein:get-kernelspec (ein:$notebook-url-or-port notebook)
                                                                kernel-name))
  (ein:log 'info "Restarting notebook %s with new kernel %s." (ein:$notebook-notebook-name notebook) kernel-name)
  (ein:notebook-restart-kernel notebook))

;;; This no longer works in iPython-2.0. Protocol is to create a session for a
;;; notebook, which will automatically create and associate a kernel with the notebook.
(defun ein:notebook-start-kernel (notebook)
  (let* ((base-url (concat ein:base-kernel-url "kernels"))
         (kernelspec (ein:$notebook-kernelspec notebook))
         (kernel (ein:kernel-new (ein:$notebook-url-or-port notebook)
                                 base-url
                                 (ein:$notebook-events notebook)
                                 (ein:$notebook-api-version notebook))))
    (setf (ein:$notebook-kernel notebook) kernel)
    (when (and kernelspec (string-equal (ein:$kernelspec-language kernelspec) "python"))
      (ein:pytools-setup-hooks kernel notebook))
    (ein:kernel-start kernel notebook)))

(defun ein:notebook-restart-kernel (notebook)
  (ein:kernel-restart (ein:$notebook-kernel notebook)))

(defun ein:notebook-restart-kernel-command ()
  "Send request to the server to restart kernel."
  (interactive)
  (if ein:%notebook%
      (when (y-or-n-p "Really restart kernel? ")
        (ein:notebook-restart-kernel ein:%notebook%))
    (ein:log 'error "Not in notebook buffer!")))

(defun ein:notebook-reconnect-kernel ()
  "Reconnect to the websocket connection for the running kernel."
  (interactive)
  (if ein:%notebook%
      (ein:kernel-reconnect (ein:$notebook-kernel ein:%notebook%) ein:%notebook%)))

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


;;; Persistence and loading

(defun ein:notebook-set-notebook-name (notebook name)
  "Check NAME and change the name of NOTEBOOK to it."
  (if (ein:notebook-test-notebook-name name)
      (setf (ein:$notebook-notebook-name notebook) name
            (ein:$notebook-notebook-id notebook) name)
    (ein:log 'error "%S is not a good notebook name." name)
    (error "%S is not a good notebook name." name)))

(defun ein:notebook-test-notebook-name (name)
  (and (stringp name)
       (> (length name) 0)
       (not (string-match "[\\/\\\\:]" name))))

(defun* ein:notebook--worksheet-new (notebook
                                     &optional (func #'ein:worksheet-new))
  (funcall func
           (ein:$notebook-nbformat notebook)
           (ein:notebook-name-getter notebook)
           (cons (lambda (notebook cell)
                   (ein:notebook-discard-output-p notebook cell))
                 notebook)
           (ein:$notebook-kernel notebook)
           (ein:$notebook-events notebook)))

(defun ein:notebook--worksheet-render (notebook ws)
  (ein:worksheet-render ws)
  (with-current-buffer (ein:worksheet-buffer ws)
    (ein:notebook-mode)
    ;; Now that major-mode is set, set buffer local variables:
    (ein:notebook--notification-setup notebook)
    (ein:notebook-setup-kill-buffer-hook)
    (setq ein:%notebook% notebook)))

(defun ein:notebook--notification-setup (notebook)
  (ein:notification-setup
   (current-buffer)
   (ein:$notebook-events notebook)
   :get-list
   (lambda () (ein:$notebook-worksheets ein:%notebook%))
   :get-current
   (lambda () ein:%worksheet%)
   :get-name
   #'ein:worksheet-name
   :get-buffer
   (lambda (ws)
     (ein:notebook-worksheet--render-maybe ein:%notebook% ws "clicked")
     (ein:worksheet-buffer ws))
   :delete
   (lambda (ws)
     (ein:notebook-worksheet-delete ein:%notebook% ws t))
   :insert-prev
   (lambda (ws) (ein:notebook-worksheet-insert-prev ein:%notebook% ws))
   :insert-next
   (lambda (ws) (ein:notebook-worksheet-insert-next ein:%notebook% ws))
   :move-prev
   (lambda (ws) (ein:notebook-worksheet-move-prev ein:%notebook% ws))
   :move-next
   (lambda (ws) (ein:notebook-worksheet-move-next ein:%notebook% ws))
   ))

(defun ein:notebook-set-buffer-file-name-maybe (notebook)
  (ein:log 'warn "This function is deprecated. Who could be calling me?"))

;; (defun ein:notebook-set-buffer-file-name-maybe (notebook)
;;   "Set `buffer-file-name' of the current buffer to ipynb file
;; of NOTEBOOK."
;;   (when ein:notebook-set-buffer-file-name
;;     (ein:notebook-fetch-data
;;      notebook
;;      (lambda (data notebook buffer)
;;        (with-current-buffer buffer
;;          (destructuring-bind (&key project &allow-other-keys)
;;              data
;;            (setq buffer-file-name
;;                  (expand-file-name
;;                   (format "%s.ipynb"
;;                           (ein:$notebook-notebook-name notebook))
;;                   project)))))
;;      (list notebook (current-buffer)))))

(defun ein:notebook-from-json (notebook data)
  (destructuring-bind (&key metadata nbformat nbformat_minor
                            &allow-other-keys)
      data
    (setf (ein:$notebook-metadata notebook) metadata)
    (setf (ein:$notebook-nbformat notebook) nbformat)
    (setf (ein:$notebook-nbformat-minor notebook) nbformat_minor)
    ;;(setf (ein:$notebook-notebook-name notebook) (plist-get metadata :name))
    )
  (setf (ein:$notebook-worksheets notebook)
        (cl-case (ein:$notebook-nbformat notebook)
          (3 (ein:read-nbformat3-worksheets notebook data))
          (4 (ein:read-nbformat4-worksheets notebook data))
          (t (ein:log 'error "Do not currently support nbformat version %s" (ein:$notebook-nbformat notebook)))))
  (ein:notebook--worksheet-render notebook
                                  (nth 0 (ein:$notebook-worksheets notebook)))
  notebook)

(defun ein:read-nbformat3-worksheets (notebook data)
  (mapcar (lambda (ws-data)
                    (ein:worksheet-from-json
                     (ein:notebook--worksheet-new notebook)
                     ws-data))
          (or (plist-get data :worksheets)
              (list nil))))

;; nbformat4 gets rid of the concenpt of worksheets. That means, for the moment,
;; ein will no longer support worksheets. There may be a path forward for
;; reimplementing this feature, however.  The nbformat 4 json definition says
;; that cells are allowed to have tags. Clever use of this feature may lead to
;; good things.

(defun ein:read-nbformat4-worksheets (notebook data)
  "Convert a notebook in nbformat4 to a list of worksheet-like
  objects suitable for processing in ein:notebook-from-json."
  (ein:log 'info "Reading nbformat4 notebook.")
  (let* ((cells (plist-get data :cells))
         (ws-cells (mapcar (lambda (data) (ein:cell-from-json data)) cells))
         (worksheet (ein:notebook--worksheet-new notebook)))
    (oset worksheet :saved-cells ws-cells)
    ;(mapcar (lambda (data) (message "test %s" (oref data :metadata))) ws-cells)
    (list worksheet)))

(defun ein:notebook-to-json (notebook)
  "Return json-ready alist."
  (let ((data
         (case (ein:$notebook-nbformat notebook)
           (3 (ein:write-nbformat3-worksheets notebook))
           (4 (ein:write-nbformat4-worksheets notebook))
           (t (ein:log 'error "EIN does not support saving notebook format %s" (ein:$notebook-nbformat notebook))))))
    ;; Apparently metadata can be either a hashtable or a plist...
    (let ((metadata (cdr (assq 'metadata data))))
      (if (hash-table-p metadata)
          (setf (gethash 'name metadata) (ein:$notebook-notebook-name notebook))
        (plist-put metadata
                   :name (ein:$notebook-notebook-name notebook)))
      (ein:aif (ein:$notebook-nbformat-minor notebook)
          ;; Do not set nbformat when it is not given from server.
          (push `(nbformat_minor . ,it) data))
      (push `(nbformat . ,(ein:$notebook-nbformat notebook)) data)
      data)))


(defun ein:write-nbformat3-worksheets (notebook)
  (let ((worksheets (mapcar #'ein:worksheet-to-json
                            (ein:$notebook-worksheets notebook))))
    `((worksheets . ,(apply #'vector worksheets))
      (metadata . ,(ein:$notebook-metadata notebook))
      )))

(defun ein:write-nbformat4-worksheets (notebook)
  (ein:log 'verbose "Writing notebook %s as nbformat 4." (ein:$notebook-notebook-name notebook))
  (let ((all-cells (loop for ws in (ein:$notebook-worksheets notebook)
                         for i from 0
                         append (ein:worksheet-to-nb4-json ws i))))
    (ein:aif (ein:$notebook-kernelspec notebook)
        (if (ein:$notebook-metadata notebook)
            (plist-put (ein:$notebook-metadata notebook)
                       :kernelspec (ein:kernelspec-for-nb-metadata it))
          (setf (ein:$notebook-metadata notebook)
                (list :kernelspec (ein:kernelspec-for-nb-metadata it)))))
    `((metadata . ,(ein:aif (ein:$notebook-metadata notebook)
                       it
                     (make-hash-table)))
      (cells . ,(apply #'vector all-cells)))

    ))

(defun ein:notebook-maybe-save-notebook (notebook retry &optional callback cbargs)
  (if (cl-some #'(lambda (ws)
                   (buffer-modified-p
                    (ein:worksheet-buffer ws)))
               (ein:$notebook-worksheets notebook))
      (ein:notebook-save-notebook notebook retry callback cbargs)))

(defun ein:notebook-save-notebook (notebook retry &optional callback cbargs)
  (condition-case err
      (with-current-buffer (ein:notebook-buffer notebook)
        (run-hooks 'before-save-hook))
    (error (ein:log 'warn "Error running save hooks: '%s'. I will still try to save the notebook." (error-message-string err))))
  (let ((content (ein:content-from-notebook notebook)))
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (ein:content-save content
                      #'ein:notebook-save-notebook-success
                      (list notebook callback cbargs)
                      #'ein:notebook-save-notebook-error
                      (list notebook))))

(defun ein:notebook-save-notebook-command ()
  "Save the notebook."
  (interactive)
  (ein:notebook-save-notebook ein:%notebook% 0))

(defun* ein:notebook-save-notebook-workaround
    (notebook retry callback cbarg
              &key
              status
              response
              &allow-other-keys
              &aux
              (response-status (request-response-status-code response)))
  ;; IPython server returns 204 only when the notebook URL is
  ;; accessed via PUT or DELETE.  As it seems Emacs failed to
  ;; choose PUT method every two times, let's check the response
  ;; here and fail when 204 is not returned.
  ;; UPDATE 12Sep2014 (JMM) - IPython server now returns 200 on successful save.
  (unless (eq response-status 200)
    (with-current-buffer (ein:notebook-buffer notebook)
      (if (< retry ein:notebook-save-retry-max)
          (progn
            (ein:log 'info "Retry saving... Next count: %s" (1+ retry))
            (ein:notebook-save-notebook notebook (1+ retry)
                                        callback cbarg))
        (ein:notebook-save-notebook-error notebook :status status)
        (ein:log 'info
          "Status code (=%s) is not 200 and retry exceeds limit (=%s)."
          response-status ein:notebook-save-retry-max)))))

(defun ein:notebook-save-notebook-success (notebook callback cbargs)
  (ein:log 'verbose "Notebook is saved.")
  (setf (ein:$notebook-dirty notebook) nil)
  (mapc (lambda (ws)
          (ein:worksheet-save-cells ws) ; [#]_
          (ein:worksheet-set-modified-p ws nil))
        (ein:$notebook-worksheets notebook))
  (ein:events-trigger (ein:$notebook-events notebook)
                      'notebook_saved.Notebook)
  (when ein:notebook-create-checkpoint-on-save
    (ein:notebook-create-checkpoint notebook))
  (when callback
    (apply callback cbargs)))

;; .. [#] Consider the following case.
;;    (1) Open worksheet WS0 and other worksheets.
;;    (2) Edit worksheet WS0 then save the notebook.
;;    (3) Edit worksheet WS0.
;;    (4) Kill WS0 buffer by discarding the edit.
;;    (5) Save the notebook.
;;    This should save the latest WS0.  To do so, WS0 at the point (2)
;;    must be cached in the worksheet slot `:saved-cells'.

(defun* ein:notebook-save-notebook-error (notebook &key symbol-status
                                                   &allow-other-keys)
  (if (eq symbol-status 'user-cancel)
      (ein:log 'info "Cancel saving notebook.")
    (ein:log 'info "Failed to save notebook!")
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_save_failed.Notebook)))

(defun ein:notebook-rename-command (path)
  "Rename current notebook and save it immediately.

NAME is any non-empty string that does not contain '/' or '\\'."
  (interactive
   (list (read-string "Rename notebook: " (ein:$notebook-notebook-path ein:%notebook%))))
  (unless (and (string-match ".ipynb" path) (= (match-end 0) (length path)))
    (setq path (format "%s.ipynb" path)))
  (let ((content (ein:content-from-notebook ein:%notebook%))
        (old-name (ein:$notebook-notebook-name ein:%notebook%)))
    (ein:log 'info "Renaming notebook at URL %s" (ein:notebook-url ein:%notebook%))
    (ein:content-rename content path #'ein:notebook-rename-success (list ein:%notebook% content))))

(defun ein:notebook-save-to-command (path)
  "Make a copy of the notebook and save it to a new path specified by NAME.
NAME is any non-empty string that does not contain '/' or '\\'.
"
  (interactive
   (list (read-string "Save copy to: " (ein:$notebook-notebook-path ein:%notebook%))))
  (unless (and (string-match ".ipynb" path) (= (match-end 0) (length path)))
    (setq path (format "%s.ipynb" path)))
  (let* ((content (ein:content-from-notebook ein:%notebook%))
         (name (substring path (or (cl-position ?/ path :from-end t) 0))))
    (setf (ein:$content-path content) path
          (ein:$content-name content) name)
    (ein:content-save content #'ein:notebook-open
                      (list (ein:$notebook-url-or-port ein:%notebook%)
                            path))))

;; (defun* ein:notebook-rename-error (old new notebook &key symbol-status response
;;                                        error-thrown
;;                                        &allow-other-keys)
;;   (if (= (request-response-status-code response) 409)
;;       (progn
;;         (ein:log 'warn "IPython returned a 409 status code, but has still renamed the notebook. This may be an IPython bug.")
;;         (ein:notebook-rename-success notebook new response))
;;     (ein:log 'error
;;       "Error (%s :: %s) while renaming notebook %s to %s."
;;       symbol-status error-thrown old new)))

(defun* ein:notebook-rename-success (notebook content)
  (ein:notebook-set-notebook-name notebook (ein:$content-name content))
  (setf (ein:$notebook-notebook-path notebook) (ein:$content-path content))
  (mapc #'ein:worksheet-set-buffer-name
        (ein:$notebook-worksheets notebook))
  (ein:log 'info "Notebook renamed to %s." (ein:$content-name content)))

(defun ein:notebook-close (notebook)
  "Close NOTEBOOK and kill its buffer."
  (interactive (prog1 (list (ein:notebook--get-nb-or-error))
                 (or (ein:notebook-ask-before-kill-buffer)
                     (error "Quit"))))
  (let ((ein:notebook-kill-buffer-ask nil))
    ;; Let `ein:notebook-kill-buffer-callback' do its job.
    (mapc #'kill-buffer (ein:notebook-buffer-list notebook))))

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

(defun ein:fast-content-from-notebook (notebook)
  "Quickly generate a basic content structure from notebook. This
function does not generate the full json representation of the
notebook worksheets."
  (make-ein:$content :name (ein:$notebook-notebook-name notebook)
                     :path (ein:$notebook-notebook-path notebook)
                     :url-or-port (ein:$notebook-url-or-port notebook)
                     :type "notebook"
                     :ipython-version (ein:$notebook-api-version notebook)))

(defun ein:notebook-create-checkpoint (notebook)
  "Create checkpoint for current notebook based on most recent save."
  (interactive
   (let* ((notebook (ein:get-notebook)))
     (list notebook)))
  (ein:events-trigger (ein:$notebook-events notebook)
                      'notebook_create_checkpoint.Notebook)
  (ein:content-create-checkpoint (ein:fast-content-from-notebook notebook)
                                 (lexical-let ((notebook notebook))
                                   #'(lambda (content)
                                       (ein:log 'verbose "Checkpoint %s for %s generated."
                                                (plist-get (first (ein:$content-checkpoints content)) :id)
                                                (ein:$notebook-notebook-name notebook))
                                       (ein:events-trigger (ein:$notebook-events notebook)
                                                           'notebook_checkpoint_created.Notebook)
                                       (setf (ein:$notebook-checkpoints notebook)
                                             (ein:$content-checkpoints content))))))

(defun ein:notebook-list-checkpoint-ids (notebook)
  (unless (ein:$notebook-checkpoints notebook)
    (ein:content-query-checkpoints (ein:fast-content-from-notebook notebook)
                                   (lexical-let ((notebook notebook))
                                     #'(lambda (content)
                                         (setf (ein:$notebook-checkpoints notebook)
                                               (ein:$content-checkpoints content)))))
    (sleep-for 0.5))
  (loop for cp in (ein:$notebook-checkpoints notebook)
        collecting (plist-get cp :last_modified)))

(defun ein:notebook-restore-to-checkpoint (notebook checkpoint)
  "Restore notebook to previous checkpoint saved on the Jupyter
server. Note that if there are multiple checkpoints the user will
be prompted on which one to use."
  (interactive
   (let* ((notebook (ein:get-notebook))
          (checkpoint (completing-read
                       "Select checkpoint: "
                       (ein:notebook-list-checkpoint-ids notebook))))
     (list notebook checkpoint)))
  (ein:content-restore-checkpoint (ein:fast-content-from-notebook notebook)
                                  checkpoint)
  (ein:notebook-close notebook)
  (ein:notebook-open (ein:$notebook-url-or-port notebook)
                     (ein:$notebook-notebook-path notebook)))


;;; Worksheet

(defmacro ein:notebook--worksheet-render-new (notebook type)
  "Create new worksheet of TYPE in NOTEBOOK."
  (let ((func (intern (format "ein:%s-new" type)))
        (slot (list (intern (format "ein:$notebook-%ss" type)) notebook)))
    `(let ((ws (ein:notebook--worksheet-new ,notebook #',func)))
       (setf ,slot (append ,slot (list ws)))
       (ein:notebook--worksheet-render ,notebook ws)
       ws)))

(defun ein:notebook-worksheet-render-new (notebook)
  "Create new worksheet in NOTEBOOK."
  (ein:notebook--worksheet-render-new notebook worksheet))

(defun ein:notebook-worksheet-open-next-or-new (notebook ws &optional show)
  "Open next worksheet.  Create new if none.

Try to open the worksheet to the worksheet WS using the function
`ein:notebook-worksheet-open-next', open a new worksheet if not
found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ein:notebook-worksheet-open-next notebook ws)))
    (unless next
      (ein:log 'info "Creating new worksheet...")
      (setq next (ein:notebook-worksheet-render-new notebook))
      (ein:log 'info "Creating new worksheet... Done."))
    (when show
      (funcall show (ein:worksheet-buffer next)))))

(defun ein:notebook-worksheet-open-next-or-first (notebook ws &optional show)
  "Open next or first worksheet.

Try to open the worksheet to the worksheet WS using the function
`ein:notebook-worksheet-open-next', open the first worksheet if
not found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ein:notebook-worksheet-open-next notebook ws)))
    (unless next
      (setq next (car (ein:$notebook-worksheets notebook))))
    (when show
      (funcall show (ein:worksheet-buffer next)))))

(defun ein:notebook-worksheet-open-prev-or-last (notebook ws &optional show)
  "Open previous or last worksheet.
See also `ein:notebook-worksheet-open-next-or-first' and
`ein:notebook-worksheet-open-prev'."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (ein:notebook-worksheet-open-prev notebook ws)))
    (unless prev
      (setq prev (car (last (ein:$notebook-worksheets notebook)))))
    (when show
      (funcall show (ein:worksheet-buffer prev)))))

(defun* ein:notebook-worksheet--render-maybe
    (notebook ws &optional (adj "next"))
  "Render worksheet WS of NOTEBOOK if it does not have buffer.
ADJ is a adjective to describe worksheet to be rendered."
  (if (ein:worksheet-has-buffer-p ws)
      (ein:log 'verbose "The worksheet already has a buffer.")
    (ein:log 'info "Rendering %s worksheet..." adj)
    (ein:notebook--worksheet-render notebook ws)
    (ein:log 'info "Rendering %s worksheet... Done." adj)))

(defun* ein:notebook-worksheet--open-new
    (notebook new &optional (adj "next") show)
  "Open (possibly new) worksheet NEW of NOTEBOOK with SHOW function.
ADJ is a adjective to describe worksheet to be opened.
SHOW is a function to be called with worksheet buffer if given."
  (when new
    (ein:notebook-worksheet--render-maybe notebook new adj))
  (when show
    (assert (ein:worksheet-p new) nil "No %s worksheet." adj)
    (funcall show (ein:worksheet-buffer new))))

(defun ein:notebook-worksheet-open-next (notebook ws &optional show)
  "Open next worksheet.

Search the worksheet after the worksheet WS, render it if it is
not yet, then return the worksheet.  If there is no such
worksheet, return nil.  Open the first worksheet if the worksheet
WS is an instance of `ein:scratchsheet'.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (if (ein:scratchsheet-p ws)
                  (car (ein:$notebook-worksheets notebook))
                (loop with worksheets = (ein:$notebook-worksheets notebook)
                      for current in worksheets
                      for next in (cdr worksheets)
                      when (eq current ws) return next))))
    (ein:notebook-worksheet--open-new notebook next "next" show)
    next))

(defun ein:notebook-worksheet-open-prev (notebook ws &optional show)
  "Open previous worksheet.
See also `ein:notebook-worksheet-open-next'."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (if (ein:scratchsheet-p ws)
                  (car (last (ein:$notebook-worksheets notebook)))
                (loop for (prev current) on (ein:$notebook-worksheets notebook)
                      when (eq current ws) return prev))))
    (ein:notebook-worksheet--open-new notebook prev "previous" show)
    prev))

(defun ein:notebook-worksheet-open-ith (notebook i &optional show)
  "Open I-th (zero-origin) worksheet."
  (let ((ws (nth i (ein:$notebook-worksheets notebook))))
    (unless ws (error "No %s-th worksheet" (1+ i)))
    (ein:notebook-worksheet--open-new notebook ws (format "%s-th" i) show)))

(defmacro ein:notebook-worksheet--defun-open-nth (n)
  "Define a command to open N-th (one-origin) worksheet."
  (assert (and (integerp n) (> n 0)) t)
  (let ((func (intern (format "ein:notebook-worksheet-open-%sth" n))))
    `(defun ,func (notebook &optional show)
       ,(format "Open %d-th worksheet." n)
       (interactive (list (ein:notebook--get-nb-or-error)
                          #'switch-to-buffer))
       (ein:notebook-worksheet-open-ith notebook ,(1- n) show))))

(defmacro ein:notebook-worksheet--defun-all-open-nth (min max)
  `(progn
     ,@(loop for n from min to max
             collect `(ein:notebook-worksheet--defun-open-nth ,n))))

(ein:notebook-worksheet--defun-all-open-nth 1 8)

(defun ein:notebook-worksheet-open-last (notebook &optional show)
  "Open the last worksheet."
  (interactive (list (ein:notebook--get-nb-or-error)
                     #'switch-to-buffer))
  (let ((last (car (last (ein:$notebook-worksheets notebook)))))
    (ein:notebook-worksheet--open-new notebook last "last" show)
    last))

(defun ein:notebook-worksheet-insert-new (notebook ws &optional render show
                                                   inserter)
  (let ((new (ein:notebook--worksheet-new notebook)))
    (setf (ein:$notebook-worksheets notebook)
          (funcall inserter (ein:$notebook-worksheets notebook) ws new))
    (when (or render show)
      (ein:notebook--worksheet-render notebook new))
    (when show
      (funcall show (ein:worksheet-buffer new)))
    new))

(defun* ein:notebook-worksheet-insert-next
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet after this worksheet and open it.
See also `ein:notebook-worksheet-insert-prev'.

.. The worksheet WS is searched in the worksheets slot of
   NOTEBOOK and a newly created worksheet is inserted after WS.
   Worksheet buffer is created when RENDER or SHOW is non-`nil'.
   SHOW is a function which take a buffer."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (ein:notebook-worksheet-insert-new notebook ws render show
                                     #'ein:list-insert-after))

(defun* ein:notebook-worksheet-insert-prev
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet before this worksheet and open it.
See also `ein:notebook-worksheet-insert-next'."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (ein:notebook-worksheet-insert-new notebook ws render show
                                     #'ein:list-insert-before))

(defun ein:notebook-worksheet-delete (notebook ws &optional confirm)
  "Delete the current worksheet.
When used as a lisp function, delete worksheet WS from NOTEBOOk."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     t))
  (when confirm
    (unless (y-or-n-p
             "Really remove this worksheet? There is no undo.")
      (error "Quit deleting the current worksheet.")))
  (setf (ein:$notebook-worksheets notebook)
        (delq ws (ein:$notebook-worksheets notebook)))
  (setf (ein:$notebook-dirty notebook) t)
  (let ((ein:notebook-kill-buffer-ask nil))
    (kill-buffer (ein:worksheet-buffer ws))))

(defun ein:notebook-worksheet-move-prev (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (assert (ein:worksheet-p ws) nil "Not worksheet.")
  (setf (ein:$notebook-worksheets notebook)
        (ein:list-move-left (ein:$notebook-worksheets notebook) ws)))

(defun ein:notebook-worksheet-move-next (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (assert (ein:worksheet-p ws) nil "Not worksheet.")
  (setf (ein:$notebook-worksheets notebook)
        (ein:list-move-right (ein:$notebook-worksheets notebook) ws)))

(defun* ein:notebook-worksheet-index
    (&optional (notebook ein:%notebook%)
               (ws ein:%worksheet%))
  "Return an index of the worksheet WS in NOTEBOOK."
  (loop for i from 0
        for ith-ws in (ein:$notebook-worksheets notebook)
        when (eq ith-ws ws)
        return i))


;;; Scratch sheet

(defun ein:notebook-scratchsheet-render-new (notebook)
  "Create new scratchsheet in NOTEBOOK."
  (ein:notebook--worksheet-render-new notebook scratchsheet))

(defun ein:notebook-scratchsheet-open (notebook &optional new popup)
  "Open \"scratch sheet\".
Open a new one when prefix argument is given.
Scratch sheet is almost identical to worksheet.  However, EIN
will not save the buffer.  Use this buffer like of normal IPython
console.  Note that you can always copy cells into the normal
worksheet to save result."
  (interactive (list (ein:get-notebook-or-error)
                     current-prefix-arg
                     t))
  (let ((ss (or (unless new
                  (car (ein:$notebook-scratchsheets notebook)))
                (ein:notebook-scratchsheet-render-new notebook))))
    (when popup
      (pop-to-buffer (ein:worksheet-buffer ss)))
    ss))


;;; Opened notebooks

(defvar ein:notebook--opened-map (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT NOTEBOOK-ID) => notebook instance.")

(defun ein:notebook-get-opened-notebook (url-or-port path)
  (gethash (list url-or-port path) ein:notebook--opened-map))

(defun ein:notebook-get-opened-buffer (url-or-port path)
  (ein:aand (ein:notebook-get-opened-notebook url-or-port path)
            (ein:notebook-buffer it)))

(defun ein:notebook-put-opened-notebook (notebook)
  (puthash (list (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-path notebook))
           notebook
           ein:notebook--opened-map))

(defun ein:notebook-opened-notebooks (&optional predicate)
  "Return list of opened notebook instances.
If PREDICATE is given, notebooks are filtered by PREDICATE.
PREDICATE is called with each notebook and notebook is included
in the returned list only when PREDICATE returns non-nil value."
  (let (notebooks)
    (maphash (lambda (k n) (if (ein:notebook-live-p n)
                               (push n notebooks)
                             (remhash k ein:notebook--opened-map)))
             ein:notebook--opened-map)
    (if predicate
        (ein:filter predicate notebooks)
      notebooks)))

(defun ein:notebook-opened-buffers (&optional predicate)
  "Return list of opened notebook buffers."
  (mapcar #'ein:notebook-buffer (ein:notebook-opened-notebooks predicate)))

(defun ein:notebook-opened-buffer-names (&optional predicate)
  "Return list of opened notebook buffer names."
  (mapcar #'buffer-name (ein:notebook-opened-buffers predicate)))


;;; Generic getter

(defun ein:get-url-or-port--notebook ()
  (when ein:%notebook% (ein:$notebook-url-or-port ein:%notebook%)))

(defun ein:get-notebook--notebook ()
  ein:%notebook%)

(defun ein:get-kernel--notebook ()
  (when (ein:$notebook-p ein:%notebook%)
    (ein:$notebook-kernel ein:%notebook%)))


;;; Predicate

(defun ein:notebook-buffer-p ()
  "Return non-`nil' if current buffer is notebook buffer."
  ein:%notebook%)

(defun ein:notebook-live-p (notebook)
  "Return non-`nil' if NOTEBOOK has live buffer."
  (buffer-live-p (ein:notebook-buffer notebook)))

(defun ein:notebook-modified-p (&optional notebook)
  "Return non-nil if NOTEBOOK is modified.
If NOTEBOOK is not given or nil then consider the notebook
associated with current buffer (if any)."
  (unless notebook (setq notebook ein:%notebook%))
  (and (ein:$notebook-p notebook)
       (ein:notebook-live-p notebook)
       (or (ein:$notebook-dirty notebook)
           (loop for ws in (ein:$notebook-worksheets notebook)
                 when (ein:worksheet-modified-p ws)
                 return t))))


;;; Notebook mode

(defcustom ein:notebook-modes
  '(ein:notebook-multilang-mode)
  "Notebook modes to use \(in order of preference).

When the notebook is opened, mode in this value is checked one by one
and the first usable mode is used.

Available modes:

* `ein:notebook-multilang-mode'
* `ein:notebook-mumamo-mode'
* `ein:notebook-python-mode'
* `ein:notebook-plain-mode'

Examples:

Use MuMaMo if it is installed.  Otherwise, use plain mode.
This is the old default setting::

  (setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-plain-mode))

Avoid using MuMaMo even when it is installed::

  (setq ein:notebook-modes '(ein:notebook-plain-mode))

Use simple `python-mode' based notebook mode when MuMaMo is not installed::

  (setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-python-mode))
"
  :type '(repeat (choice (const :tag "Multi-lang" ein:notebook-multilang-mode)
                         (const :tag "MuMaMo" ein:notebook-mumamo-mode)
                         (const :tag "Only Python" ein:notebook-python-mode)
                         (const :tag "Plain" ein:notebook-plain-mode)))
  :group 'ein)

(defcustom ein:notebook-mode-hook nil
  "Hook for `ein:notebook-mode'.
This hook is run regardless the actual major mode used."
  :type 'hook
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
  (define-key map "\C-ci" 'ein:inspect-object)
  (define-key map "\C-c'" 'ein:edit-cell-contents)
  (define-key map "\C-cS" 'ein:worksheet-toggle-slideshow-view)
  (define-key map "\C-c\C-c" 'ein:worksheet-execute-cell)
  (define-key map (kbd "M-RET") 'ein:worksheet-execute-cell-and-goto-next)
  (define-key map (kbd "<M-S-return>")
    'ein:worksheet-execute-cell-and-insert-below)
  (define-key map (kbd "C-c C-'") 'ein:worksheet-turn-on-autoexec)
  (define-key map "\C-c\C-e" 'ein:worksheet-toggle-output)
  (define-key map "\C-c\C-v" 'ein:worksheet-set-output-visibility-all)
  (define-key map "\C-c\C-l" 'ein:worksheet-clear-output)
  (define-key map (kbd "C-c C-S-l") 'ein:worksheet-clear-all-output)
  (define-key map (kbd "C-c C-;") 'ein:shared-output-show-code-cell-at-point)
  (define-key map "\C-c\C-k" 'ein:worksheet-kill-cell)
  (define-key map "\C-c\M-w" 'ein:worksheet-copy-cell)
  (define-key map "\C-c\C-w" 'ein:worksheet-copy-cell)
  (define-key map "\C-c\C-y" 'ein:worksheet-yank-cell)
  (define-key map "\C-c\C-a" 'ein:worksheet-insert-cell-above)
  (define-key map "\C-c\C-b" 'ein:worksheet-insert-cell-below)
  (define-key map "\C-c\C-t" 'ein:worksheet-toggle-cell-type)
  (define-key map "\C-c\C-d" 'ein:worksheet-toggle-slide-type)
  (define-key map "\C-c\C-u" 'ein:worksheet-change-cell-type)
  (define-key map "\C-c\C-s" 'ein:worksheet-split-cell-at-point)
  (define-key map "\C-c\C-m" 'ein:worksheet-merge-cell)
  (define-key map "\C-c\C-n" 'ein:worksheet-goto-next-input)
  (define-key map "\C-c\C-p" 'ein:worksheet-goto-prev-input)
  (define-key map (kbd "C-<up>") 'ein:worksheet-goto-prev-input)
  (define-key map (kbd "C-<down>") 'ein:worksheet-goto-next-input)
  (define-key map (kbd "C-c <up>") 'ein:worksheet-move-cell-up)
  (define-key map (kbd "C-c <down>") 'ein:worksheet-move-cell-down)
  (define-key map (kbd "M-<up>") 'ein:worksheet-move-cell-up)
  (define-key map (kbd "M-<down>") 'ein:worksheet-move-cell-down)
  (define-key map "\C-c\C-f" 'ein:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ein:completer-complete)
  (define-key map "\C-c\C-x" 'ein:tb-show)
  (define-key map "\C-c\C-r" 'ein:notebook-restart-kernel-command)
  (define-key map "\C-c\C-z" 'ein:notebook-kernel-interrupt-command)
  (define-key map "\C-c\C-q" 'ein:notebook-kill-kernel-then-close-command)
  (define-key map (kbd "C-c C-#") 'ein:notebook-close)
  (define-key map (kbd "C-:") 'ein:shared-output-eval-string)
  (define-key map "\C-c\C-o" 'ein:console-open)
  (define-key map "\C-x\C-s" 'ein:notebook-save-notebook-command)
  (define-key map "\C-x\C-w" 'ein:notebook-rename-command)
  (define-key map "\M-."          'ein:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein:pytools-jump-back-command)
  (define-key map "\M-p"          'ein:worksheet-previous-input-history)
  (define-key map "\M-n"          'ein:worksheet-next-input-history)
  (define-key map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)
  ;; Worksheets
  (define-key map (kbd "C-c !")     'ein:worksheet-rename-sheet)
  (define-key map (kbd "C-c {")     'ein:notebook-worksheet-open-prev-or-last)
  (define-key map (kbd "C-c }")     'ein:notebook-worksheet-open-next-or-first)
  (define-key map (kbd "C-c M-{")   'ein:notebook-worksheet-move-prev)
  (define-key map (kbd "C-c M-}")   'ein:notebook-worksheet-move-next)
  (define-key map (kbd "C-c +")     'ein:notebook-worksheet-insert-next)
  (define-key map (kbd "C-c M-+")   'ein:notebook-worksheet-insert-prev)
  (define-key map (kbd "C-c -")     'ein:notebook-worksheet-delete)
  (loop for n from 1 to 8
        do (define-key map (format "\C-c%d" n)
             (intern (format "ein:notebook-worksheet-open-%sth" n))))
  (define-key map "\C-c9" 'ein:notebook-worksheet-open-last)
  ;; Menu
  (easy-menu-define ein:notebook-menu map "EIN Notebook Mode Menu"
    `("EIN Notebook"
      ("File"
       ,@(ein:generate-menu
          '(("Save notebook" ein:notebook-save-notebook-command)
            ("Copy and rename notebook" ein:notebook-save-to-command)
            ("Rename notebook" ein:notebook-rename-command)
            ("Close notebook without saving"
             ein:notebook-close)
            ("Kill kernel then close notebook"
             ein:notebook-kill-kernel-then-close-command))))
      ("Edit"
       ,@(ein:generate-menu
          '(("Kill cell" ein:worksheet-kill-cell)
            ("Copy cell" ein:worksheet-copy-cell)
            ("Yank cell" ein:worksheet-yank-cell)
            ("Insert cell above" ein:worksheet-insert-cell-above)
            ("Insert cell below" ein:worksheet-insert-cell-below)
            ("Toggle cell type" ein:worksheet-toggle-cell-type)
            ("Toggle slide type" ein:worksheet-toggle-slide-type)
            ("Change cell type" ein:worksheet-change-cell-type)
            ("Split cell at point" ein:worksheet-split-cell-at-point)
            ("Merge cell" ein:worksheet-merge-cell)
            ("Go to next cell" ein:worksheet-goto-next-input)
            ("Go to previous cell" ein:worksheet-goto-prev-input)
            ("Move cell up" ein:worksheet-move-cell-up)
            ("Move cell down" ein:worksheet-move-cell-down)
            ("Dedent text in CELL" ein:worksheet-dedent-cell-text)
            )))
      ("Cell/Code"
       ,@(ein:generate-menu
          '(("Edit cell contents in dedicated buffer" ein:edit-cell-contents)
            ("Execute cell" ein:worksheet-execute-cell
             :active (ein:worksheet-at-codecell-p))
            ("Execute cell and go to next"
             ein:worksheet-execute-cell-and-goto-next
             :active (ein:worksheet-at-codecell-p))
            ("Execute cell and insert below"
             ein:worksheet-execute-cell-and-insert-below
             :active (ein:worksheet-at-codecell-p))
            ("Execute all"
             ein:worksheet-execute-all-cell)
            ("Turn on auto execution flag" ein:worksheet-turn-on-autoexec
             :active (ein:worksheet-at-codecell-p))
            ("Evaluate code in minibuffer" ein:shared-output-eval-string)
            ("Toggle instant cell execution mode" ein:iexec-mode)
            ))
       "---"
       ,@(ein:generate-menu
          '(("Toggle output visibility" ein:worksheet-toggle-output
             :active (ein:worksheet-at-codecell-p))
            ("Show all output"
             ein:worksheet-set-output-visibility-all)
            ("Discard output" ein:worksheet-clear-output
             :active (ein:worksheet-at-codecell-p))
            ("Discard all output" ein:worksheet-clear-all-output)
            ("Show full output" ein:shared-output-show-code-cell-at-point
             :active (ein:worksheet-at-codecell-p))
            ("Traceback viewer" ein:tb-show)
            ))
       "---"
       ,@(ein:generate-menu
          '(("Show object help"
             ein:pytools-request-tooltip-or-help)
            ("Complete code" ein:completer-complete
             :active (ein:worksheet-at-codecell-p))
            ("Jump to definition" ein:pytools-jump-to-source-command)
            ("Go back to the previous jump point"
             ein:pytools-jump-back-command)
            ("Previous input history"
             ein:worksheet-previous-input-history)
            ("Next input history"
             ein:worksheet-next-input-history))))
      ("Kernel"
       ,@(ein:generate-menu
          '(("Restart kernel" ein:notebook-restart-kernel-command)
            ("Switch kernel" ein:notebook-switch-kernel)
            ("Interrupt kernel" ein:notebook-kernel-interrupt-command))))
      ("Worksheets [Experimental]"
       ,@(ein:generate-menu
          '(("Toggle slide metadata view" ein:worksheet-toggle-slideshow-view)
            ("Rename worksheet" ein:worksheet-rename-sheet)
            ("Insert next worksheet"
             ein:notebook-worksheet-insert-next)
            ("Insert previous worksheet"
             ein:notebook-worksheet-insert-prev)
            ("Delete worksheet" ein:notebook-worksheet-delete)
            ("Move worksheet left"  ein:notebook-worksheet-move-prev)
            ("Move worksheet right" ein:notebook-worksheet-move-next)
            ))
       "---"
       ,@(ein:generate-menu
          '(("Open previous worksheet"
             ein:notebook-worksheet-open-prev)
            ("Open previous or last worksheet"
             ein:notebook-worksheet-open-prev-or-last)
            ("Open next worksheet"
             ein:notebook-worksheet-open-next)
            ("Open next or first worksheet"
             ein:notebook-worksheet-open-next-or-first)
            ("Open next or new worksheet"
             ein:notebook-worksheet-open-next-or-new)
            ))
       "---"
       ,@(ein:generate-menu
          (append
           (loop for n from 1 to 8
                 collect
                 (list
                  (format "Open %d-th worksheet" n)
                  (intern (format "ein:notebook-worksheet-open-%sth" n))))
           '(("Open last worksheet" ein:notebook-worksheet-open-last)))))
      ("Junk notebook"
       ,@(ein:generate-menu
          '(("Junk this notebook" ein:junk-rename)
            ("Open new junk" ein:junk-new))))
      ;; Misc:
      ,@(ein:generate-menu
         '(("Open regular IPython console" ein:console-open)
           ("Open scratch sheet" ein:notebook-scratchsheet-open)
           ("Toggle pseudo console mode" ein:pseudo-console-mode)
           ))
      ))
  map)

(defun ein:notebook-mode ()
  (funcall (ein:notebook-choose-mode))
  (ein:complete-on-dot-install
   ein:notebook-mode-map 'ein:notebook-complete-dot)
  (ein:aif ein:helm-kernel-history-search-key
      (define-key ein:notebook-mode-map it 'helm-ein-kernel-history))
  (ein:aif ein:anything-kernel-history-search-key
      (define-key ein:notebook-mode-map it 'anything-ein-kernel-history))
  (ein:notebook-minor-mode +1)
  (setq indent-tabs-mode nil) ;; Being T causes problems with Python code.
  (run-hooks 'ein:notebook-mode-hook))

(add-hook 'ein:notebook-mode-hook 'ein:worksheet-imenu-setup)

(define-minor-mode ein:notebook-minor-mode
  "Minor mode to install `ein:notebook-mode-map' for `ein:notebook-mode'."
  :keymap ein:notebook-mode-map
  :group 'ein)

;; To avoid MuMaMo to discard `ein:notebook-minor-mode', make it
;; permanent local.
(put 'ein:notebook-minor-mode 'permanent-local t)

(define-derived-mode ein:notebook-plain-mode fundamental-mode "ein:notebook"
  "IPython notebook mode without fancy coloring."
  (font-lock-mode))

(define-derived-mode ein:notebook-python-mode python-mode "ein:python"
  "Use `python-mode' for whole notebook buffer.")

(defun ein:notebook-open-in-browser (&optional print)
  "Open current notebook in web browser.
When the prefix argument (``C-u``) is given, print page is opened.
Note that print page is not supported in IPython 0.12.1."
  (interactive "P")
  (let ((url (apply #'ein:url
                    (ein:$notebook-url-or-port ein:%notebook%)
                    (if (>= (ein:$notebook-api-version ein:%notebook%) 3)
                        "notebooks")
                    (ein:$notebook-notebook-path ein:%notebook%)
                    (if print (list "print")))))
    (message "Opening %s in browser" url)
    (browse-url url)))

(defun ein:notebook-fetch-data (notebook callback &optional cbargs)
  "Fetch data in body tag of NOTEBOOK html page.
CALLBACK is called with a plist with data in the body tag as
the first argument and CBARGS as the rest of arguments."
  (let ((url-or-port (ein:$notebook-url-or-port notebook))
        (notebook-id (ein:$notebook-notebook-id notebook)))
    (ein:query-singleton-ajax
     (list 'notebook-fetch-data url-or-port notebook-id)
     (ein:url url-or-port notebook-id)
     :parser
     (lambda ()
       (list
        :project
        (ein:html-get-data-in-body-tag "data-project")
        :base-project-url
        (ein:html-get-data-in-body-tag "data-base-project-url")
        :base-kernel-url
        (ein:html-get-data-in-body-tag "data-base-kernel-url")
        :read-only
        (ein:html-get-data-in-body-tag "data-read-only")
        :notebook-id
        (ein:html-get-data-in-body-tag "data-notebook-id")))
     :success
     (apply-partially (function*
                       (lambda (callback cbargs &key data &allow-other-keys)
                         (apply callback data cbargs)))
                      callback cbargs))))


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
  (not (or (and ein:notebook-kill-buffer-ask
                (ein:worksheet-p ein:%worksheet%) ; it's not `ein:scratchsheet'
                (ein:notebook-modified-p)
                (not (y-or-n-p
                      "This notebook has unsaved changes. Discard those changes?")))
           (when (ein:worksheet-p ein:%worksheet%)
             ;; To make `ein:worksheet-save-cells' no-op.
             (ein:worksheet-dont-save-cells ein:%worksheet%)
             nil))))

(add-hook 'kill-buffer-query-functions 'ein:notebook-ask-before-kill-buffer)

;; -- `kill-emacs-query-functions'
(defun ein:notebook-ask-before-kill-emacs ()
  "Return `nil' to prevent killing Emacs when unsaved notebook exists.
Called via `kill-emacs-query-functions'."
  (condition-case err
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
            answer)))
    ((debug error)
     (ein:log 'error "Got error: %S" err)
     (y-or-n-p "Error while examine notebooks.  Kill Emacs anyway? "))))

(add-hook 'kill-emacs-query-functions 'ein:notebook-ask-before-kill-emacs)

;; -- `kill-buffer-hook'
(defun ein:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  (when (ein:$notebook-p ein:%notebook%)
    (ein:notebook-disable-autosaves ein:%notebook%)
    (ein:notebook-close-worksheet ein:%notebook% ein:%worksheet%)))

(defun ein:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ein:notebook-kill-buffer-callback))

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
