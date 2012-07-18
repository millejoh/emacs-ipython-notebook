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

(require 'ein-utils)
(require 'ein-log)
(require 'ein-node)
(require 'ein-kernel)
(require 'ein-cell)
(require 'ein-completer)
(require 'ein-pager)
(require 'ein-events)
(require 'ein-notification)
(require 'ein-kill-ring)
(require 'ein-query)
(require 'ein-shared-output)
(require 'ein-pytools)
(require 'ein-traceback)


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
(defvar ein:notebook-tb-buffer-name-template "*ein:tb %s/%s*")
(defvar ein:notebook-buffer-name-template "*ein: %s/%s*")

(defvar ein:notebook-save-retry-max 1
  "Maximum retries for notebook saving.")

(defvar ein:notebook-opened-map (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT NOTEBOOK-ID) => notebook buffer.")

(defstruct ein:$notebook
  "Hold notebook variables.

`ein:$notebook-url-or-port'
  URL or port of IPython server.

`ein:$notebook-notebook-id' : string
  uuid string

`ein:$notebook-data' - FIXME: remove this!
  Original notebook JSON data sent from server.  This slot exists
  first for debugging reason and should be deleted later.

`ein:$notebook-ewoc' : `ewoc'
  An instance of `ewoc'.  Notebook is rendered using `ewoc'.
  Also `ewoc' nodes are used for saving cell data.

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

`ein:$notebook-events' : `ein:$events'
  Event handler instance.

`ein:$notebook-notification' : `ein:notification'
  Notification widget.

`ein:$notebook-traceback' : `ein:traceback'
  Traceback viewer."
  url-or-port
  notebook-id
  data
  ewoc
  kernel
  pager
  dirty
  metadata
  notebook-name
  nbformat
  events
  notification
  traceback
  )

(ein:deflocal ein:notebook nil
  "Buffer local variable to store an instance of `ein:$notebook'.")

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
  "Similar to `ein:notebook-with-cell' but sets a list of cells to `cells'."
  (declare (indent 0))
  `(let* ((cells (ein:notebook-get-cells-in-region-or-at-point)))
     (if cells
         (progn ,@body)
       (ein:log 'warn "Not in cell"))))

(defun ein:notebook-new (url-or-port notebook-id &rest args)
  (let ((notebook (apply #'make-ein:$notebook
                         :url-or-port url-or-port
                         :notebook-id notebook-id
                         args)))
    notebook))

(defun ein:notebook-init (notebook data)
  "Initialize NOTEBOOK with DATA from the server."
  (setf (ein:$notebook-data notebook) data)
  (let* ((metadata (plist-get data :metadata))
         (notebook-name (plist-get metadata :name)))
    (setf (ein:$notebook-metadata notebook) metadata)
    (setf (ein:$notebook-nbformat notebook) (plist-get data :nbformat))
    (setf (ein:$notebook-notebook-name notebook) notebook-name)))

(defun ein:notebook-del (notebook)
  "Destructor for `ein:$notebook'."
  (ein:log-ignore-errors
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:log-del))
    (ein:kernel-del (ein:$notebook-kernel notebook))))

(defun ein:notebook-get-buffer-name (notebook)
  (format ein:notebook-buffer-name-template
          (ein:$notebook-url-or-port notebook)
          (ein:$notebook-notebook-name notebook)))

(defun ein:notebook-get-buffer (notebook)
  (get-buffer-create (ein:notebook-get-buffer-name notebook)))

(defun ein:notebook-buffer (notebook)
  "Return the buffer that is associated with NOTEBOOK."
  (ewoc-buffer (ein:$notebook-ewoc notebook)))

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
  (let* ((key (list url-or-port notebook-id))
         (buffer (gethash key ein:notebook-opened-map)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (when callback
            (apply callback ein:notebook nil cbargs))
          ein:notebook)
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
      (with-current-buffer (ein:notebook-get-buffer notebook)
        (apply callback notebook t cbargs)))))

(defun* ein:notebook-request-open-callback (notebook &key status data
                                                     &allow-other-keys)
  (ein:log 'debug "URL-RETRIEVE nodtebook-id = %S, status = %S"
           (ein:$notebook-notebook-id notebook)
           status)
  (let ((notebook-id (ein:$notebook-notebook-id notebook)))
    (ein:notebook-init notebook data)
    (with-current-buffer (ein:notebook-get-buffer notebook)
      (ein:log-setup (ein:$notebook-notebook-id notebook))
      (setq ein:notebook notebook)
      (ein:notebook-render)
      (set-buffer-modified-p nil)
      (puthash (list (ein:$notebook-url-or-port ein:notebook) notebook-id)
               (current-buffer)
               ein:notebook-opened-map))))

(defun ein:notebook-render ()
  "(Re-)Render the notebook."
  (interactive)
  (assert ein:notebook)  ; make sure in a notebook buffer
  (ein:notebook-from-json ein:notebook (ein:$notebook-data ein:notebook))
  (setq buffer-undo-list nil)  ; clear undo history
  (when (eq ein:notebook-enable-undo 'no)
    (setq buffer-undo-list t))
  (ein:notebook-mode)
  (setf (ein:$notebook-notification ein:notebook)
        (ein:notification-setup (current-buffer)))
  (ein:notebook-bind-events ein:notebook (ein:events-new (current-buffer)))
  (ein:notebook-setup-traceback ein:notebook)
  (ein:notebook-start-kernel)
  (ein:log 'info "Notebook %s is ready"
           (ein:$notebook-notebook-name ein:notebook)))

(defun ein:notebook-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))


;;; Initialization.

(defun ein:notebook-bind-events (notebook events)
  "Bind events related to PAGER to the event handler EVENTS."
  (setf (ein:$notebook-events ein:notebook) events)
  (ein:events-on events
                 'set_next_input.Notebook
                 #'ein:notebook--set-next-input
                 notebook)
  (ein:events-on events
                 'set_dirty.Notebook
                 (lambda (notebook data)
                   (setf (ein:$notebook-dirty notebook)
                         (plist-get data :value)))
                 notebook)
  (ein:events-on events
                 'maybe_reset_undo.Notebook
                 (lambda (&rest -ignore-)
                   (ein:notebook-empty-undo-maybe)))
  ;; Bind events for sub components:
  (mapc (lambda (cell) (oset cell :events (ein:$notebook-events notebook)))
        (ein:notebook-get-cells notebook))
  (ein:notification-bind-events (ein:$notebook-notification ein:notebook)
                                events)
  (setf (ein:$notebook-pager notebook)
        (ein:pager-new
         (format ein:notebook-pager-buffer-name-template
                 (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-name notebook))
         (ein:$notebook-events notebook))))

(defun ein:notebook--set-next-input (notebook data)
  (let* ((cell (plist-get data :cell))
         (text (plist-get data :text))
         (new-cell (ein:notebook-insert-cell-below notebook 'code cell)))
    (ein:cell-set-text new-cell text)
    (setf (ein:$notebook-dirty notebook) t)))

(defun ein:notebook-setup-traceback (notebook)
  (setf (ein:$notebook-traceback notebook)
        (ein:tb-new
         (format ein:notebook-tb-buffer-name-template
                 (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-name notebook)))))


;;; Cell indexing, retrieval, etc.

(defun ein:notebook-cell-from-json (notebook data &rest args)
  (apply #'ein:cell-from-json
         data :ewoc (ein:$notebook-ewoc notebook) args))

(defun ein:notebook-cell-from-type (notebook type &rest args)
  ;; Note: TYPE can be a string.
  ;; FIXME: unify type of TYPE to symbol or string.
  (apply #'ein:cell-from-type
         (format "%s" type)
         :ewoc (ein:$notebook-ewoc notebook)
         :events (ein:$notebook-events notebook)
         args))

(defun ein:notebook-get-cells (notebook)
  (let* ((ewoc (ein:$notebook-ewoc notebook))
         (nodes (ewoc-collect ewoc (lambda (n) (ein:cell-node-p n 'prompt)))))
    (mapcar #'ein:$node-data nodes)))

(defun ein:notebook-ncells (notebook)
  (length (ein:notebook-get-cells notebook)))


;; Insertion and deletion of cells

(defun ein:notebook-delete-cell (notebook cell)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))        ; disable undo recording
    (apply #'ewoc-delete
           (ein:$notebook-ewoc notebook)
           (ein:cell-all-element cell)))
  (setf (ein:$notebook-dirty notebook) t)
  (ein:notebook-empty-undo-maybe))

(defun ein:notebook-delete-cell-command ()
  "Delete a cell.  \(WARNING: no undo!)
This command has no key binding because there is no way to undo
deletion.  Use kill to play on the safe side.

If you really want use this command, you can do something like this
\(but be careful when using it!)::

  \(define-key ein:notebook-mode-map \"\\C-c\\C-d\"
              'ein:notebook-delete-cell-command)"
  (interactive)
  (ein:notebook-with-cell nil
    (ein:notebook-delete-cell ein:notebook cell)
    (ein:aif (ein:notebook-get-current-cell) (ein:cell-goto it))))

(defun ein:notebook-kill-cells (notebook cells)
  (when cells
    (mapc (lambda (c)
            (ein:cell-save-text c)
            (ein:notebook-delete-cell notebook c)
            (ein:cell-deactivate c))
          cells)
    (ein:kill-new cells)))

(defun ein:notebook-kill-cell-command ()
  "Kill (\"cut\") the cell at point.
Note that the kill-ring for cells is not shared with the default
kill-ring of Emacs (kill-ring for texts)."
  (interactive)
  (ein:notebook-with-cells-in-region
    (ein:notebook-kill-cells ein:notebook cells)
    (deactivate-mark)
    (ein:aif (ein:notebook-get-current-cell) (ein:cell-goto it))))

(defun ein:notebook-copy-cell-command ()
  "Copy the cell at point.  (Put the current cell into the kill-ring.)"
  (interactive)
  (ein:notebook-with-cells-in-region
    (deactivate-mark)
    (let ((cells (mapcar
                  (lambda (c)
                    (ein:cell-deactivate (ein:cell-copy c))) cells)))
      (ein:log 'info "%s cells are copied." (length  cells))
      (ein:kill-new cells))))

(defun ein:notebook-insert-clone-below (notebook cell pivot)
  (let ((clone (ein:cell-copy cell)))
    ;; Cell can be from another buffer, so reset `ewoc'.
    (oset clone :ewoc (ein:$notebook-ewoc notebook))
    (ein:notebook-insert-cell-below notebook clone pivot)
    clone))

(defun ein:notebook-yank-cell-command (&optional arg)
  "Insert (\"paste\") the latest killed cell.
Prefixes are act same as the normal `yank' command."
  (interactive "*P")
  ;; Do not use `ein:notebook-with-cell'.
  ;; `ein:notebook-insert-cell-below' handles empty cell.
  (let* ((cell (ein:notebook-get-current-cell))
         (killed (ein:current-kill (cond
                                    ((listp arg) 0)
                                    ((eq arg '-) -2)
                                    (t (1- arg))))))
    (loop for c in killed
          with last = cell
          do (setq last (ein:notebook-insert-clone-below ein:notebook c last))
          finally (ein:cell-goto last))))

(defun ein:notebook-maybe-new-cell (notebook type-or-cell)
  "Return TYPE-OR-CELL as-is if it is a cell, otherwise return a new cell."
  (let ((cell (if (ein:basecell-child-p type-or-cell)
                  type-or-cell
                (ein:notebook-cell-from-type notebook type-or-cell))))
    ;; When newly created or copied, kernel is not attached or not the
    ;; kernel of this notebook.  So reset it here.
    (when (ein:codecell-p cell)
      (oset cell :kernel (ein:$notebook-kernel notebook)))
    (oset cell :events (ein:$notebook-events notebook))
    cell))

(defun ein:notebook-insert-cell-below (notebook type-or-cell base-cell)
  "Insert a cell just after BASE-CELL and return the new cell."
  (let ((cell (ein:notebook-maybe-new-cell notebook type-or-cell)))
    (when cell
      (cond
       ((= (ein:notebook-ncells notebook) 0)
        (ein:cell-enter-last cell))
       (base-cell
        (ein:cell-insert-below base-cell cell))
       (t (error (concat "`base-cell' is `nil' but ncells != 0.  "
                         "There is something wrong..."))))
      (ein:notebook-empty-undo-maybe)
      (ein:cell-goto cell)
      (setf (ein:$notebook-dirty notebook) t))
    cell))

(defun ein:notebook-insert-cell-below-command (&optional markdown)
  "Insert cell below.  Insert markdown cell instead of code cell
when the prefix argument is given."
  (interactive "P")
  (let ((cell (ein:notebook-get-current-cell)))
    ;; Do not use `ein:notebook-with-cell'.  When there is no cell,
    ;; This command should add the first cell.  So this clause must be
    ;; executed even if `cell' is `nil'.
    (ein:cell-goto
     (ein:notebook-insert-cell-below ein:notebook
                                     (if markdown 'markdown 'code)
                                     cell))))

(defun ein:notebook-insert-cell-above (notebook type-or-cell base-cell)
  (let ((cell (ein:notebook-maybe-new-cell notebook type-or-cell)))
    (when cell
      (cond
       ((< (ein:notebook-ncells notebook) 2)
        (ein:cell-enter-first cell))
       (base-cell
        (let ((prev-cell (ein:cell-prev base-cell)))
          (if prev-cell
              (ein:cell-insert-below prev-cell cell)
            (ein:cell-enter-first cell))))
       (t (error (concat "`base-cell' is `nil' but ncells > 1.  "
                         "There is something wrong..."))))
      (ein:notebook-empty-undo-maybe)
      (ein:cell-goto cell)
      (setf (ein:$notebook-dirty notebook) t))
    cell))

(defun ein:notebook-insert-cell-above-command (&optional markdown)
  "Insert cell above.  Insert markdown cell instead of code cell
when the prefix argument is given."
  (interactive "P")
  (let ((cell (ein:notebook-get-current-cell)))
    (ein:cell-goto
     (ein:notebook-insert-cell-above ein:notebook
                                     (if markdown 'markdown 'code)
                                     cell))))

(defun ein:notebook-toggle-cell-type ()
  "Toggle the cell type of the cell at point.
Use `ein:notebook-change-cell-type' to change the cell type
directly."
  (interactive)
  (ein:notebook-with-cell nil
    (let ((type (case (ein:$notebook-nbformat ein:notebook)
                  (2 (ein:case-equal (oref cell :cell-type)
                       (("code") "markdown")
                       (("markdown") "code")))
                  (3 (ein:case-equal (oref cell :cell-type)
                       (("code") "markdown")
                       (("markdown") "raw")
                       (("raw") "heading")
                       (("heading") "code"))))))
      (let ((new (ein:cell-convert-inplace cell type)))
        (when (ein:codecell-p new)
          (oset new :kernel (ein:$notebook-kernel ein:notebook)))
        (ein:notebook-empty-undo-maybe)
        (ein:cell-goto new)))))

(defun ein:notebook-change-cell-type ()
  "Change the cell type of the current cell.
Prompt will appear in the minibuffer."
  (interactive)
  (ein:notebook-with-cell nil
    (let* ((choices (case (ein:$notebook-nbformat ein:notebook)
                      (2 "cm")
                      (3 "cmr123456")))
           (key (ein:ask-choice-char
                 (format "Cell type [%s]: " choices) choices))
           (type (case key
                   (?c "code")
                   (?m "markdown")
                   (?r "raw")
                   (t "heading")))
           (level (when (equal type "heading")
                    (string-to-number (char-to-string key)))))
      (let ((new (ein:cell-convert-inplace cell type)))
        (when (ein:codecell-p new)
          (oset new :kernel (ein:$notebook-kernel ein:notebook)))
        (when level
          (ein:cell-change-level new type))
        (ein:notebook-empty-undo-maybe)))))

(defun ein:notebook-split-cell-at-point (&optional no-trim)
  "Split cell at current position. Newlines at the splitting
point will be removed. This can be omitted by giving a prefix
argument \(C-u)."
  (interactive "P")
  (ein:notebook-with-cell nil
    ;; FIXME: should I inhibit undo?
    (let* ((end (ein:cell-input-pos-max cell))
           (pos (point))
           (tail (buffer-substring pos end))
           (new (ein:notebook-insert-cell-below ein:notebook
                                                (oref cell :cell-type)
                                                cell)))
      (delete-region pos end)
      (unless no-trim
        (setq tail (ein:trim-left tail "\n"))
        (save-excursion
          (goto-char pos)
          (ignore-errors
            (while t
              (search-backward-regexp "\n\\=")
              (delete-char 1)))))
      (ein:cell-set-text new tail))))

(defun ein:notebook-merge-cell-command (&optional prev)
  "Merge next cell into current cell.
If prefix is given, merge current cell into previous cell."
  (interactive "P")
  (ein:notebook-with-cell nil
    (when prev
      (setq cell (ein:cell-prev cell))
      (unless cell (error "No previous cell"))
      (ein:cell-goto cell))
    (let* ((next-cell (ein:cell-next cell))
           (tail (ein:cell-get-text next-cell)))
      (ein:notebook-delete-cell ein:notebook next-cell)
      (save-excursion
        (goto-char (1- (ein:cell-location cell :input t)))
        (insert "\n" tail)))))


;;; Cell selection.

(defun ein:notebook-goto-input (ewoc-node up)
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ein:$node-data ewoc-data))
         (path (ein:$node-path ewoc-data))
         (element (nth 1 path)))
    (ein:aif
        (if (memql element (if up '(output footer) '(prompt)))
            cell
          (funcall (if up #'ein:cell-prev #'ein:cell-next) cell))
        (ein:cell-goto it)
      (ein:log 'warn "No %s input!" (if up "previous" "next")))))

(defun ein:notebook-goto-input-in-notebook-buffer (up)
  (ein:aif (ein:notebook-get-current-ewoc-node)
      (ein:notebook-goto-input it up)
    (ein:log 'warn "Not in notebook buffer!")))

(defun ein:notebook-goto-next-input-command ()
  (interactive)
  (ein:notebook-goto-input-in-notebook-buffer nil))

(defun ein:notebook-goto-prev-input-command ()
  (interactive)
  (ein:notebook-goto-input-in-notebook-buffer t))


;;; Cell movement

(defun ein:notebook-move-cell (notebook cell up)
  (ein:aif (if up (ein:cell-prev cell) (ein:cell-next cell))
      (let ((inhibit-read-only t)
            (pivot-cell it))
        (ein:cell-save-text cell)
        (ein:notebook-delete-cell ein:notebook cell)
        (funcall (if up
                     #'ein:notebook-insert-cell-above
                   #'ein:notebook-insert-cell-below)
                 notebook cell pivot-cell)
        (ein:cell-goto cell)
        (setf (ein:$notebook-dirty notebook) t))
    (ein:log 'warn "No %s cell" (if up "previous" "next"))))

(defun ein:notebook-move-cell-up-command ()
  (interactive)
  (ein:notebook-with-cell nil
    (ein:notebook-move-cell ein:notebook cell t)))

(defun ein:notebook-move-cell-down-command ()
  (interactive)
  (ein:notebook-with-cell nil
    (ein:notebook-move-cell ein:notebook cell nil)))


;;; Cell collapsing and output clearing

(defun ein:notebook-toggle-output (notebook cell)
  (ein:cell-toggle-output cell)
  (ein:notebook-empty-undo-maybe)
  (setf (ein:$notebook-dirty notebook) t))

(defun ein:notebook-toggle-output-command ()
  "Toggle the visibility of the output of the cell at point.
This does not alter the actual data stored in the cell."
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:notebook-toggle-output ein:notebook cell)))

(defun ein:notebook-set-collapsed-all (notebook collapsed)
  (mapc (lambda (c)
          (when (ein:codecell-p c) (ein:cell-set-collapsed c collapsed)))
        (ein:notebook-get-cells notebook))
  (ein:notebook-empty-undo-maybe)
  (setf (ein:$notebook-dirty notebook) t))

(defun ein:notebook-set-collapsed-all-command (&optional show)
  "Hide all cell output.  When prefix is given, show all cell output."
  (interactive "P")
  (ein:notebook-set-collapsed-all ein:notebook (not show)))

(defun ein:notebook-clear-output-command (&optional preserve-input-prompt)
  "Clear output from the current cell at point.
Do not clear input prompt when the prefix argument is given."
  (interactive "P")
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:cell-clear-output cell t t t)
    (unless preserve-input-prompt
      (ein:cell-set-input-prompt cell))
    (ein:notebook-empty-undo-maybe)))

(defun ein:notebook-clear-all-output-command (&optional preserve-input-prompt)
  "Clear output from all cells.
Do not clear input prompts when the prefix argument is given."
  (interactive "P")
  (if ein:notebook
    (loop for cell in (ein:notebook-get-cells ein:notebook)
          do (when (ein:codecell-p cell)
               (ein:cell-clear-output cell t t t)
               (unless preserve-input-prompt
                 (ein:cell-set-input-prompt cell))
               (ein:notebook-empty-undo-maybe)))
    (ein:log 'error "Not in notebook buffer!")))

(defun ein:notebook-show-in-shared-output ()
  "Show truncated code cell ouput in shared-output buffer.
See also `ein:cell-max-num-outputs' to how to truncate long
output."
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:shared-output-show-code-cell cell)))


;;; Traceback

(defun ein:notebook-view-traceback ()
  "Open traceback viewer for the traceback at point."
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (let ((tb-data
           (loop for out in (oref cell :outputs)
                 when (equal (plist-get out :output_type) "pyerr")
                 return (plist-get out :traceback))))
      (if tb-data
          (ein:tb-popup (ein:$notebook-traceback ein:notebook) tb-data)
        (ein:log 'info "No Traceback found for the current cell.")))))


;;; Kernel related things

(defun ein:notebook-start-kernel ()
  (let* ((base-url (concat ein:base-kernel-url "kernels"))
         (kernel (ein:kernel-new (ein:$notebook-url-or-port ein:notebook)
                                 base-url
                                 (ein:$notebook-events ein:notebook))))
    (setf (ein:$notebook-kernel ein:notebook) kernel)
    (ein:kernelinfo-init (ein:$kernel-kernelinfo kernel) (current-buffer))
    (ein:kernelinfo-setup-hooks kernel)
    (ein:pytools-setup-hooks kernel)
    (ein:kernel-start kernel
                      (ein:$notebook-notebook-id ein:notebook))
    (loop for cell in (ein:notebook-get-cells ein:notebook)
          do (when (ein:codecell-p cell)
               (ein:cell-set-kernel cell kernel)))))

(defun ein:notebook-restart-kernel (notebook)
  (ein:kernel-restart (ein:$notebook-kernel notebook)))

(defun ein:notebook-restart-kernel-command ()
  "Send request to the server to restart kernel."
  (interactive)
  (if ein:notebook
      (when (y-or-n-p "Really restart kernel? ")
        (ein:notebook-restart-kernel ein:notebook))
    (ein:log 'error "Not in notebook buffer!")))


(defun ein:notebook-get-current-ewoc-node (&optional pos)
  (ein:aand ein:notebook (ein:$notebook-ewoc it) (ewoc-locate it pos)))

(defun ein:notebook-get-nearest-cell-ewoc-node (&optional pos max cell-p)
  (ein:aif (ein:notebook-get-current-ewoc-node pos)
      (let ((ewoc-node it))
        ;; FIXME: can be optimized using the argument `max'
        (while (and ewoc-node
                    (not (and (ein:cell-ewoc-node-p ewoc-node)
                              (if cell-p
                                  (funcall cell-p
                                           (ein:cell-from-ewoc-node ewoc-node))
                                t))))
          (setq ewoc-node (ewoc-next (ein:$notebook-ewoc ein:notebook)
                                     ewoc-node)))
        ewoc-node)))

(defun ein:notebook-get-current-cell (&optional pos)
  (let ((cell (ein:cell-from-ewoc-node
               (ein:notebook-get-current-ewoc-node pos))))
    (when (ein:basecell-child-p cell) cell)))

(defun ein:notebook-get-cells-in-region (beg end)
  (ein:clip-list (ein:aand ein:notebook (ein:notebook-get-cells it))
                 (ein:notebook-get-current-cell beg)
                 (ein:notebook-get-current-cell end)))

(defun ein:notebook-get-cells-in-region-or-at-point ()
  (if (region-active-p)
      (ein:notebook-get-cells-in-region (region-beginning) (region-end))
    (list (ein:notebook-get-current-cell))))

(defun ein:notebook-execute-cell (notebook cell)
  (ein:kernel-if-ready (ein:$notebook-kernel notebook)
    (ein:cell-execute cell)
    (setf (ein:$notebook-dirty notebook) t)
    cell))

(defun ein:notebook-execute-current-cell ()
  "Execute cell at point."
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:notebook-execute-cell ein:notebook cell)))

(defun ein:notebook-execute-current-cell-and-goto-next ()
  "Execute cell at point if it is a code cell and move to the
next cell, or insert if none."
  (interactive)
  (ein:notebook-with-cell nil
    (when (ein:codecell-p cell)
      (ein:notebook-execute-cell ein:notebook cell))
    (ein:aif (ein:cell-next cell)
        (ein:cell-goto it)
      (ein:notebook-insert-cell-below ein:notebook 'code cell))))

(defun ein:notebook-execute-all-cell ()
  "Execute all cells in the current notebook buffer."
  (interactive)
  (if ein:notebook
    (loop for cell in (ein:notebook-get-cells ein:notebook)
          when (ein:codecell-p cell)
          do (ein:cell-execute cell))
    (ein:log 'error "Not in notebook buffer!")))

(defun ein:notebook-request-tool-tip (notebook cell func)
  (let ((kernel (ein:$notebook-kernel notebook))
        (callbacks
         (list :object_info_reply
               (cons #'ein:cell-finish-tooltip cell))))
    (ein:kernel-object-info-request kernel func callbacks)))

(defun ein:notebook-request-tool-tip-command ()
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:kernel-if-ready (ein:$notebook-kernel ein:notebook)
      (let ((func (ein:object-at-point)))
        (ein:notebook-request-tool-tip ein:notebook cell func)))))

(defun ein:notebook-request-help (notebook)
  (ein:kernel-if-ready (ein:$notebook-kernel notebook)
    (let ((func (ein:object-at-point)))
      (when func
        (ein:kernel-execute (ein:$notebook-kernel notebook)
                            (format "%s?" func) ; = code
                            nil                 ; = callbacks
                            ;; It looks like that magic command does
                            ;; not work in silent mode.
                            :silent nil)))))

(defun ein:notebook-request-help-command ()
  (interactive)
  (ein:notebook-request-help ein:notebook))

(defun ein:notebook-request-tool-tip-or-help-command (&optional pager)
  "Show the help for the object at point using tooltip.
When the prefix argument ``C-u`` is given, open the help in the
pager buffer.  You can explicitly specify the object by selecting it."
  (interactive "P")
  (if pager
      (ein:notebook-request-help-command)
    (ein:notebook-request-tool-tip-command)))

(defun ein:notebook-complete-at-point (notebook)
  (let ((kernel (ein:$notebook-kernel notebook))
        (callbacks
         (list :complete_reply
               (cons #'ein:completer-finish-completing nil))))
    (ein:kernel-complete kernel
                         (thing-at-point 'line)
                         (current-column)
                         callbacks)))

(defun ein:notebook-complete-command ()
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:kernel-if-ready (ein:$notebook-kernel ein:notebook)
      (ein:notebook-complete-at-point ein:notebook))))

(defun ein:notebook-kernel-interrupt-command ()
  "Interrupt the kernel.
This is equivalent to do ``C-c`` in the console program."
  (interactive)
  (ein:kernel-interrupt (ein:$notebook-kernel ein:notebook)))

(defun ein:notebook-kernel-kill-command ()
  (interactive)
  (when (y-or-n-p "Really kill kernel?")
    (ein:kernel-kill (ein:$notebook-kernel ein:notebook))))

;; misc kernel related

(defun ein:notebook-eval-string (code)
  "Evaluate a code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ein:shared-output-pop-to-buffer'."
  (interactive "sIP[y]: ")
  (let ((cell (ein:shared-output-get-cell))
        (kernel (ein:$notebook-kernel ein:notebook))
        (code (ein:trim-indent code)))
    (ein:cell-execute cell kernel code))
  (ein:log 'info "Code \"%s\" is sent to the kernel." code))

;; Followings are kernel related, but EIN specific

(defun ein:notebook-sync-directory (notebook)
  (ein:kernel-sync-directory (ein:$notebook-kernel notebook)
                             (ein:notebook-buffer notebook)))

(defun ein:notebook-sync-directory-command ()
  (interactive)
  (when ein:notebook (ein:notebook-sync-directory ein:notebook)))


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
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Enable nonsep for ewoc object (the last argument is non-nil).
    ;; This is for putting read-only text properties to the newlines.
    (setf (ein:$notebook-ewoc notebook)
          (ewoc-create 'ein:notebook-pp
                       (ein:propertize-read-only "\n")
                       nil t))
    (mapc (lambda (cell-data)
            (ein:cell-enter-last
             (ein:notebook-cell-from-json ein:notebook cell-data)))
          ;; Only handle 1 worksheet for now, as in notebook.js
          (plist-get (nth 0 (plist-get data :worksheets)) :cells))))

(defun ein:notebook-to-json (notebook)
  "Return json-ready alist."
  (let ((cells (mapcar (lambda (c)
                         (ein:cell-to-json
                          c (ein:notebook-discard-output-p notebook c)))
                       (ein:notebook-get-cells notebook))))
    `((worksheets . [((cells . ,(apply #'vector cells)))])
      (metadata . ,(ein:$notebook-metadata notebook)))))

(defun ein:notebook-save-notebook (notebook retry &optional callback cbarg)
  (let ((data (ein:notebook-to-json notebook)))
    (plist-put (cdr (assq 'metadata data))
               :name (ein:$notebook-notebook-name notebook))
    (push `(nbformat . ,(ein:$notebook-nbformat notebook)) data)
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
  (ein:notebook-save-notebook ein:notebook 0))

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
                      (let ((name (ein:$notebook-notebook-name ein:notebook)))
                        (unless (string-match "Untitled[0-9]+" name)
                          name)))))
  (ein:notebook-set-notebook-name ein:notebook name)
  (rename-buffer (ein:notebook-get-buffer-name ein:notebook))
  (ein:notebook-save-notebook
   ein:notebook 0
   (lambda (notebook &rest ignore)
     (with-current-buffer (ein:notebook-buffer notebook)
       (run-hooks 'ein:notebook-after-rename-hook)))
   ein:notebook))

(defun ein:notebook-rename-to-scratch-command (name)
  "Rename notebook based on `ein:scratch-notebook-name-template'
and save it immediately."
  (interactive
   (list (read-string "Rename notebook: "
                      (ein:scratch-notebook-name))))
  (ein:notebook-rename-command name))

(defun ein:notebook-kill-kernel-then-close-command ()
  "Kill kernel and then kill notebook buffer.
It does not kill buffer if killing kernel fails.  To close
notebook without killing kernel, just close the buffer as usual."
  (interactive)
  (when (ein:notebook-ask-before-kill-buffer)
    (ein:kernel-kill
     (ein:$notebook-kernel ein:notebook)
     (lambda (notebook)
       (let ((ein:notebook-kill-buffer-ask nil))
         (kill-buffer (ein:notebook-buffer notebook))))
     (list ein:notebook))))


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

(defun ein:notebook-mode ()
  (funcall (ein:notebook-choose-mode)))

(defvar ein:notebook-mode-map (make-sparse-keymap))

(let ((map ein:notebook-mode-map))
  (define-key map "\C-c\C-c" 'ein:notebook-execute-current-cell)
  (define-key map (kbd "M-RET")
    'ein:notebook-execute-current-cell-and-goto-next)
  (define-key map "\C-c\C-e" 'ein:notebook-toggle-output-command)
  (define-key map "\C-c\C-v" 'ein:notebook-set-collapsed-all-command)
  (define-key map "\C-c\C-l" 'ein:notebook-clear-output-command)
  (define-key map (kbd "C-c C-S-l") 'ein:notebook-clear-all-output-command)
  (define-key map (kbd "C-c C-;") 'ein:notebook-show-in-shared-output)
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
  (define-key map "\C-c\C-f" 'ein:notebook-request-tool-tip-or-help-command)
  (define-key map "\C-c\C-i" 'ein:notebook-complete-command)
  (define-key map "\C-c\C-x" 'ein:notebook-view-traceback)
  (define-key map "\C-c\C-r" 'ein:notebook-restart-kernel-command)
  (define-key map "\C-c\C-z" 'ein:notebook-kernel-interrupt-command)
  (define-key map "\C-c\C-q" 'ein:notebook-kill-kernel-then-close-command)
  (define-key map (kbd "C-:") 'ein:notebook-eval-string)
  (define-key map "\C-c\C-o" 'ein:notebook-console-open)
  (define-key map "\C-x\C-s" 'ein:notebook-save-notebook-command)
  (define-key map "\C-x\C-w" 'ein:notebook-rename-command)
  (define-key map "\M-."          'ein:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein:pytools-jump-back-command)
  map)

(define-derived-mode ein:notebook-plain-mode fundamental-mode "ein:notebook"
  "IPython notebook mode without fancy coloring."
  (font-lock-mode))

(define-derived-mode ein:notebook-python-mode python-mode "ein:python"
  "Use `python-mode' for whole notebook buffer.")

;; "Sync" `ein:notebook-plain-mode-map' with `ein:notebook-mode-map'.
;; This way, `ein:notebook-plain-mode-map' automatically changes when
;; `ein:notebook-mode-map' is changed.
(setcdr ein:notebook-plain-mode-map (cdr ein:notebook-mode-map))

(setcdr ein:notebook-python-mode-map (cdr ein:notebook-mode-map))

(defun ein:notebook-open-in-browser (&optional print)
  "Open current notebook in web browser.
When the prefix argument (``C-u``) is given, print page is opened.
Note that print page is not supported in IPython 0.12.1."
  (interactive "P")
  (let ((url (apply #'ein:url
                    (ein:$notebook-url-or-port ein:notebook)
                    (ein:$notebook-notebook-id ein:notebook)
                    (if print (list "print")))))
    (message "Opening %s in browser" url)
    (browse-url url)))

(defun ein:notebook-modified-p (&optional buffer)
  (unless buffer (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (ein:$notebook-p ein:notebook)
           (or (ein:$notebook-dirty ein:notebook)
               (buffer-modified-p))))))

(defcustom ein:notebook-kill-buffer-ask t
  "Whether EIN should ask before killing unsaved notebook buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

(defun ein:notebook-ask-before-kill-buffer ()
  "Return `nil' to prevent killing the notebook buffer.
Called via `kill-buffer-query-functions'."
  (not (and ein:notebook-kill-buffer-ask
            (ein:notebook-modified-p)
            (not (y-or-n-p "You have unsaved changes. Discard changes?")))))

(add-hook 'kill-buffer-query-functions 'ein:notebook-ask-before-kill-buffer)

(defun ein:notebook-force-kill-buffers (buffers)
  "Kill notebook BUFFERS without confirmation."
  (let ((ein:notebook-kill-buffer-ask nil))
    (mapc #'kill-buffer buffers)))

(defun ein:notebook-opened-buffers ()
  "Return list of opened buffers.
This function also cleans up closed buffers stores in
`ein:notebook-opened-map'."
  (let (buffers)
    (maphash (lambda (k b) (if (buffer-live-p b)
                               (push b buffers)
                             (remhash k ein:notebook-opened-map)))
             ein:notebook-opened-map)
    buffers))

(defun ein:notebook-ask-before-kill-emacs ()
  "Return `nil' to prevent killing Emacs when unsaved notebook exists.
Called via `kill-emacs-query-functions'."
  (let ((unsaved (ein:filter #'ein:notebook-modified-p
                             (ein:notebook-opened-buffers))))
    (if (null unsaved)
        t
      (let ((answer
             (y-or-n-p
              (format "You have %s unsaved notebook(s). Discard changes?"
                      (length unsaved)))))
        ;; kill all unsaved buffers forcefully
        (when answer
          (ein:notebook-force-kill-buffers unsaved))
        answer))))

(add-hook 'kill-emacs-query-functions 'ein:notebook-ask-before-kill-emacs)

(defun ein:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  (when (ein:$notebook-p ein:notebook)
    (ein:notebook-del ein:notebook)))

(defun ein:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ein:notebook-kill-buffer-callback))

(add-hook 'ein:notebook-plain-mode-hook 'ein:notebook-setup-kill-buffer-hook)

(defun ein:notebook-kill-all-buffers ()
  "Close all opened notebooks.  If there are unsaved notebooks
prompt asking whether to kill all of them or not will appears."
  (interactive)
  (let ((buffers (ein:notebook-opened-buffers)))
    (if buffers
        (if (y-or-n-p
             (format (concat "You have %s unsaved notebook(s). "
                             "Really kill all of them?")
                     (length buffers)))
            (progn (ein:log 'info "Killing all notebook buffers...")
                   (ein:notebook-force-kill-buffers buffers)
                   (ein:log 'info "Killing all notebook buffers... Done!"))
          (ein:log 'info "Canceled to kill all notebooks."))
      (ein:log 'info "No opened notebooks."))))


;;; Console integration

(defcustom ein:notebook-console-security-dir ""
  "Security directory setting.

Following types are valid:

string
    Use this value as a path to security directory.
    Handy when you have only one IPython server.
alist
    An alist whose element is \"(URL-OR-PORT . DIR)\".
    Key (URL-OR-PORT) can be string (URL), integer (port), or
    `default' (symbol).  The value of `default' is used when
    other key does not much.  Normally you should have this
    entry.
function
    Called with an argument URL-OR-PORT (integer or string).
    You can have complex setting using this."
  :type '(choice
          (string :tag "Security directory"
                  "~/.config/ipython/profile_nbserver/security/")
          (alist :tag "Security directory mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "Security directory"))
          (function :tag "Security directory getter"
                    (lambda (url-or-port)
                      (format "~/.config/ipython/profile_%s/security/"
                              url-or-port))))
  :group 'ein)

(defcustom ein:notebook-console-executable (executable-find "ipython")
  "IPython executable used for console.

Example: ``\"/user/bin/ipython\"``.
Types same as `ein:notebook-console-security-dir' are valid."
  :type '(choice
          (string :tag "IPython executable" "/user/bin/ipython")
          (alist :tag "IPython executable mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "IPython executable"
                                     "/user/bin/ipython"))
          (function :tag "IPython executable getter"
                    (lambda (url-or-port) (executable-find "ipython"))))
  :group 'ein)

(defcustom ein:notebook-console-args "--profile nbserver"
  "Additional argument when using console.

Example: ``\"--ssh HOSTNAME\"``.
Types same as `ein:notebook-console-security-dir' are valid."
  :type '(choice
          (string :tag "Arguments to IPython"
                  "--profile nbserver --ssh HOSTNAME")
          (alist :tag "Arguments mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "Arguments to IPython"
                                     "--profile nbserver --ssh HOSTNAME"))
          (function :tag "Additional arguments getter"
                    (lambda (url-or-port)
                      (format "--ssh %s" url-or-port))))
  :group 'ein)

(defun ein:notebook-console-security-dir-get (notebook)
  (let ((dir (ein:choose-setting 'ein:notebook-console-security-dir
                                 (ein:$notebook-url-or-port notebook))))
    (if (equal dir "")
        dir
    (file-name-as-directory (expand-file-name dir)))))

(defun ein:notebook-console-executable-get (notebook)
  (ein:choose-setting 'ein:notebook-console-executable
                      (ein:$notebook-url-or-port notebook)))

(defun ein:notebook-console-args-get (notebook)
  (ein:choose-setting 'ein:notebook-console-args
                      (ein:$notebook-url-or-port notebook)))

;; `Fabian Gallina's python.el`_
(declare-function run-python "python")
(declare-function python-shell-parse-command "python")
(declare-function python-shell-switch-to-shell "python")

(defun ein:notebook-console-open ()
  "Open IPython console.
To use this function, `ein:notebook-console-security-dir' and
`ein:notebook-console-args' must be set properly.
This function requires `Fabian Gallina's python.el`_ for now;
It should be possible to support python-mode.el.  Patches are welcome!

.. _`Fabian Gallina's python.el`: https://github.com/fgallina/python.el"
  ;; FIXME: use %connect_info to get connection file, then I can get
  ;; rid of `ein:notebook-console-security-dir'.
  (interactive)
  (unless ein:notebook (error "Not in notebook buffer!"))
  (if (fboundp 'python-shell-switch-to-shell)
      (let* ((dir (ein:notebook-console-security-dir-get ein:notebook))
             (kid (ein:$kernel-kernel-id
                   (ein:$notebook-kernel ein:notebook)))
             (ipy (ein:notebook-console-executable-get ein:notebook))
             (args (ein:notebook-console-args-get ein:notebook))
             ;; python.el settings:
             (python-shell-setup-codes nil)
             (python-shell-interpreter
              (format "python %s console --existing %skernel-%s.json %s"
                      ipy dir kid args))
             ;; python.el makes dedicated process when
             ;; `buffer-file-name' has some value.
             (buffer-file-name (buffer-name)))
        (run-python t (python-shell-parse-command))
        (python-shell-switch-to-shell))
    (ein:log 'warn "python.el is not loaded!")))

(provide 'ein-notebook)

;;; ein-notebook.el ends here
