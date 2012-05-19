;;; ein-notebook.el --- Notebook module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

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

;;

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
(require 'ein-kill-ring)

(defvar ein:notebook-pager-buffer-name-template "*ein:pager %s/%s*")
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

`ein:$notebook-msg-cell-map' : hash
  Hash to hold map from msg-id to cell-id.

`ein:$notebook-metadata' : plist
  Notebook meta data (e.g., notebook name).

`ein:$notebook-name' : string
  Notebook name.

`ein:$notebook-nbformat' : integer
  Notebook file format version."
  url-or-port
  notebook-id
  data
  ewoc
  kernel
  pager
  dirty
  msg-cell-map
  metadata
  notebook-name
  nbformat
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

(defun ein:notebook-new (url-or-port notebook-id &rest args)
  (let ((notebook (apply #'make-ein:$notebook
                         :url-or-port url-or-port
                         :notebook-id notebook-id
                         :msg-cell-map (make-hash-table :test 'equal)
                         args)))
    notebook))

(defun ein:notebook-init (notebook data)
  "Initialize NOTEBOOK with DATA from the server."
  (setf (ein:$notebook-data notebook) data)
  (let* ((metadata (plist-get data :metadata))
         (notebook-name (plist-get metadata :name)))
    (setf (ein:$notebook-metadata notebook) metadata)
    (setf (ein:$notebook-nbformat notebook) (plist-get data :nbformat))
    (setf (ein:$notebook-notebook-name notebook) notebook-name))
  (setf (ein:$notebook-pager notebook)
        (ein:pager-new
         (format ein:notebook-pager-buffer-name-template
                 (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-name notebook)))))

(defun ein:notebook-del (notebook)
  "Destructor for `ein:$notebook'."
  (ein:kernel-del (ein:$notebook-kernel notebook)))

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

(defun ein:notebook-open (url-or-port notebook-id)
  "Open notebook."
  (let* ((key (list url-or-port notebook-id))
         (buffer (gethash key ein:notebook-opened-map)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (pop-to-buffer (current-buffer))
          ein:notebook)
      (remhash key ein:notebook-opened-map)
      (ein:notebook-request-open url-or-port notebook-id))))

(defun ein:notebook-request-open (url-or-port notebook-id)
  "Request notebook of NOTEBOOK-ID to the server at URL-OR-PORT.
Return `ein:$notebook' instance.  Notebook may not be ready at
the time of execution."
  (let ((url (ein:notebook-url-from-url-and-id url-or-port notebook-id))
        (notebook (ein:notebook-new url-or-port notebook-id)))
    (ein:log 'debug "Opening notebook at %s" url)
    (url-retrieve url
                  #'ein:notebook-url-retrieve-callback
                  (list notebook))
    notebook))

(defun ein:notebook-url-retrieve-callback (status notebook)
  (ein:log 'debug "URL-RETRIEVE nodtebook-id = %S, status = %S"
           (ein:$notebook-notebook-id notebook)
           status)
  (let ((data (ein:json-read))
        (notebook-id (ein:$notebook-notebook-id notebook)))
    (kill-buffer (current-buffer))
    (ein:notebook-init notebook data)
    (with-current-buffer (ein:notebook-get-buffer notebook)
      (ein:log-setup (ein:$notebook-notebook-id notebook))
      (setq ein:notebook notebook)
      (ein:notebook-render)
      (set-buffer-modified-p nil)
      (puthash (list (ein:$notebook-url-or-port ein:notebook) notebook-id)
               (current-buffer)
               ein:notebook-opened-map)
      (pop-to-buffer (current-buffer)))))

(defun ein:notebook-render ()
  "(Re-)Render the notebook."
  (interactive)
  (assert ein:notebook)  ; make sure in a notebook buffer
  (ein:notebook-from-json ein:notebook (ein:$notebook-data ein:notebook))
  (setq buffer-undo-list nil)  ; clear undo history
  (ein:notebook-mode)
  (ein:notebook-start-kernel)
  (ein:log 'info "Notebook %s is ready"
           (ein:$notebook-notebook-name ein:notebook)))

(defun ein:notebook-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))


;;; Cell indexing, retrieval, etc.

(defun ein:notebook-cell-from-json (notebook data &rest args)
  (apply #'ein:cell-from-json
         data :ewoc (ein:$notebook-ewoc notebook) args))

(defun ein:notebook-cell-from-type (notebook type &rest args)
  (apply #'ein:cell-from-type
         (format "%s" type) :ewoc (ein:$notebook-ewoc notebook) args))

(defun ein:notebook-get-cells (notebook)
  (let* ((ewoc (ein:$notebook-ewoc notebook))
         (nodes (ewoc-collect ewoc (lambda (n) (ein:cell-node-p n 'prompt)))))
    (mapcar #'ein:$node-data nodes)))

(defun ein:notebook-cell-for-msg (notebook msg-id)
  (let* ((msg-cell-map (ein:$notebook-msg-cell-map notebook))
         (cell-id (gethash msg-id msg-cell-map)))
    (when cell-id
      (loop for cell in (ein:notebook-get-cells notebook)
            when (equal (oref cell :cell-id) cell-id)
            return cell))))

(defun ein:notebook-ncells (notebook)
  (length (ein:notebook-get-cells notebook)))


;; Insertion and deletion of cells

(defun ein:notebook-delete-cell (notebook cell)
  (let ((inhibit-read-only t))
    (apply #'ewoc-delete
           (ein:$notebook-ewoc notebook)
           (ein:cell-all-element cell)))
  (setf (ein:$notebook-dirty notebook) t))

(defun ein:notebook-delete-cell-command ()
  (interactive)
  (ein:notebook-with-cell nil
    (ein:notebook-delete-cell ein:notebook cell)
    (ein:aif (ein:notebook-get-current-cell) (ein:cell-goto it))))

(defun ein:notebook-kill-cell-command ()
  (interactive)
  (ein:notebook-with-cell nil
    (ein:cell-save-text cell)
    (ein:notebook-delete-cell ein:notebook cell)
    (ein:kill-new (ein:cell-deactivate cell))
    (ein:aif (ein:notebook-get-current-cell) (ein:cell-goto it))))

(defun ein:notebook-copy-cell-command ()
  (interactive)
  (ein:notebook-with-cell nil
    (ein:kill-new (ein:cell-deactivate (ein:cell-copy cell)))
    (ein:aif (ein:notebook-get-current-cell) (ein:cell-goto it))))

(defun ein:notebook-yank-cell-command (&optional arg)
  (interactive "*P")
  ;; Do not use `ein:notebook-with-cell'.
  ;; `ein:notebook-insert-cell-below' handles empty cell.
  (let* ((cell (ein:notebook-get-current-cell))
         (killed (ein:current-kill (cond
                                    ((listp arg) 0)
                                    ((eq arg '-) -2)
                                    (t (1- arg)))))
         (clone (ein:cell-copy killed)))
    ;; Cell can be from another buffer, so reset `ewoc'.
    (oset clone :ewoc (ein:$notebook-ewoc ein:notebook))
    (ein:notebook-insert-cell-below ein:notebook clone cell)
    (ein:cell-goto clone)))

(defun ein:notebook-maybe-new-cell (notebook type-or-cell)
  "Return TYPE-OR-CELL as-is if it is a cell, otherwise return a new cell."
  (if (ein:basecell-child-p type-or-cell)
      type-or-cell
    (ein:notebook-cell-from-type notebook type-or-cell)))

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
  (interactive)
  (ein:notebook-with-cell nil
    (let ((type (ein:case-equal (oref cell :cell-type)
                  (("code") "markdown")
                  (("markdown") "code"))))
      (ein:cell-convert-inplace cell type)
      (ein:cell-goto cell))))

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
           (tail (ein:cell-get-text next-cell))
           (buffer-undo-list t))        ; disable undo recording
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
  (setf (ein:$notebook-dirty notebook) t))

(defun ein:notebook-toggle-output-command ()
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:notebook-toggle-output ein:notebook cell)))

(defun ein:notebook-set-collapsed-all (notebook collapsed)
  (mapc (lambda (c)
          (when (ein:codecell-p c) (ein:cell-set-collapsed c collapsed)))
        (ein:notebook-get-cells notebook))
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
      (ein:cell-set-input-prompt cell))))

(defun ein:notebook-clear-all-output-command (&optional preserve-input-prompt)
  "Clear output from all cells.
Do not clear input prompts when the prefix argument is given."
  (interactive "P")
  (if ein:notebook
    (loop for cell in (ein:notebook-get-cells ein:notebook)
          do (when (ein:codecell-p cell)
               (ein:cell-clear-output cell t t t)
               (unless preserve-input-prompt
                 (ein:cell-set-input-prompt cell))))
    (ein:log 'error "Not in notebook buffer!")))


;;; Kernel related things

(defun ein:notebook-start-kernel ()
  (let ((kernel (ein:kernel-new (ein:$notebook-url-or-port ein:notebook))))
    (setf (ein:$notebook-kernel ein:notebook) kernel)
    (ein:kernel-start kernel
                      (ein:$notebook-notebook-id ein:notebook)
                      #'ein:notebook-kernel-started
                      (list ein:notebook))))

(defun ein:notebook-restart-kernel (notebook)
  (ein:kernel-restart (ein:$notebook-kernel notebook)
                      #'ein:notebook-kernel-started
                      (list ein:notebook)))

(defun ein:notebook-restart-kernel-command ()
  (interactive)
  (if ein:notebook
      (when (y-or-n-p "Really restart kernel? ")
        (ein:notebook-restart-kernel ein:notebook))
    (ein:log 'error "Not in notebook buffer!")))

(defun ein:notebook-kernel-started (notebook)
  (let* ((kernel (ein:$notebook-kernel notebook))
         (shell-channel (ein:$kernel-shell-channel kernel))
         (iopub-channel (ein:$kernel-iopub-channel kernel)))
    (lexical-let ((notebook notebook))
      (setf (ein:$websocket-onmessage shell-channel)
            (lambda (packet)
              (ein:notebook-handle-shell-reply notebook packet)))
      (setf (ein:$websocket-onmessage iopub-channel)
            (lambda (packet)
              (ein:notebook-handle-iopub-reply notebook packet)))))
  (ein:log 'info "IPython kernel is started"))

(defun ein:notebook-handle-shell-reply (notebook packet)
  (destructuring-bind
      (&key header content msg_type parent_header &allow-other-keys)
      (ein:json-read-from-string packet)
    (let ((cell (ein:notebook-cell-for-msg
                 notebook
                 (plist-get parent_header :msg_id))))
      (cond
       ((equal msg_type "execute_reply")
        (ein:cell-set-input-prompt cell (plist-get content :execution_count))
        (ein:cell-running-set cell nil)
        (setf (ein:$notebook-dirty notebook) t))
       ((equal msg_type "complete_reply")
        (ein:completer-finish-completing (plist-get content :matched_text)
                                         (plist-get content :matches)))
       ((equal msg_type "object_info_reply")
        (when (plist-get content :found)
          (ein:cell-finish-tooltip cell content)))
       (t (ein:log 'info "unknown reply: %s" msg_type)))
      (when (plist-member content :payload)
        (ein:notebook-handle-payload notebook cell
                                     (plist-get content :payload))))))

(defun ein:notebook-handle-payload (notebook cell payload)
  (loop for p in payload
        for text = (plist-get p :text)
        for source = (plist-get p :source)
        if (equal source "IPython.zmq.page.page")
        when (not (equal (ein:trim text) ""))
        do (let ((pager (ein:$notebook-pager notebook)))
             (ein:pager-clear pager)
             (ein:pager-expand pager)
             (ein:pager-append-text pager text))
        else if
        (equal source "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input")
        do (let ((new-cell (ein:notebook-insert-cell-below
                            notebook 'code cell)))
             (ein:cell-set-text new-cell text)
             (setf (ein:$notebook-dirty notebook) t))))

(defun ein:notebook-handle-iopub-reply (notebook packet)
  (destructuring-bind
      (&key content msg_type parent_header &allow-other-keys)
      (ein:json-read-from-string packet)
    (let ((cell (ein:notebook-cell-for-msg
                 notebook
                 (plist-get parent_header :msg_id))))
      (if (and (not (equal msg_type "status")) (null cell))
          (ein:log 'verbose "Got message not from this notebook.")
        (ein:log 'debug "handle-iopub-reply: msg_type = %s" msg_type)
        (ein:case-equal msg_type
          (("stream" "display_data" "pyout" "pyerr")
           (ein:notebook-handle-output notebook cell msg_type content))
          (("status")
           (ein:case-equal (plist-get content :execution_state)
             (("busy")
              (ein:events-trigger 'status_busy.Kernel))
             (("idle")
              (ein:events-trigger 'status_idle.Kernel))
             (("dead"))
             (ein:notebook-handle-status-dead notebook)))
          (("clear_output")
           (ein:cell-clear-output cell
                                  (plist-get content :stdout)
                                  (plist-get content :stderr)
                                  (plist-get content :other))))))))

(defun ein:notebook-handle-status-dead (notebook)
  ;; FIXME: do something more useful
  (ein:log 'info "The kernel has died."))

(defun ein:notebook-handle-output (notebook cell msg-type content)
  (let* ((json (list :output_type msg-type)))
    (ein:case-equal msg-type
      (("stream")
       (plist-put json :text (plist-get content :data))
       (plist-put json :stream (plist-get content :name)))
      (("display_data" "pyout")
       (when (equal msg-type "pyout")
         (plist-put json :prompt_number (plist-get content :execution_count)))
       (setq json (ein:notebook-convert-mime-types
                   json (plist-get content :data))))
      (("pyerr")
       (plist-put json :ename (plist-get content :ename))
       (plist-put json :evalue (plist-get content :evalue))
       (plist-put json :traceback (plist-get content :traceback))))
    (ein:cell-append-output cell json t)
    (setf (ein:$notebook-dirty notebook) t)))

(defun ein:notebook-convert-mime-types (json data)
  (loop for (prop . mime) in '((:text       . :text/plain)
                               (:html       . :text/html)
                               (:png        . :image/png)
                               (:jpeg       . :image/jpeg)
                               (:latex      . :text/latex)
                               (:json       . :application/json)
                               (:javascript . :application/javascript))
        when (plist-member data mime)
        do (plist-put json prop (plist-get data mime)))
  json)


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

(defun ein:notebook-execute-code (notebook cell code)
  (let* ((msg-id (ein:kernel-execute (ein:$notebook-kernel notebook) code)))
    (puthash msg-id (oref cell :cell-id)
             (ein:$notebook-msg-cell-map notebook))))

(defun ein:notebook-execute-cell (notebook cell)
  "Execute code cell CELL in NOTEBOOK."
  (ein:cell-clear-output cell t t t)
  (ein:cell-set-input-prompt cell "*")
  (ein:cell-running-set cell t)
  (ein:notebook-execute-code notebook cell (ein:cell-get-text cell)))

(defun ein:notebook-execute-current-cell ()
  "Execute cell at point."
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:kernel-if-ready (ein:$notebook-kernel ein:notebook)
      (ein:notebook-execute-cell ein:notebook cell)
      (setf (ein:$notebook-dirty ein:notebook) t)
      cell)))

(defun ein:notebook-execute-current-cell-and-goto-next ()
  "Execute cell at point and move to the next cell, or insert if none."
  (interactive)
  (let ((cell (ein:notebook-execute-current-cell)))
    (when cell
      (ein:aif (ein:cell-next cell)
          (ein:cell-goto it)
        (ein:notebook-insert-cell-below ein:notebook 'code cell)))))

(defun ein:notebook-execute-all-cell ()
  "Execute all cells in the current notebook buffer."
  (interactive)
  (if ein:notebook
    (loop for cell in (ein:notebook-get-cells ein:notebook)
          when (ein:codecell-p cell)
          do (ein:notebook-execute-cell ein:notebook cell))
    (ein:log 'error "Not in notebook buffer!")))

(defun ein:notebook-request-tool-tip (notebook cell func)
  (let ((msg-id (ein:kernel-object-info-request
                 (ein:$notebook-kernel notebook) func)))
    (when msg-id
      (puthash msg-id (oref cell :cell-id)
               (ein:$notebook-msg-cell-map notebook)))))

(defun ein:notebook-request-tool-tip-command ()
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:kernel-if-ready (ein:$notebook-kernel ein:notebook)
      (let ((func (ein:object-at-point)))
        (ein:notebook-request-tool-tip ein:notebook cell func)))))

(defun ein:notebook-request-help-command ()
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:kernel-if-ready (ein:$notebook-kernel ein:notebook)
      (let ((func (ein:object-at-point)))
        (when func
          (ein:notebook-execute-code
           ein:notebook cell (format "%s?" func)))))))

(defun ein:notebook-request-tool-tip-or-help-command (&optional pager)
  (interactive "P")
  (if pager
      (ein:notebook-request-help-command)
    (ein:notebook-request-tool-tip-command)))

(defun ein:notebook-complete-cell (notebook cell line-string rel-pos)
  (let ((msg-id (ein:kernel-complete (ein:$notebook-kernel notebook)
                                     line-string rel-pos)))
    (puthash msg-id (oref cell :cell-id)
             (ein:$notebook-msg-cell-map notebook))))

(defun ein:notebook-complete-cell-command ()
  (interactive)
  (ein:notebook-with-cell #'ein:codecell-p
    (ein:kernel-if-ready (ein:$notebook-kernel ein:notebook)
      (ein:notebook-complete-cell ein:notebook
                                  cell
                                  (thing-at-point 'line)
                                  (current-column)))))

(defun ein:notebook-kernel-interrupt-command ()
  (interactive)
  (ein:kernel-interrupt (ein:$notebook-kernel ein:notebook)))

(defun ein:notebook-kernel-kill-command ()
  (interactive)
  (when (y-or-n-p "Really kill kernel?")
    (ein:kernel-kill (ein:$notebook-kernel ein:notebook))))


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
                       (ein:propertize-read-only "IPython notebook\n\n")
                       nil t))
    (mapc (lambda (cell-data)
            (ein:cell-enter-last
             (ein:notebook-cell-from-json ein:notebook cell-data)))
          ;; Only handle 1 worksheet for now, as in notebook.js
          (plist-get (nth 0 (plist-get data :worksheets)) :cells))))

(defun ein:notebook-to-json (notebook)
  "Return json-ready alist."
  `((worksheets
     . [((cells
          . ,(apply #'vector
                    (mapcar #'ein:cell-to-json
                            (ein:notebook-get-cells notebook)))))])
    (metadata . ,(ein:$notebook-metadata notebook))))

(defun ein:notebook-save-notebook (notebook retry)
  (let ((data (ein:notebook-to-json notebook)))
    (plist-put (cdr (assq 'metadata data))
               :name (ein:$notebook-notebook-name notebook))
    (push `(nbformat . ,(ein:$notebook-nbformat notebook)) data)
    (ein:events-trigger 'notebook_saving.Notebook)
    (let ((url (ein:url-no-cache (ein:notebook-url notebook)))
          (url-request-method "PUT")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data (json-encode data)))
      (ein:log 'debug "URL-RETRIEVE url = %s" url)
      (ein:log 'debug "URL-REQUEST-DATA = %s" url-request-data)
      (url-retrieve
       url
       #'ein:notebook-save-notebook-callback
       (list notebook retry)))))

(defun ein:notebook-save-notebook-command ()
  (interactive)
  (ein:notebook-save-notebook ein:notebook 0))

(defun ein:notebook-save-notebook-callback (status notebook retry)
  (declare (special url-http-response-status
                    url-http-method))
  (ein:log 'debug "SAVE-NOTEBOOK-CALLBACK nodtebook-id = %S, status = %S"
           (ein:$notebook-notebook-id notebook)
           status)
  (ein:log 'debug "url-http-response-status = %s" url-http-response-status)
  (ein:log 'debug "url-request-method = %s" url-request-method)
  (ein:log 'debug "url-http-method = %s" (when (boundp 'url-http-method)
                                           url-http-method))
  (ein:log 'debug "(buffer-string) = \n%s" (buffer-string))
  (let ((response url-http-response-status))
    ;; ^-- "save" local variable before killing buffer.
    (kill-buffer (current-buffer))
    (with-current-buffer (ewoc-buffer (ein:$notebook-ewoc notebook))
      (ein:aif (plist-get status :error)
          (progn
            (ein:log 'debug "ERROR CODE = %S" it)
            (ein:notebook-save-notebook-error notebook status))
        ;; IPython server returns 204 only when the notebook URL is
        ;; accessed via PUT or DELETE.  As it seems Emacs failed to
        ;; choose PUT method every two times, let's check the response
        ;; here and fail when 204 is not returned.
        (if (eq response 204)
            (ein:notebook-save-notebook-success notebook status)
          (if (< retry ein:notebook-save-retry-max)
              (progn
                (ein:log 'info "Retry saving... Next count: %s" (1+ retry))
                (ein:notebook-save-notebook notebook (1+ retry)))
            (ein:notebook-save-notebook-error notebook status)
            (ein:log 'info
              "Status code (=%s) is not 204 and retry exceeds limit (=%s)."
              response ein:notebook-save-retry-max)))))))

(defun ein:notebook-save-notebook-success (notebook status)
  (setf (ein:$notebook-dirty notebook))
  (set-buffer-modified-p nil)
  (ein:events-trigger 'notebook_saved.Notebook))

(defun ein:notebook-save-notebook-error (notebook status)
  (ein:events-trigger 'notebook_save_failed.Notebook))

(defun ein:notebook-rename-command (name)
  "Rename current notebook and save it immediately.

NAME is any non-empty string that does not contain '/' or '\\'."
  (interactive
   (list (read-string "Rename notebook: "
                      (ein:$notebook-notebook-name ein:notebook))))
  (ein:notebook-set-notebook-name ein:notebook name)
  (rename-buffer (ein:notebook-get-buffer-name ein:notebook))
  (ein:notebook-save-notebook ein:notebook 0))


;;; Notebook mode

(defvar ein:notebook-modes
  '(ein:notebook-mumamo-mode ein:notebook-plain-mode))

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

(defvar ein:notebook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'ein:notebook-execute-current-cell)
    (define-key map (kbd "M-RET")
      'ein:notebook-execute-current-cell-and-goto-next)
    (define-key map "\C-c\C-e" 'ein:notebook-toggle-output-command)
    (define-key map "\C-c\C-v" 'ein:notebook-set-collapsed-all-command)
    (define-key map "\C-c\C-l" 'ein:notebook-clear-output-command)
    (define-key map (kbd "C-c C-S-l") 'ein:notebook-clear-all-output-command)
    (define-key map "\C-c\C-d" 'ein:notebook-delete-cell-command)
    (define-key map "\C-c\C-k" 'ein:notebook-kill-cell-command)
    (define-key map "\C-c\M-w" 'ein:notebook-copy-cell-command)
    (define-key map "\C-c\C-y" 'ein:notebook-yank-cell-command)
    (define-key map "\C-c\C-a" 'ein:notebook-insert-cell-above-command)
    (define-key map "\C-c\C-b" 'ein:notebook-insert-cell-below-command)
    (define-key map "\C-c\C-t" 'ein:notebook-toggle-cell-type)
    (define-key map "\C-c\C-s" 'ein:notebook-split-cell-at-point)
    (define-key map "\C-c\C-m" 'ein:notebook-merge-cell-command)
    (define-key map "\C-c\C-n" 'ein:notebook-goto-next-input-command)
    (define-key map "\C-c\C-p" 'ein:notebook-goto-prev-input-command)
    (define-key map (kbd "C-c <up>") 'ein:notebook-move-cell-up-command)
    (define-key map (kbd "C-c <down>") 'ein:notebook-move-cell-down-command)
    (define-key map "\C-c\C-f" 'ein:notebook-request-tool-tip-or-help-command)
    (define-key map "\C-c\C-i" 'ein:notebook-complete-cell-command)
    (define-key map "\C-c\C-z" 'ein:notebook-kernel-interrupt-command)
    (define-key map "\C-c\C-q" 'ein:notebook-kernel-kill-command)
    (define-key map "\C-c\C-o" 'ein:notebook-console-open)
    (define-key map "\C-x\C-s" 'ein:notebook-save-notebook-command)
    (define-key map "\C-x\C-w" 'ein:notebook-rename-command)
    map))

(define-derived-mode ein:notebook-plain-mode fundamental-mode "ein:notebook"
  "IPython notebook command without fancy coloring."
  (font-lock-mode))

;; "Sync" `ein:notebook-plain-mode-map' with `ein:notebook-mode-map'.
;; This way, `ein:notebook-plain-mode-map' automatically changes when
;; `ein:notebook-mode-map' is changed.
(setcdr ein:notebook-plain-mode-map (cdr ein:notebook-mode-map))

(defun ein:notebook-open-in-browser ()
  "Open current notebook in web browser."
  (interactive)
  (let ((url (ein:url (ein:$notebook-url-or-port ein:notebook)
                      (ein:$notebook-notebook-id ein:notebook))))
    (message "Opening %s in browser" url)
    (browse-url url)))

(defun ein:notebook-modified-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (and (ein:$notebook-p ein:notebook)
         (or (ein:$notebook-dirty ein:notebook)
             (buffer-modified-p)))))

(defun ein:notebook-ask-before-kill-buffer ()
  "Return `nil' to prevent killing the notebook buffer.
Called via `kill-buffer-query-functions'."
  (not (and (ein:notebook-modified-p)
            (not (y-or-n-p "You have unsaved changes. Discard changes?")))))

(add-hook 'kill-buffer-query-functions 'ein:notebook-ask-before-kill-buffer)

(defun ein:notebook-ask-before-kill-emacs ()
  "Return `nil' to prevent killing Emacs when unsaved notebook exists.
Called via `kill-emacs-query-functions'."
  (let (unsaved)
    (maphash
     (lambda (key buffer)
       (when (ein:notebook-modified-p buffer)
         (push buffer unsaved)))
     ein:notebook-opened-map)
    (if (null unsaved)
        t
      (not (y-or-n-p
            (format "You have %s unsaved notebook(s). Discard changes?"
                    (length unsaved)))))))

(add-hook 'kill-emacs-query-functions 'ein:notebook-ask-before-kill-emacs)

(defun ein:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  (when (ein:$notebook-p ein:notebook)
    (ein:notebook-del ein:notebook)))

(defun ein:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ein:notebook-kill-buffer-callback))

(add-hook 'ein:notebook-plain-mode-hook 'ein:notebook-setup-kill-buffer-hook)


;;; Console integration

(defcustom ein:notebook-console-security-dir ""
  "Security directory setting.

Following type is accepted:
string   : Use this value as a path to security directory.
           Handy when you have only one IPython server.
alist    : An alist whose element is \"(URL-OR-PORT . DIR)\".
           Key (URL-OR-PORT) can be string (URL), integer (port), or
           `default' (symbol).  The value of `default' is used when
           other key does not much.  Normally you should have this
           entry.
function : Called with an argument URL-OR-PORT (integer or string).
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

(defcustom ein:notebook-console-args "--profile nbserver"
  "Additional argument when using console.

Example: \"--ssh HOSTNAME\"
Types same as `ein:notebook-console-security-dir' are accepted."
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

(defun ein:notebook-console-args-get (notebook)
  (ein:choose-setting 'ein:notebook-console-args
                      (ein:$notebook-url-or-port notebook)))

(defun ein:notebook-console-open ()
  "Open IPython console.
To use this function, `ein:notebook-console-security-dir' and
`ein:notebook-console-args' must be set properly.
This function requires Fabian Gallina's python.el for now:
https://github.com/fgallina/python.el"
  ;; FIXME: use %connect_info to get connection file, then I can get
  ;; rid of `ein:notebook-console-security-dir'.
  (interactive)
  (unless ein:notebook (error "Not in notebook buffer!"))
  (if (fboundp 'python-shell-switch-to-shell)
      (progn
        (let* ((dir (ein:notebook-console-security-dir-get ein:notebook))
               (kid (ein:$kernel-kernel-id
                     (ein:$notebook-kernel ein:notebook)))
               (ipy (executable-find "ipython"))
               (args (ein:notebook-console-args-get ein:notebook))
               (python-shell-setup-codes nil)
               (python-shell-interpreter
                (format "python %s console --existing %skernel-%s.json %s"
                        ipy dir kid args)))
          (funcall 'python-shell-switch-to-shell)))
    (ein:log 'warn "python.el is not loaded!")))

(provide 'ein-notebook)

;;; ein-notebook.el ends here
