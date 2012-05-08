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
(require 'ein-pager)
(require 'ein-events)


(defvar ein:notebook-buffer-name-template "*ein: %s*")

(defstruct ein:$notebook
  notebook-id                           ; uuid string
  data                                  ; json data - FIXME: remove this!
  ewoc                                  ; ewoc
  kernel                                ; ein:$kernel
  pager                                 ; ein:$pager
  dirty                                 ; t/nil
  msg-cell-map                          ; hash
  metadata                              ; json data
  notebook-name                         ; string
  )

(defvar ein:notebook nil)
(make-variable-buffer-local 'ein:notebook)
(put 'ein:notebook 'permanent-local t)

(defmacro ein:@notebook (slot)
  "Quick access to buffer local notebook attributes \(slot of `ein:$notebook').

The following two lines are equivalent:
  (ein:@notebook SLOT)
  (ein:$notebook-SLOT ein:notebook)
Note that SLOT should not be quoted."
  (let ((accessor (intern (format "ein:$notebook-%s" slot))))
    `(,accessor ein:notebook)))

(defmacro ein:notebook-in-buffer (&rest body)
  "Execute BODY if `ein:notebook' is defined."
  (declare (indent 0))
  `(if ein:notebook
       (progn
         ,@body)
     (ein:log 'warn "Not in notebook buffer")))

(defun ein:notebook-new (notebook-id data &rest args)
  (apply #'make-ein:$notebook
         :notebook-id notebook-id
         :data data
         :msg-cell-map (make-hash-table :test 'equal)
         args))

(defun ein:notebook-setup (&rest args)
  (setq ein:notebook (apply #'ein:notebook-new args)))

(defun ein:notebook-url (notebook-id)
  (concat (file-name-as-directory ein:base-project-url)
          "notebooks/" notebook-id))

(defun ein:notebook-open (notebook-id)
  (let ((url (ein:notebook-url notebook-id)))
    (lexical-let ((notebook-id notebook-id))
      (url-retrieve
       url
       (lambda (s) (ein:notebook-pop-buffer notebook-id))))))

(defun ein:notebook-pop-buffer (notebook-id)
  (let ((data (ein:json-read)))
    (kill-buffer (current-buffer))
    (with-current-buffer
        (get-buffer-create
         (format ein:notebook-buffer-name-template notebook-id))
      (ein:log-setup notebook-id)
      (ein:log 'info "Start logging.")
      (ein:notebook-setup notebook-id data)
      (ein:notebook-render)
      (pop-to-buffer (current-buffer)))))

(defun ein:notebook-get-worksheet (&optional wid)
  (unless wid (setq wid 0))
  (nth wid (plist-get (ein:@notebook data) :worksheets)))

(defun ein:notebook-render ()
  "(Re-)Render the notebook."
  (interactive)
  (assert ein:notebook)  ; make sure in a notebook buffer
  (ein:notebook-from-json ein:notebook (ein:@notebook data))
  (ein:notebook-mode)
  (ein:notebook-start-kernel)
  (ein:log 'info "Notebook %s is ready" (ein:@notebook notebook-id)))

(defun ein:notebook-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))


;;; Cell indexing, retrieval, etc.

(defun ein:notebook-cell-new (notebook &rest args)
  (apply #'ein:cell-new :ewoc (ein:$notebook-ewoc notebook) args))

(defun ein:notebook-get-cells (notebook)
  (let* ((ewoc (ein:$notebook-ewoc notebook))
         (nodes (ewoc-collect ewoc (lambda (n) (ein:cell-node-p n 'prompt)))))
    (mapcar #'ein:$node-data nodes)))

(defun ein:notebook-cell-for-msg (notebook msg-id)
  (let* ((msg-cell-map (ein:$notebook-msg-cell-map notebook))
         (cell-id (gethash msg-id msg-cell-map)))
    (when cell-id
      (loop for cell in (ein:notebook-get-cells notebook)
            when (equal (ein:$cell-cell-id cell) cell-id)
            return cell))))

(defun ein:notebook-ncells (notebook)
  (length (ein:notebook-get-cells notebook)))


;; Insertion and deletion of cells

(defun ein:notebook-insert-cell-below (notebook type &optional base-cell)
  (unless base-cell
    (setq base-cell (ein:notebook-get-current-cell)))
  (let ((cell (case type
                (code (ein:notebook-cell-new notebook))
                ((markdown html raw heading)
                 (ein:log 'info "cell type `%s' is not implemented yet" type)
                 ;; but make a new one anyway for now
                 (ein:notebook-cell-new notebook))
                (t
                 (ein:log 'info "cell type %s is not supported" type)))))
    (when cell
      (cond
       ((= (ein:notebook-ncells notebook) 0)
        (ein:cell-enter-last cell))
       (base-cell
        (ein:cell-insert-below base-cell cell)))
      (ein:cell-goto cell)
      (setf (ein:$notebook-dirty notebook) t))
    cell))

(defun ein:notebook-insert-cell-below-command ()
  (interactive)
  (ein:notebook-in-buffer
    (ein:notebook-insert-cell-below ein:notebook 'code)))


;;; Kernel related things

(defun ein:notebook-start-kernel ()
  (let ((kernel (ein:kernel-init)))
    (setf (ein:@notebook kernel) kernel)
    (ein:kernel-start kernel (ein:@notebook notebook-id)
                      #'ein:notebook-kernel-started
                      (list ein:notebook))))

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
        (ein:cell-finish-completing cell
                                    (plist-get content :matched_text)
                                    (plist-get content :matches)))
       ((equal msg_type "object_info_reply")
        (when (plist-get content :found)
          (ein:cell-finish-tooltip cell content)))
       (t (ein:log 'info "nown reply: %s" msg_type)))
      (when (plist-member content :payload)
        (ein:notebook-handle-payload notebook cell
                                     (plist-get content :payload))))))

(defun ein:notebook-handle-payload (notebook cell payload)
  (loop for p in payload
        for text = (plist-get p :text)
        for source = (plist-get p :source)
        if (equal source "IPython.zmq.page.page")
        when (equal (ein:trim text) "")
        do (let ((pager (ein:$notebook-pager notebook)))
             (ein:pager-clear pager)
             (ein:pager-expand pager)
             (ein:pager-append-text pager text))
        else if
        (equal source "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input")
        do (let ((new-cell (ein:notebook-insert-cell-below 'code cell)))
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

(defun ein:notebook-get-current-cell (&optional pos)
  (let ((cell (ein:aand (ein:notebook-get-current-ewoc-node pos)
                        (ewoc-data it)
                        (ein:$node-data it))))
    (when (ein:$cell-p cell) cell)))

(defun ein:notebook-execute-current-cell ()
  (interactive)
  ;; FIXME: implement `add_new' and `terminal' option like
  ;; `Notebook.execute_selected_cell'.
  (let ((cell (ein:notebook-get-current-cell)))
    (ein:cell-clear-output cell t t t)
    (ein:cell-set-input-prompt cell "*")
    (ein:cell-running-set cell t)
    ;; FIXME: treat cell type
    (let* ((code (ein:cell-get-text cell))
           (msg-id (ein:kernel-execute (ein:@notebook kernel) code)))
      (puthash msg-id (ein:$cell-cell-id cell) (ein:@notebook msg-cell-map)))
    (setf (ein:@notebook dirty) t)))


;;; Persistance and loading

(defun ein:notebook-from-json (notebook data)
  (let* ((metadata (plist-get data :metadata))
         (notebook-name (plist-get metadata :name)))
    (setf (ein:$notebook-metadata notebook) metadata)
    (setf (ein:$notebook-notebook-name notebook) notebook-name))
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
             (ein:notebook-cell-new ein:notebook :data cell-data)))
          ;; Only handle 1 worksheet for now, as in notebook.js
          (plist-get (nth 0 (plist-get data :worksheets)) :cells))))

(defun ein:notebook-to-json (notebook)
  "Return json-ready plist."
  (list
   :worksheets
   (list :cells (mapcar #'ein:cell-to-json (ein:notebook-get-cells notebook)))
   :metadata (ein:$notebook-metadata notebook)))


;;; Notebook mode

(defvar ein:notebook-modes
  '(ein:notebook-mumamo-mode ein:notebook-plain-mode))

(defun ein:notebook-choose-mode ()
  (loop for mode in ein:notebook-modes
        if (functionp mode)
        return mode))

(defun ein:notebook-mode ()
  (funcall (ein:notebook-choose-mode)))

(defvar ein:notebook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'ein:notebook-render)
    (define-key map "\C-c\C-c" 'ein:notebook-execute-current-cell)
    (define-key map "\C-c\C-b" 'ein:notebook-insert-cell-below-command)
    map))

(define-derived-mode ein:notebook-plain-mode fundamental-mode "ein:notebook"
  "IPython notebook command without fancy coloring.")

(setq ein:notebook-plain-mode-map ein:notebook-mode-map)

(provide 'ein-notebook)

;;; ein-notebook.el ends here
