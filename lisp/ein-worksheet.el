;;; ein-worksheet.el --- Worksheet module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-worksheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-worksheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-worksheet.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)

(require 'ein)
(require 'ein-utils)
(require 'ein-cell)
(require 'ein-notification)
(require 'ein-kill-ring)

(eval-when-compile (defvar ein:notebook-enable-undo))
(declare-function ein:$notebook-url-or-port "ein-notebook")
(declare-function ein:$worksheet-nbformat "ein-notebook")
(declare-function ein:notebook-mode "ein-notebook")
(declare-function ein:notebook-discard-output-p "ein-notebook")
(declare-function ein:notebook-empty-undo-maybe "ein-notebook")


;;; Class and variable

(defvar ein:worksheet-buffer-name-template "*ein: %s/%s*")

(defclass ein:worksheet ()
  (;; Recursive reference to notebook... but needs notebook name here.
   (notebook :initarg :notebook :type ein:$notebook)
   (data :initarg :data)
   (ewoc :initarg :ewoc :type ewoc)
   (kernel :initarg :kernel :type ein:$kernel)
   (dirty :initarg :dirty :type boolean)
   (metadata :initarg :metadata :initform nil)
   (events :initarg :events)
   (notification :initarg :notification)))

(ein:deflocal ein:%worksheet% nil
  "Buffer local variable to store an instance of `ein:worksheet'.")


;;; Initialization of object and buffer

(defun ein:worksheet-new (notebook kernel events &rest args)
  (apply #'make-instance 'ein:worksheet
         :notebook notebook :kernel kernel :events events
         args))

(defmethod ein:worksheet-bind-events ((ws ein:worksheet))
  ;; Bind events for sub components:
  (with-slots (events) ws
    (ein:notification-bind-events (oref ws :notification) events)
    (mapc (lambda (cell) (oset cell :events events))
          (ein:worksheet-get-cells ws))))

(defmethod ein:worksheet-notebook-name ((ws ein:worksheet))
  (ein:notebook-name (oref ws :notebook)))

(defmethod ein:worksheet-url-or-port ((ws ein:worksheet))
  (ein:$notebook-url-or-port (oref ws :notebook)))

(defmethod ein:worksheet-name ((ws ein:worksheet))
  (plist-get (oref ws :metadata) :name))

(defmethod ein:worksheet-full-name ((ws ein:worksheet))
  (let ((nb-name (ein:worksheet-notebook-name ws)))
    (ein:aif (ein:worksheet-name ws)
        (concat nb-name "/" it)
      nb-name)))

(defmethod ein:worksheet-buffer ((ws ein:worksheet))
  (ein:and-let* (((slot-boundp ws :ewoc))
                 (ewoc (oref ws :ewoc))
                 (buffer (ewoc-buffer ewoc))
                 ((buffer-live-p buffer)))
    buffer))

(defmethod ein:worksheet--get-buffer ((ws ein:worksheet))
  (or (ein:worksheet-buffer ws)
      (generate-new-buffer
       (format ein:worksheet-buffer-name-template
               (ein:worksheet-url-or-port ws)
               (ein:worksheet-full-name ws)))))

(defmethod ein:worksheet-render ((ws ein:worksheet))
  (with-current-buffer (ein:worksheet--get-buffer ws)
    (setq ein:%worksheet% ws)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ewoc (ein:ewoc-create 'ein:worksheet-pp
                                   (ein:propertize-read-only "\n")
                                   nil t)))
        (oset ws :ewoc ewoc)
        (mapc (lambda (cell-data)
                (ein:cell-enter-last
                 (ein:cell-from-json cell-data :ewoc ewoc)))
              (plist-get (oref ws :data) :cells))))
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)  ; clear undo history
    (when (eq ein:notebook-enable-undo 'no)
      (setq buffer-undo-list t))
    (ein:notebook-mode)
    (oset ws :notification (ein:notification-setup (current-buffer)))
    (ein:worksheet-bind-events ws)
    (ein:worksheet-set-kernel ws)
    (ein:log 'info "Worksheet %s is ready" (ein:worksheet-full-name ws))))

(defun ein:worksheet-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))


;;; Persistance and loading

(defmethod ein:worksheet-from-json ((ws ein:worksheet) data)
  (oset ws :data data)
  ws)

(defmethod ein:worksheet-to-json ((ws ein:worksheet))
  (let* ((notebook (oref ws :notebook))
         (cells (mapcar (lambda (c)
                          (ein:cell-to-json
                           c (ein:notebook-discard-output-p notebook c)))
                        (ein:worksheet-get-cells ws))))
    `((cells . ,(apply #'vector cells)))))


;;; Cell indexing, retrieval, etc.

(defmethod ein:worksheet-cell-from-type ((ws ein:worksheet) type &rest args)
  "Create a cell of TYPE (symbol or string)."
  ;; FIXME: unify type of TYPE to symbol or string.
  (apply #'ein:cell-from-type
         (format "%s" type)
         :ewoc (oref ws :ewoc)
         :events (oref ws :events)
         args))

(defmethod ein:worksheet-get-cells ((ws ein:worksheet))
  (let* ((ewoc (oref ws :ewoc))
         (nodes (ewoc-collect ewoc (lambda (n) (ein:cell-node-p n 'prompt)))))
    (mapcar #'ein:$node-data nodes)))

(defmethod ein:worksheet-ncells ((ws ein:worksheet))
  (length (ein:worksheet-get-cells ws)))

(defun ein:worksheet-get-current-ewoc-node (&optional pos)
  (ein:aand ein:%worksheet% (oref it :ewoc) (ewoc-locate it pos)))

(defun ein:worksheet-get-nearest-cell-ewoc-node (&optional pos max cell-p)
  (ein:and-let* ((ewoc-node (ein:worksheet-get-current-ewoc-node pos)))
    ;; FIXME: can be optimized using the argument `max'
    (while (and ewoc-node
                (not (and (ein:cell-ewoc-node-p ewoc-node)
                          (if cell-p
                              (funcall cell-p
                                       (ein:cell-from-ewoc-node ewoc-node))
                            t))))
      (setq ewoc-node (ewoc-next (oref ein:%worksheet% :ewoc) ewoc-node)))
    ewoc-node))

(defun ein:worksheet-get-current-cell (&optional pos noerror)
  "Return a cell at POS.  If POS is not given, it is assumed be the
current cursor position.  When the current buffer is not worksheet
buffer or there is no cell in the current buffer, return `nil'."
  (let ((cell (ein:cell-from-ewoc-node
               (ein:worksheet-get-current-ewoc-node pos))))
    (if (ein:basecell-child-p cell)
        cell
      (unless noerror
        (error "No cell found at pos=%s" pos)))))

(defun ein:worksheet-get-cells-in-region (beg end)
  (ein:clip-list (ein:aand ein:%worksheet% (ein:worksheet-get-cells it))
                 (ein:worksheet-get-current-cell beg)
                 (ein:worksheet-get-current-cell end)))

(defun ein:worksheet-get-cells-in-region-or-at-point ()
  (if (region-active-p)
      (ein:worksheet-get-cells-in-region (region-beginning) (region-end))
    (list (ein:worksheet-get-current-cell))))


;;; Insertion and deletion of cells

(defun ein:worksheet--get-ws-or-error ()
  (or ein:%worksheet% (error "Not in worksheet buffer.")))

(defun ein:worksheet-focus-cell ()
  (ein:aand (ein:worksheet-get-current-cell) (ein:cell-goto it)))

(defun ein:worksheet-delete-cell (ws cell &optional focus)
  "Delete a cell.  \(WARNING: no undo!)
This command has no key binding because there is no way to undo
deletion.  Use kill to play on the safe side.

If you really want use this command, you can do something like this
\(but be careful when using it!)::

  \(define-key ein:notebook-mode-map \"\\C-c\\C-d\"
              'ein:worksheet-delete-cell)"
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     t))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))        ; disable undo recording
    (apply #'ewoc-delete
           (oref ws :ewoc)
           (ein:cell-all-element cell)))
  (oset ws :dirty t)
  (ein:notebook-empty-undo-maybe)
  (when focus (ein:worksheet-focus-cell)))

(defun ein:worksheet-kill-cells (ws cells &optional focus)
  "Kill (\"cut\") the cell at point or cells in region.
Note that the kill-ring for cells is not shared with the default
kill-ring of Emacs (kill-ring for texts)."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-cells-in-region-or-at-point)
                     t))
  (when cells
    (mapc (lambda (c)
            (ein:cell-save-text c)
            (ein:worksheet-delete-cell ws c)
            (ein:cell-deactivate c))
          cells)
    (ein:kill-new cells)
    (when focus
      (deactivate-mark)
      (ein:worksheet-focus-cell))))

(defun ein:worksheet-copy-cell (cells)
  "Copy the cell at point.  (Put the current cell into the kill-ring.)"
  (interactive
   (list (when (ein:worksheet--get-ws-or-error)
           (prog1 (ein:worksheet-get-cells-in-region-or-at-point)
             (deactivate-mark)))))
  (let ((cells (mapcar
                (lambda (c)
                  (ein:cell-deactivate (ein:cell-copy c))) cells)))
    (ein:log 'info "%s cells are copied." (length  cells))
    (ein:kill-new cells)))

(defun ein:worksheet-insert-clone-below (ws cell pivot)
  (let ((clone (ein:cell-copy cell)))
    ;; Cell can be from another buffer, so reset `ewoc'.
    (oset clone :ewoc (oref ws :ewoc))
    (ein:worksheet-insert-cell-below ws clone pivot)
    clone))

(defun ein:worksheet-yank-cell (ws &optional n)
  "Insert (\"paste\") the latest killed cell.
Prefixes are act same as the normal `yank' command."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (let ((arg current-prefix-arg))
                       (cond ((listp arg) 0)
                             ((eq arg '-) -2)
                             (t (1- arg))))))
  (let* ((cell (ein:worksheet-get-current-cell)) ; can be nil
         (killed (ein:current-kill n)))
    (loop for c in killed
          with last = cell
          do (setq last (ein:worksheet-insert-clone-below ws c last))
          finally (ein:cell-goto last))))

(defun ein:worksheet-maybe-new-cell (ws type-or-cell)
  "Return TYPE-OR-CELL as-is if it is a cell, otherwise return a new cell."
  (let ((cell (if (ein:basecell-child-p type-or-cell)
                  type-or-cell
                (ein:worksheet-cell-from-type ws type-or-cell))))
    ;; When newly created or copied, kernel is not attached or not the
    ;; kernel of this worksheet.  So reset it here.
    (when (ein:codecell-p cell)
      (oset cell :kernel (oref ws :kernel)))
    (oset cell :events (oref ws :events))
    cell))

(defun ein:worksheet-insert-cell-below (ws type-or-cell pivot &optional focus)
  "Insert cell below.  Insert markdown cell instead of code cell
when the prefix argument is given.

When used as a lisp function, insert a cell of TYPE-OR-CELL just
after PIVOT and return the new cell."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (if prefix-arg 'markdown 'code)
                     (ein:worksheet-get-current-cell nil t) ; can be nil
                     t))
  (let ((cell (ein:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((= (ein:worksheet-ncells ws) 0)
      (ein:cell-enter-last cell))
     (pivot
      (ein:cell-insert-below pivot cell))
     (t (error
         "PIVOT is `nil' but ncells != 0.  There is something wrong...")))
    (ein:notebook-empty-undo-maybe)
    (oset ws :dirty t)
    (when focus (ein:cell-goto cell))
    cell))

(defun ein:worksheet-insert-cell-above (ws type-or-cell pivot &optional focus)
  "Insert cell above.  Insert markdown cell instead of code cell
when the prefix argument is given.
See also: `ein:worksheet-insert-cell-below'."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (if prefix-arg 'markdown 'code)
                     (ein:worksheet-get-current-cell nil t) ; can be nil
                     t))
  (let ((cell (ein:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((< (ein:worksheet-ncells ws) 2)
      (ein:cell-enter-first cell))
     (pivot
      (let ((prev-cell (ein:cell-prev pivot)))
        (if prev-cell
            (ein:cell-insert-below prev-cell cell)
          (ein:cell-enter-first cell))))
     (t (error
         "PIVOT is `nil' but ncells > 0.  There is something wrong...")))
    (ein:notebook-empty-undo-maybe)
    (oset ws :dirty t)
    (when focus (ein:cell-goto cell))
    cell))

(defun ein:worksheet-toggle-cell-type (ws cell &optional focus)
  "Toggle the cell type of the cell at point.
Use `ein:worksheet-change-cell-type' to change the cell type
directly."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     t))
  (let ((type (case (ein:$worksheet-nbformat (oref ws :notebook))
                (2 (ein:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "code")))
                (3 (ein:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "raw")
                     (("raw") "heading")
                     (("heading") "code"))))))
    (let ((relpos (ein:cell-relative-point cell))
          (new (ein:cell-convert-inplace cell type)))
      (when (ein:codecell-p new)
        (oset new :kernel (oref ws :kernel)))
      (ein:notebook-empty-undo-maybe)
      (when focus (ein:cell-goto new relpos)))))

(defun ein:worksheet-change-cell-type (ws cell type &optional level focus)
  "Change the cell type of the current cell.
Prompt will appear in the minibuffer.

When used in as a Lisp function, TYPE (string) should be chose
from \"code\", \"markdown\", \"raw\" and \"heading\".  LEVEL is
an integer used only when the TYPE is \"heading\"."
  (interactive
   (let* ((ws (ein:worksheet--get-ws-or-error))
          (cell (ein:worksheet-get-current-cell))
          (choices (case (ein:$worksheet-nbformat (oref ws :notebook))
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
     (list ws cell type level t)))

  (let ((relpos (ein:cell-relative-point cell))
        (new (ein:cell-convert-inplace cell type)))
    (when (ein:codecell-p new)
      (oset new :kernel (oref ws :kernel)))
    (when level
      (ein:cell-change-level new level))
    (ein:notebook-empty-undo-maybe)
    (when focus (ein:cell-goto new relpos))))

(defun ein:worksheet-split-cell-at-point (ws cell &optional no-trim focus)
  "Split cell at current position. Newlines at the splitting
point will be removed. This can be omitted by giving a prefix
argument \(C-u)."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     prefix-arg
                     t))
  ;; FIXME: should I inhibit undo?
  (let* ((beg (set-marker (make-marker) (ein:cell-input-pos-min cell)))
         (pos (point-marker))
         (head (buffer-substring beg pos))
         (new (ein:worksheet-insert-cell-above ws
                                               (oref cell :cell-type)
                                               cell)))
    (delete-region beg pos)
    (unless no-trim
      (setq head (ein:trim-right head "\n"))
      (save-excursion
        (goto-char pos)
        (while (looking-at-p "\n")
          (delete-char 1))))
    (ein:cell-set-text new head)
    (ein:notebook-empty-undo-maybe)
    (when focus (ein:cell-goto cell))))

(defun ein:worksheet-merge-cell (ws cell &optional next focus)
  "Merge previous cell into current cell.
If prefix is given, merge current cell into next cell."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     prefix-arg
                     t))
  (unless next
    (setq cell (ein:cell-prev cell))
    (unless cell (error "No previous cell"))
    (ein:cell-goto cell))
  (let* ((next-cell (ein:cell-next cell))
         (head (ein:cell-get-text cell)))
    (assert next-cell nil "No cell to merge.")
    (ein:worksheet-delete-cell ws cell)
    (save-excursion
      (goto-char (ein:cell-input-pos-min next-cell))
      (insert head "\n"))
    (ein:notebook-empty-undo-maybe)
    (when focus (ein:cell-goto next-cell))))


;;; Cell selection.


;;; Cell movement


;;; Cell collapsing and output clearing


;;; Kernel related things

(defmethod ein:worksheet-set-kernel ((ws ein:worksheet))
  (mapc (lambda (cell) (oset cell :kernel (oref ws :kernel)))
        (ein:filter #'ein:codecell-p (ein:worksheet-get-cells ws))))


;;; Generic getter


;;; Buffer


;;; Imenu

(provide 'ein-worksheet)

;;; ein-worksheet.el ends here
