;;; ein-cell.el --- Cell module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-cell.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-cell.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-cell.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; IPython has cell.js, codecell.js and textcell.js.
;; But let's start with one file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ansi-color)

(require 'ein-log)
(require 'ein-utils)
(require 'ein-node)

(defstruct ein:$cell
  type                                  ; "code"/"html"/"markdown"
  read-only                             ; nil/t
  data                                  ; default data - FIXME: remove this!
  ;; notebook                              ; `ein:$notebook'
  ewoc                                  ; ewoc (NOTE: use `ein:cell-get-ewoc')
  element                               ; ewoc nodes
  running                               ; nil/t
  input-prompt-number                   ; int
  outputs                               ; list
  cell-id                               ; uuid
  )

;; In the very first implementation `ein:$cell' had `notebook' slot.
;; But requiring ein-notebook causes recursive require which should be
;; avoided if possible.  For now, I just put `ewoc' instead of `notebook'.
;; To prepare for the future changes, `ein:cell-get-ewoc' must be used
;; to get ewoc instance instead of `ein:$cell-ewoc'.


(defun ein:cell-new (&rest args)
  (let ((cell (apply 'make-ein:$cell
                     :cell-id (ein:utils-uuid)
                     args)))
    (setf (ein:$cell-outputs cell) (ein:cell-data-get cell :outputs))
    (ein:setf-default (ein:$cell-input-prompt-number cell)
                      (ein:cell-data-get cell :prompt_number))
    (ein:setf-default (ein:$cell-type cell)
                      (ein:cell-data-get cell :cell_type))
    cell))

(defun ein:cell-data-get (cell prop)
  (plist-get (ein:$cell-data cell) prop))

;; (defun ein:cell-data-put (cell prop val)
;;   (plist-put (ein:$cell-data cell) prop val))

(defun ein:cell-num-outputs (cell)
  (length (ein:$cell-outputs cell)))

(defvar ein:cell-element-names
  (list :prompt :input :output :footer))

(defun ein:cell-element-get (cell prop &rest index)
  "Return ewoc node named PROP in CELL.
If PROP is `:output' a list of ewoc nodes is returned.
A specific note can be specified using INDEX."
  (let ((element (ein:$cell-element cell)))
    (if index
        (progn
          (assert (eql prop :output))
          (nth index (plist-get element prop)))
      (case prop
        (:after-input
         (ein:aif (nth 0 (plist-get element :output))
             it
           (plist-get element :footer)))
        (:after-output (plist-get element :footer))
        (:before-input (plist-get element :prompt))
        (:before-output (plist-get element :input))
        (:last-output
         (ein:aif (plist-get element :output)
             (car (last it))
           (plist-get element :input)))
        (t (if (memq prop ein:cell-element-names)
               (plist-get element prop)
             (error "PROP %s is not supported." prop)))))))

(defun ein:cell-get-ewoc (cell)
  (ein:$cell-ewoc cell))

(defun ein:cell-make-element (make-node num-outputs)
  (list
   :prompt (funcall make-node 'prompt)
   :input  (funcall make-node 'input)
   :output (loop for i from 0 below num-outputs
                 collect (funcall make-node 'output i))
   :footer (funcall make-node 'footer)))

(defun ein:cell-enter-last (cell)
  (let* ((ewoc (ein:cell-get-ewoc cell))
         ;; Use `cell' as data for ewoc.  Use the whole cell data even
         ;; if it is not used, to access it from the notebook buffer.
         ;; It is equivalent to `this.element.data("cell", this)' in
         ;; IPython.Cell (see cell.js).
         (make-node
          (lambda (&rest path)
            (ewoc-enter-last ewoc (ein:node-new `(cell ,@path) cell))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs cell))))
    (setf (ein:$cell-element cell) element)
    cell))

(defun ein:cell-insert-below (base-cell other-cell)
  (let* ((ewoc (ein:cell-get-ewoc base-cell))
         (node (ein:cell-element-get base-cell :footer))
         (make-node
          (lambda (&rest path)
            (setq node (ewoc-enter-after
                        ewoc node (ein:node-new `(cell ,@path) other-cell)))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs other-cell))))
    (setf (ein:$cell-element other-cell) element)
    other-cell))

(defun ein:cell-pp (path data)
  (case (car path)
    (prompt (ein:cell-insert-prompt data))
    (input  (ein:cell-insert-input data))
    (output (ein:cell-insert-output (cadr path) data))
    (footer (ein:cell-insert-footer))))

(defun ein:cell-insert-prompt (cell)
  ;; Newline is inserted in `ein:cell-insert-input'.
  (ein:insert-read-only
   (format "In [%s]:" (or (ein:$cell-input-prompt-number cell)  " "))))

(defun ein:cell-insert-input (cell)
  ;; Newlines must allow insertion before/after its position.
  (insert (propertize "\n" 'read-only t 'rear-nonsticky t)
          (or (ein:cell-data-get cell :input) "")
          (propertize "\n" 'read-only t)))

(defvar ein:cell-output-dynamic nil)

(defun ein:cell-insert-output (index cell)
  (let ((out (nth index (ein:$cell-outputs cell)))
        (dynamic ein:cell-output-dynamic))
    (ein:case-equal (plist-get out :output_type)
      (("pyout")        (ein:cell-append-pyout        cell out dynamic))
      (("pyerr")        (ein:cell-append-pyerr        cell out))
      (("display_data") (ein:cell-append-display-data cell out dynamic))
      (("stream")       (ein:cell-append-stream       cell out))))
  (ein:insert-read-only "\n"))

(defun ein:cell-insert-footer ()
  (ein:insert-read-only "\n"))


(defun ein:cell-node-p (node &optional cell-type)
  (let* ((path (ein:$node-path node))
         (p0 (car path))
         (p1 (cadr path))
         (cell (ein:$node-path node)))
    (and cell (eql p0 'cell) (or (not cell-type) (eql p1 cell-type)))))

(defun ein:cell-ewoc-node-p (ewoc-node &optional cell-type)
  (ein:cell-node-p (ewoc-data ewoc-node) cell-type))

(defun ein:cell-get-text (cell)
  "Grab text in the input area of the cell at point."
  (let* ((ewoc (ein:cell-get-ewoc cell))
         (input-node (ein:cell-element-get cell :input))
         ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (buffer-substring beg end)))

(defun ein:cell-set-text (cell text)
  (let* ((input-node (ein:cell-element-get cell :input))
         (ewoc (ein:cell-get-ewoc cell))
           ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (save-excursion
      ;; probably it is better to set $cell-data and update via ewoc?
      (goto-char beg)
      (delete-region beg end)
      (insert text))))

(defun ein:cell-running-set (cell running)
  ;; FIXME: change the appearance of the cell
  (setf (ein:$cell-running cell) running))

(defun ein:cell-set-input-prompt (cell &optional number)
  (setf (ein:$cell-input-prompt-number cell) number)
  (let ((inhibit-read-only t))
    (ewoc-invalidate (ein:cell-get-ewoc cell)
                     (ein:cell-element-get cell :prompt))))

(defun ein:cell-finish-completing (cell matched-text matches)
  ;; FIXME: implement!
  (ein:log 'info "`ein:cell-finish-completing' is not implemented!"))

(defun ein:cell-finish-tooltip (cell content)
  ;; FIXME: implement!
  (ein:log 'info "`ein:cell-finish-tooltip' is not implemented!"))

(defun ein:cell-goto (cell)
  (ewoc-goto-node (ein:cell-get-ewoc cell) (ein:cell-element-get cell :input))
  ;; Skip the newline
  (forward-char))

(defun ein:cell-clear-output (cell stdout stderr other)
  ;; codecell.js in IPytohn implements it using timeout and callback.
  ;; As it is unclear why timeout is needed, just clear output
  ;; instantaneously for now.
  (ein:log 'debug "cell-clear-output stdout=%s stderr=%s other=%s"
           stdout stderr other)
  (let ((ewoc (ein:cell-get-ewoc cell))
        (output-nodes (ein:cell-element-get cell :output)))
    (if (and stdout stderr other)
        (progn
          ;; clear all
          (let ((inhibit-read-only t))
            (apply #'ewoc-delete ewoc output-nodes))
          (plist-put (ein:$cell-element cell) :output nil)
          (setf (ein:$cell-outputs cell) nil))
      (let* ((ewoc-node-list
              (append
               (when stdout (ein:node-filter output-nodes :is 'output-stdout))
               (when stderr (ein:node-filter output-nodes :is 'output-stderr))
               (when stdout (ein:node-filter output-nodes
                                             :is 'output-subarea
                                             :not 'output-stderr
                                             :not 'output-stdout))))
             (indices
              (mapcar (lambda (n) (last (ein:$node-path (ewoc-data n))))
                      ewoc-node-list)))
        ;; remove from buffer
        (let ((inhibit-read-only t))
          (apply #'ewoc-delete ewoc ewoc-node-list))
        ;; remove from `ein:$cell-element'
        (let* ((element (ein:$cell-element cell))
               (old-output (plist-get element :output))
               (new-ouptut (ein:remove-by-index old-output indices)))
          (plist-put element :output new-ouptut))
        ;; remove cleared outputs from internal data
        (setf (ein:$cell-outputs cell)
              (ein:remove-by-index (ein:$cell-outputs cell) indices))))))

(defun ein:cell-output-json-to-class (json)
  (ein:case-equal (plist-get json :output_type)
    (("pyout")
     '(ouput-subarea))
    (("pyerr")
     '(ouput-subarea))
    (("display_data")
     '(ouput-subarea))
    (("stream")
     (list 'ouput-stream 'ouput-subarea
           (intern (format "ouput-%s" (plist-get json :stream)))))))

(defun ein:cell-append-output (cell json dynamic)
  ;; (ein:cell-expand cell)
  ;; (ein:flush-clear-timeout)
  (setf (ein:$cell-outputs cell)
        (append (ein:$cell-outputs cell) (list json)))
  ;; enter last output element
  (let* ((inhibit-read-only t)
         (ewoc (ein:cell-get-ewoc cell))
         (index (1- (ein:cell-num-outputs cell)))
         (path `(cell output ,index))
         (class (ein:cell-output-json-to-class json))
         (data (ein:node-new path cell class))
         (last-node (ein:cell-element-get cell :last-output))
         (ewoc-node (ewoc-enter-after ewoc last-node data))
         (element (ein:$cell-element cell)))
    (plist-put element :output
               (append (plist-get element :output) (list ewoc-node)))))

(defun ein:cell-append-pyout (cell json dynamic)
  (ein:insert-read-only (format "Out [%s]:\n"
                                (or (plist-get json :prompt_number) " ")))
  (ein:cell-append-mime-type json dynamic))

(defun ein:cell-append-pyerr (cell json)
  (mapc (lambda (tb)
          (ein:cell-append-text tb)
          (ein:cell-append-text "\n"))
        (plist-get json :traceback)))

(defun ein:cell-append-stream (cell json)
  (unless (plist-get json :stream)
    (plist-put json :stream "stdout"))
  (let ((last (last (ein:cell-element-get cell :output))))
    (when (and last
               (equal (plist-get last :output_type) "stream")
               (equal (plist-get json :stream) (plist-get last :stream)))
      (ein:insert-read-only (plist-get json :text)))))

(defun ein:cell-append-display-data (cell json dynamic)
  (ein:cell-append-mime-type json dynamic))

(defun ein:cell-append-mime-type (json dynamic)
  (loop
   for key in '(javascript svg png jpeg text html latex)
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     (javascript
      (when dynamic
        (ein:log 'info (concat "ein:cell-append-mime-type does not support "
                               "dynamic javascript. got: %s") value)))
     ((html latex text)
      (ein:insert-read-only (plist-get json type)))
     (svg
      (insert-image (create-image value key t)))
     ((png jpeg)
      (insert-image (create-image (base64-decode-string value) key t))))))

(defun ein:cell-append-text (data)
  ;; FIXME: implement HTML special escaping
  ;; escape ANSI & HTML specials in plaintext:
  (ein:insert-read-only (ansi-color-apply data)))

(defun ein:cell-to-json (cell)
  "Return json-ready alist."
  (ein:case-equal (ein:$cell-type cell)
    (("code") (ein:codecell-to-json cell))
    (t        (ein:textcell-to-json cell))))

(defun ein:codecell-to-json (cell)
  `((input . ,(ein:cell-get-text cell))
    (cell_type . "code")
    ,@(ein:aif (ein:$cell-input-prompt-number cell)
          `((prompt_number . ,it)))
    (outputs . ,(apply #'vector (ein:$cell-outputs cell)))
    (language . "python")
    ;; FIXME: implement `collapsed'
    (collapsed . ,json-false)))

(defun ein:textcell-to-json (cell)
  `((cell_type . ,(ein:$cell-type cell))
    (source    . ,(ein:cell-get-text cell))))

(defun ein:cell-next (cell)
  "Return next cell of the given CELL or nil if CELL is the last one."
  (ein:aif (ewoc-next (ein:cell-get-ewoc cell)
                      (ein:cell-element-get cell :footer))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (ein:$cell-p cell)
          cell))))

(provide 'ein-cell)

;;; ein-cell.el ends here
