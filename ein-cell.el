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
(require 'eieio)
(require 'ansi-color)

(require 'ein-log)
(require 'ein-utils)
(require 'ein-node)


(defmacro ein:oset-if-empty (obj slot value)
  `(unless (and (slot-boundp ,obj ,slot) (oref ,obj ,slot))
     (oset ,obj ,slot ,value)))

(defmacro ein:oref-safe (obj slot)
  `(when (slot-boundp ,obj ,slot)
     (oref ,obj ,slot)))


(defclass ein:basecell ()
  ((cell-type :initarg :cell-type :type string)
   (read-only :initarg :read-only :initform nil :type boolean)
   (ewoc :initarg :ewoc :type ewoc)
   (element :initarg :element :initform nil :type list
    :documentation "ewoc nodes")
   (element-names :initarg :element-names)
   (input :initarg :input :type string
    :documentation "Place to hold data until it is rendered via `ewoc'.")
   (outputs :initarg :outputs :initform nil :type list)
   (cell-id :initarg :cell-id :initform (ein:utils-uuid) :type string))
  "Notebook cell base class")

(defclass ein:codecell (ein:basecell)
  ((cell-type :initarg :cell-type :initform "code")
   (element-names :initform (:prompt :input :output :footer))
   (input-prompt-number :initarg :input-prompt-number
                        ;; FIXME: "*" should be treated some how to
                        ;;        make this slot typed.
                        ;; :type integer
                        )
   (running :initarg :running :initform nil :type boolean)))

(defclass ein:textcell (ein:basecell)
  ((cell-type :initarg :cell-type :initform "text")
   (element-names :initform (:prompt :input :footer))))

(defclass ein:htmlcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "html")))

(defclass ein:markdowncell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "markdown")))

(defclass ein:rstcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "rst")))

(defun ein:cell-class-from-type (type)
  (ein:case-equal type
    (("code") 'ein:codecell)
    (("text") 'ein:textcell)
    (("html") 'ein:htmlcell)
    (("markdown") 'ein:markdowncell)
    (("rst") 'ein:rstcell)))

(defun ein:cell-from-type (type &rest args)
  (apply (ein:cell-class-from-type type) "Cell" args))

(defun ein:cell-from-json (data &rest args)
  (ein:cell-init (apply #'ein:cell-from-type
                        (plist-get data :cell_type) args) data))

(defmethod ein:cell-init ((cell ein:codecell) data)
  (ein:oset-if-empty cell :outputs (plist-get data :outputs))
  (ein:oset-if-empty cell :input (plist-get data :input))
  (ein:aif (plist-get data :prompt_number)
      (ein:oset-if-empty cell :input-prompt-number it))
  cell)

(defmethod ein:cell-init ((cell ein:textcell) data)
  (ein:aif (plist-get data :source)
      (oset cell :input it))
  cell)

(defmethod ein:cell-num-outputs ((cell ein:codecell))
  (length (oref cell :outputs)))

(defmethod ein:cell-num-outputs ((cell ein:textcell))
  0)

(defmethod ein:cell-element-get ((cell ein:basecell) prop &rest args)
  "Return ewoc node named PROP in CELL.
If PROP is `:output' a list of ewoc nodes is returned.
A specific node can be specified using optional ARGS."
  (if (memq prop (oref cell :element-names))
      (plist-get (oref cell :element) prop)
    (error "PROP %s is not supported." prop)))

(defmethod ein:cell-element-get ((cell ein:codecell) prop &optional index)
  (let ((element (oref cell :element)))
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
        (t (call-next-method))))))

(defmethod ein:cell-element-get ((cell ein:textcell) prop)
  (let ((element (oref cell :element)))
    (case prop
      (:after-input (plist-get element :footer))
      (:before-input (plist-get element :prompt))
      (t (call-next-method)))))

(defmethod ein:cell-all-element ((cell ein:basecell))
  (list (ein:cell-element-get cell :prompt)
        (ein:cell-element-get cell :input)
        (ein:cell-element-get cell :footer)))

(defmethod ein:cell-all-element ((cell ein:codecell))
  (append (call-next-method)
          (ein:cell-element-get cell :output)))

(defun ein:cell-make-element (make-node num-outputs)
  (list
   :prompt (funcall make-node 'prompt)
   :input  (funcall make-node 'input)
   :output (loop for i from 0 below num-outputs
                 collect (funcall make-node 'output i))
   :footer (funcall make-node 'footer)))

(defun ein:cell-enter-last (cell)
  (let* ((ewoc (oref cell :ewoc))
         ;; Use `cell' as data for ewoc.  Use the whole cell data even
         ;; if it is not used, to access it from the notebook buffer.
         ;; It is equivalent to `this.element.data("cell", this)' in
         ;; IPython.Cell (see cell.js).
         (make-node
          (lambda (&rest path)
            (ewoc-enter-last ewoc (ein:node-new `(cell ,@path) cell))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs cell))))
    (oset cell :element element)
    cell))

(defun ein:cell-insert-below (base-cell other-cell)
  (let* ((ewoc (oref base-cell :ewoc))
         (node (ein:cell-element-get base-cell :footer))
         (make-node
          (lambda (&rest path)
            (setq node (ewoc-enter-after
                        ewoc node (ein:node-new `(cell ,@path) other-cell)))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs other-cell))))
    (oset other-cell :element element)
    other-cell))

(defun ein:cell-pp (path data)
  (case (car path)
    (prompt (ein:cell-insert-prompt data))
    (input  (ein:cell-insert-input data))
    (output (ein:cell-insert-output (cadr path) data))
    (footer (ein:cell-insert-footer))))

(defmethod ein:cell-insert-prompt ((cell ein:codecell))
  ;; Newline is inserted in `ein:cell-insert-input'.
  (ein:insert-read-only
   (format "In [%s]:" (or (ein:oref-safe cell :input-prompt-number)  " "))))

(defmethod ein:cell-insert-prompt ((cell ein:textcell))
  (ein:insert-read-only
   (format "In [%s]:" (oref cell :cell-type))))

(defun ein:cell-insert-input (cell)
  ;; Newlines must allow insertion before/after its position.
  (insert (propertize "\n" 'read-only t 'rear-nonsticky t)
          (or (ein:oref-safe cell :input) "")
          (propertize "\n" 'read-only t)))

(defvar ein:cell-output-dynamic nil)

(defun ein:cell-insert-output (index cell)
  (let ((out (nth index (oref cell :outputs)))
        (dynamic ein:cell-output-dynamic))
    (ein:case-equal (plist-get out :output_type)
      (("pyout")        (ein:cell-append-pyout        cell out dynamic))
      (("pyerr")        (ein:cell-append-pyerr        cell out))
      (("display_data") (ein:cell-append-display-data cell out dynamic))
      (("stream")       (ein:cell-append-stream       cell out))))
  (ein:insert-read-only "\n"))

(defun ein:cell-insert-footer ()
  (ein:insert-read-only "\n"))


(defun ein:cell-node-p (node &optional element-name)
  (let* ((path (ein:$node-path node))
         (p0 (car path))
         (p1 (cadr path))
         (cell (ein:$node-path node)))
    (and cell (eql p0 'cell) (or (not element-name) (eql p1 element-name)))))

(defun ein:cell-ewoc-node-p (ewoc-node &optional element-name)
  (ein:cell-node-p (ewoc-data ewoc-node) element-name))

(defun ein:cell-from-ewoc-node (ewoc-node)
  (ein:aand ewoc-node (ewoc-data it) (ein:$node-data it)))

(defun ein:cell-get-text (cell)
  "Grab text in the input area of the cell at point."
  (let* ((ewoc (oref cell :ewoc))
         (input-node (ein:cell-element-get cell :input))
         ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (buffer-substring beg end)))

(defun ein:cell-set-text (cell text)
  (let* ((input-node (ein:cell-element-get cell :input))
         (ewoc (oref cell :ewoc))
           ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (save-excursion
      ;; probably it is better to set :input and update via ewoc?
      (goto-char beg)
      (delete-region beg end)
      (insert text))))

(defun ein:cell-running-set (cell running)
  ;; FIXME: change the appearance of the cell
  (oset cell :running running))

(defun ein:cell-set-input-prompt (cell &optional number)
  (oset cell :input-prompt-number number)
  (let ((inhibit-read-only t))
    (ewoc-invalidate (oref cell :ewoc)
                     (ein:cell-element-get cell :prompt))))

(defun ein:cell-finish-completing (cell matched-text matches)
  (let* ((end (point))
         (beg (re-search-backward (concat matched-text "\\=")))
         (word (if (and beg matches)
                   (completing-read "Complete: " matches))))
    (when word
      (delete-region beg end)
      (insert word))))

(defun ein:cell-finish-tooltip (cell content)
  ;; FIXME: implement!
  (ein:log 'info "`ein:cell-finish-tooltip' is not implemented!"))

(defun ein:cell-goto (cell)
  (ewoc-goto-node (oref cell :ewoc) (ein:cell-element-get cell :input))
  ;; Skip the newline
  (forward-char))

(defun ein:cell-clear-output (cell stdout stderr other)
  ;; codecell.js in IPytohn implements it using timeout and callback.
  ;; As it is unclear why timeout is needed, just clear output
  ;; instantaneously for now.
  (ein:log 'debug "cell-clear-output stdout=%s stderr=%s other=%s"
           stdout stderr other)
  (let ((ewoc (oref cell :ewoc))
        (output-nodes (ein:cell-element-get cell :output)))
    (if (and stdout stderr other)
        (progn
          ;; clear all
          (let ((inhibit-read-only t))
            (apply #'ewoc-delete ewoc output-nodes))
          (plist-put (oref cell :element) :output nil)
          (oset cell :outputs nil))
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
        ;; remove from `:element'
        (let* ((element (oref cell :element))
               (old-output (plist-get element :output))
               (new-ouptut (ein:remove-by-index old-output indices)))
          (plist-put element :output new-ouptut))
        ;; remove cleared outputs from internal data
        (oset cell :outputs
              (ein:remove-by-index (oref cell :outputs) indices))))))

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
  (oset cell :outputs
        (append (oref cell :outputs) (list json)))
  ;; enter last output element
  (let* ((inhibit-read-only t)
         (ewoc (oref cell :ewoc))
         (index (1- (ein:cell-num-outputs cell)))
         (path `(cell output ,index))
         (class (ein:cell-output-json-to-class json))
         (data (ein:node-new path cell class))
         (last-node (ein:cell-element-get cell :last-output))
         (ewoc-node (ewoc-enter-after ewoc last-node data))
         (element (oref cell :element)))
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

(defmethod ein:cell-to-json ((cell ein:codecell))
  "Return json-ready alist."
  `((input . ,(ein:cell-get-text cell))
    (cell_type . "code")
    ,@(ein:aif (ein:oref-safe cell :input-prompt-number)
          `((prompt_number . ,it)))
    (outputs . ,(apply #'vector (oref cell :outputs)))
    (language . "python")
    ;; FIXME: implement `collapsed'
    (collapsed . ,json-false)))

(defmethod ein:cell-to-json ((cell ein:textcell))
  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ein:cell-get-text cell))))

(defun ein:cell-next (cell)
  "Return next cell of the given CELL or nil if CELL is the last one."
  (ein:aif (ewoc-next (oref cell :ewoc)
                      (ein:cell-element-get cell :footer))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (ein:basecell-child-p cell)
          cell))))

(defun ein:cell-prev (cell)
  "Return previous cell of the given CELL or nil if CELL is the first one."
  (ein:aif (ewoc-prev (oref cell :ewoc)
                      (ein:cell-element-get cell :prompt))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (ein:basecell-child-p cell)
          cell))))

(provide 'ein-cell)

;;; ein-cell.el ends here
