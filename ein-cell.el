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

;;  Implementation note.  Current implementation of cell has redundant
;;  and not-guaranteed-to-be consistent information: `element' and
;;  `ein:$node'.  This part must be moved to ein-node.el module to
;;  make it well capsuled.

;; IPython has cell.js, codecell.js and textcell.js.
;; But let's start with one file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ansi-color)

(require 'ein-log)
(require 'ein-utils)
(require 'ein-node)


;;; Faces

(defface ein:cell-output-stderr
  '((((class color) (background light))
     :background "PeachPuff")
    (((class color) (background dark))
     :background "#8c5353"))
  "Face for stderr cell output"
  :group 'ein)


;;; EIEIO related utils

(defmacro ein:oset-if-empty (obj slot value)
  `(unless (and (slot-boundp ,obj ,slot) (oref ,obj ,slot))
     (oset ,obj ,slot ,value)))

(defmacro ein:oref-safe (obj slot)
  `(when (slot-boundp ,obj ,slot)
     (oref ,obj ,slot)))


;;; Cell classes

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
   (collapsed :initarg :collapsed :initform nil :type boolean)
   (running :initarg :running :initform nil :type boolean)))

(defclass ein:textcell (ein:basecell)
  ((cell-type :initarg :cell-type :initform "text")
   (element-names :initform (:prompt :input :footer))))

(defclass ein:htmlcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "html")))

(defclass ein:markdowncell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "markdown")))

(defclass ein:rawcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "raw")))

(defclass ein:headingcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "heading")
   (level :initarg :level :initform 1)))


;;; Cell factory

(defun ein:cell-class-from-type (type)
  (ein:case-equal type
    (("code") 'ein:codecell)
    (("text") 'ein:textcell)
    (("html") 'ein:htmlcell)
    (("markdown") 'ein:markdowncell)
    (("raw") 'ein:rawcell)
    (("heading") 'ein:headingcell)))

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
  (ein:oset-if-empty cell :collapsed
                     (let ((v (plist-get data :collapsed)))
                       (if (eql v json-false) nil v)))
  cell)

(defmethod ein:cell-init ((cell ein:textcell) data)
  (ein:aif (plist-get data :source)
      (oset cell :input it))
  cell)

(defmethod ein:cell-init ((cell ein:headingcell) data)
  (call-next-method)
  (ein:aif (plist-get data :level)
      (oset cell :level it))
  cell)

(defmethod ein:cell-convert ((cell ein:basecell) type)
  (let ((new (ein:cell-from-type type)))
    ;; copy attributes
    (loop for k in '(:read-only :ewoc)
          do (set-slot-value new k (slot-value cell k)))
    ;; copy input
    (oset new :input (if (ein:cell-active-p cell)
                         (ein:cell-get-text cell)
                       (oref cell :input)))
    ;; copy output when the new cell has it
    (when (memq :output (oref new :element-names))
      (oset new :outputs (mapcar 'identity (oref cell :outputs))))
    new))

(defmethod ein:cell-copy ((cell ein:basecell))
  (ein:cell-convert cell (oref cell :cell-type)))

(defmethod ein:cell-convert-inplace ((cell ein:basecell) type)
  "Convert CELL to TYPE and redraw corresponding ewoc nodes."
  (let ((new (ein:cell-convert cell type)))
    ;; copy element attribute
    (loop for k in (oref new :element-names)
          with old-element = (oref cell :element)
          do (oset new :element
                   (plist-put (oref new :element) k
                              (plist-get old-element k))))
    ;; setting ewoc nodes
    (loop for en in (ein:cell-all-element cell)
          for node = (ewoc-data en)
          do (setf (ein:$node-data node) new))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))         ; disable undo recording
      ;; delete ewoc nodes that is not copied
      (apply
       #'ewoc-delete (oref new :ewoc)
       (apply
        #'append
        (loop for name in (oref cell :element-names)
              unless (memq name (oref new :element-names))
              collect (let ((ens (ein:cell-element-get cell name)))
                        (if (listp ens) ens (list ens))))))
      ;; draw ewoc node
      (loop with ewoc = (oref new :ewoc)
            for en in (ein:cell-all-element new)
            do (ewoc-invalidate ewoc en)))
    new))


;;; Getter/setter

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


;; EWOC

(defun ein:cell-make-element (make-node num-outputs)
  (let ((buffer-undo-list t))           ; disable undo recording
    (list
     :prompt (funcall make-node 'prompt)
     :input  (funcall make-node 'input)
     :output (loop for i from 0 below num-outputs
                   collect (funcall make-node 'output i))
     :footer (funcall make-node 'footer))))

(defmethod ein:cell-enter-last ((cell ein:basecell))
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

(defmethod ein:cell-enter-first ((cell ein:basecell))
  (let* ((ewoc (oref cell :ewoc))
         (node nil)
         (make-node
          (lambda (&rest path)
            (let ((ewoc-data (ein:node-new `(cell ,@path) cell)))
              (setq node
                    (if node
                        (ewoc-enter-after ewoc node ewoc-data)
                      (ewoc-enter-first ewoc ewoc-data))))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs cell))))
    (oset cell :element element)
    cell))

(defmethod ein:cell-insert-below ((base-cell ein:basecell) other-cell)
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
    (footer (ein:cell-insert-footer data))))

(defmethod ein:cell-insert-prompt ((cell ein:codecell))
  "Insert prompt of the CELL in the buffer.
Called from ewoc pretty printer via `ein:cell-pp'."
  ;; Newline is inserted in `ein:cell-insert-input'.
  (ein:insert-read-only
   (format "In [%s]:" (or (ein:oref-safe cell :input-prompt-number)  " "))))

(defmethod ein:cell-insert-prompt ((cell ein:textcell))
  (ein:insert-read-only
   (format "In [%s]:" (oref cell :cell-type))))

(defmethod ein:cell-insert-prompt ((cell ein:headingcell))
  (ein:insert-read-only
   (format "In [%s %s]:" (oref cell :cell-type) (oref cell :level))))

(defmethod ein:cell-insert-input ((cell ein:basecell))
  "Insert input of the CELL in the buffer.
Called from ewoc pretty printer via `ein:cell-pp'."
  ;; Newlines must allow insertion before/after its position.
  (insert (propertize "\n" 'read-only t 'rear-nonsticky t)
          (or (ein:oref-safe cell :input) "")
          (propertize "\n" 'read-only t)))

(defvar ein:cell-output-dynamic nil)

(defun ein:cell-insert-output (index cell)
  "Insert INDEX-th output of the CELL in the buffer.
Called from ewoc pretty printer via `ein:cell-pp'."
  (if (oref cell :collapsed)
      (progn
        (ein:insert-read-only ".")
        (when (= (1+ index) (ein:cell-num-outputs cell))
          (ein:insert-read-only "\n")))
    (let ((out (nth index (oref cell :outputs)))
          (dynamic ein:cell-output-dynamic))
      ;; Handle newline for previous stream output.
      ;; In IPython JS, it is handled in `append_stream' because JS
      ;; does not need to care about newline (DOM does it for JS).
      ;; FIXME: Maybe I should abstract ewoc in some way and get rid
      ;;        of this.
      (let ((last-out (and (> index 0)
                           (nth (1- index) (oref cell :outputs)))))
        ;; If previous output is stream type, consider adding newline
        (when (and last-out
                   (equal (plist-get last-out :output_type) "stream"))
          ;; Check if the last output is from the same stream.
          ;; If so, do *NOT* insert newline, otherwise insert newline.
          (unless (and (equal (plist-get out :output_type) "stream")
                       (equal (plist-get out      :stream)
                              (plist-get last-out :stream)))
            (ein:cell-append-stream-text-fontified "\n" last-out))))
      ;; Finally insert real data
      (ein:case-equal (plist-get out :output_type)
        (("pyout")        (ein:cell-append-pyout        cell out dynamic))
        (("pyerr")        (ein:cell-append-pyerr        cell out))
        (("display_data") (ein:cell-append-display-data cell out dynamic))
        (("stream")       (ein:cell-append-stream       cell out))))))

(defmethod ein:cell-insert-footer ((cell ein:basecell))
  "Insert footer (just a new line) of the CELL in the buffer.
Called from ewoc pretty printer via `ein:cell-pp'."
  (ein:insert-read-only "\n"))

(defmethod ein:cell-insert-footer ((cell ein:codecell))
  (let ((last-out (car (last (oref cell :outputs)))))
    (when (equal (plist-get last-out :output_type) "stream")
      (ein:cell-append-stream-text-fontified "\n" last-out)))
  (call-next-method))


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

(defmethod ein:cell-input-pos-min ((cell ein:basecell))
  (let* ((ewoc (oref cell :ewoc))
         (input-node (ein:cell-element-get cell :input)))
    ;; 1+ for skipping newline
    (1+ (ewoc-location input-node))))

(defmethod ein:cell-input-pos-max ((cell ein:basecell))
  (let* ((ewoc (oref cell :ewoc))
         (input-node (ein:cell-element-get cell :input)))
    ;; 1- for skipping newline
    (1- (ewoc-location (ewoc-next ewoc input-node)))))

(defmethod ein:cell-get-text ((cell ein:basecell))
  "Grab text in the input area of the cell at point."
  (let* ((beg (ein:cell-input-pos-min cell))
         (end (ein:cell-input-pos-max cell)))
    (buffer-substring beg end)))

(defmethod ein:cell-set-text ((cell ein:basecell) text)
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

(defmethod ein:cell-save-text ((cell ein:basecell))
  (oset cell :input (ein:cell-get-text cell)))

(defmethod ein:cell-deactivate ((cell ein:basecell))
  (oset cell :element nil)
  cell)

(defmethod ein:cell-active-p ((cell ein:basecell))
  (oref cell :element))

(defmethod ein:cell-running-set ((cell ein:codecell) running)
  ;; FIXME: change the appearance of the cell
  (oset cell :running running))

(defmethod ein:cell-set-collapsed ((cell ein:codecell) collapsed)
  "Set `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (oset cell :collapsed collapsed)
  (apply #'ewoc-invalidate
         (oref cell :ewoc)
         (ein:cell-element-get cell :output)))

(defmethod ein:cell-toggle-output ((cell ein:codecell))
  "Toggle `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (ein:cell-set-collapsed cell (not (oref cell :collapsed))))

(defmethod ein:cell-set-input-prompt ((cell ein:codecell) &optional number)
  (oset cell :input-prompt-number number)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))           ; disable undo recording
    (ewoc-invalidate (oref cell :ewoc)
                     (ein:cell-element-get cell :prompt))))

(defun ein:cell-finish-tooltip (cell content)
  (let* ((defstring (or (plist-get content :call_def)
                        (plist-get content :init_definition)
                        (plist-get content :definition)))
         (docstring (or (plist-get content :call_docstring)
                        (plist-get content :init_docstring)
                        (plist-get content :docstring)
                        "<empty docstring>"))
         (name (plist-get content :name))
         (tooltip (ansi-color-apply
                   (format "%s\n%s\n%s" name defstring docstring))))
    (ein:log 'debug "EIN:CELL-FINISH-TOOLTIP")
    (ein:log 'debug "tooltip: %s" tooltip)
    (cond
     ((and window-system (fboundp 'pos-tip-show))
      (funcall 'pos-tip-show tooltip))
     ((fboundp 'popup-tip)
      (funcall 'popup-tip tooltip))
     (t (when (stringp defstring)
          (message (ein:trim (ansi-color-apply defstring))))))))

(defmethod ein:cell-goto ((cell ein:basecell))
  (ewoc-goto-node (oref cell :ewoc) (ein:cell-element-get cell :input))
  ;; Skip the newline
  (forward-char))

(defmethod ein:cell-location ((cell ein:basecell) &optional elm end)
  "Return the starting location of CELL.
ELM is a name (keyword) of element in the `:element-names' slot of CELL.
If END is non-`nil', return the location of next element."
  (unless elm (setq elm :prompt))
  (let ((element (oref cell :element)))
    (when end
      (setq elm (case elm
                  (:prompt :input)
                  (:input :after-input)
                  (:output :after-output)))
      (unless elm
        (setq cell (ein:cell-next cell))
        (setq elm :prompt)))
    (if cell
        (ewoc-location (ein:cell-element-get cell elm))
      (assert end)
      (point-max))))


;; Data manipulation

(defmethod ein:cell-clear-output ((cell ein:codecell) stdout stderr other)
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
          (let ((inhibit-read-only t)
                (buffer-undo-list t))   ; disable undo recording
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
        (let ((inhibit-read-only t)
              (buffer-undo-list t))   ; disable undo recording
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
     '(output-subarea))
    (("pyerr")
     '(output-subarea))
    (("display_data")
     '(output-subarea))
    (("stream")
     (list 'output-stream 'output-subarea
           (intern (format "output-%s" (plist-get json :stream)))))))

(defmethod ein:cell-append-output ((cell ein:codecell) json dynamic)
  ;; (ein:cell-expand cell)
  ;; (ein:flush-clear-timeout)
  (oset cell :outputs
        (append (oref cell :outputs) (list json)))
  ;; enter last output element
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)           ; disable undo recording
         (ewoc (oref cell :ewoc))
         (index (1- (ein:cell-num-outputs cell)))
         (path `(cell output ,index))
         (class (ein:cell-output-json-to-class json))
         (data (ein:node-new path cell class))
         (last-node (ein:cell-element-get cell :last-output))
         (ewoc-node (ewoc-enter-after ewoc last-node data))
         (element (oref cell :element)))
    (plist-put element :output
               (append (plist-get element :output) (list ewoc-node)))
    (ewoc-invalidate ewoc (ein:cell-element-get cell :footer))))

(defmethod ein:cell-append-pyout ((cell ein:codecell) json dynamic)
  "Insert pyout type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (ein:insert-read-only (format "Out [%s]:\n"
                                (or (plist-get json :prompt_number) " ")))
  (ein:cell-append-mime-type json dynamic)
  (ein:insert-read-only "\n"))

(defmethod ein:cell-append-pyerr ((cell ein:codecell) json)
  "Insert pyerr type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (mapc (lambda (tb)
          (ein:cell-append-text tb)
          (ein:cell-append-text "\n"))
        (plist-get json :traceback))
  (ein:insert-read-only "\n"))

(defmethod ein:cell-append-stream ((cell ein:codecell) json)
  "Insert stream type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (unless (plist-get json :stream)
    (plist-put json :stream "stdout"))
  (ein:cell-append-stream-text-fontified (plist-get json :text) json)
  ;; NOTE: newlines for stream is handled in `ein:cell-insert-output'.
  ;; So do not insert newline here.
  )

(defun ein:cell-append-stream-text-fontified (text json)
  "Insert TEXT with font properties defined by JSON data."
  (if (equal (plist-get json :stream) "stderr")
      (ein:cell-append-text text 'font-lock-face 'ein:cell-output-stderr)
    (ein:cell-append-text text)))

(defmethod ein:cell-append-display-data ((cell ein:codecell) json dynamic)
  "Insert display-data type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (ein:cell-append-mime-type json dynamic)
  (ein:insert-read-only "\n"))

(defun ein:cell-append-mime-type (json dynamic)
  (loop
   for key in '(svg png jpeg latex text html javascript)
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ;; NOTE: Normally `javascript' and `html' will not be inserted as
     ;; they come out after `text'.  Maybe it is better to inform user
     ;; when one of them is inserted.
     (javascript
      (when dynamic
        (ein:log 'info (concat "ein:cell-append-mime-type does not support "
                               "dynamic javascript. got: %s") value))
      (ein:insert-read-only (plist-get json type)))
     ((html latex text)
      (ein:insert-read-only (plist-get json type)))
     (svg
      (insert-image (create-image value key t)))
     ((png jpeg)
      (insert-image (create-image (base64-decode-string value) key t))))))

(defun ein:cell-append-text (data &rest properties)
  ;; FIXME: implement HTML special escaping
  ;; escape ANSI & HTML specials in plaintext:
  (apply #'ein:insert-read-only (ansi-color-apply data) properties))

(defmethod ein:cell-to-json ((cell ein:codecell))
  "Return json-ready alist."
  `((input . ,(ein:cell-get-text cell))
    (cell_type . "code")
    ,@(ein:aif (ein:oref-safe cell :input-prompt-number)
          `((prompt_number . ,it)))
    (outputs . ,(apply #'vector (oref cell :outputs)))
    (language . "python")
    (collapsed . ,(if (oref cell :collapsed) t json-false))))

(defmethod ein:cell-to-json ((cell ein:textcell))
  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ein:cell-get-text cell))))

(defmethod ein:cell-to-json ((cell ein:headingcell))
  (let ((json (call-next-method)))
    (append json `((level . ,(oref cell :level))))))

(defmethod ein:cell-next ((cell ein:basecell))
  "Return next cell of the given CELL or nil if CELL is the last one."
  (ein:aif (ewoc-next (oref cell :ewoc)
                      (ein:cell-element-get cell :footer))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (ein:basecell-child-p cell)
          cell))))

(defmethod ein:cell-prev ((cell ein:basecell))
  "Return previous cell of the given CELL or nil if CELL is the first one."
  (ein:aif (ewoc-prev (oref cell :ewoc)
                      (ein:cell-element-get cell :prompt))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (ein:basecell-child-p cell)
          cell))))

(provide 'ein-cell)

;;; ein-cell.el ends here
