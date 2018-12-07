;;; ein-cell.el --- Cell module

;; (C) 2012 - Takafumi Arakaki
;; (C) 2017 - John M. Miller

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Author: John Miller <millejoh at mac.com>

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
(require 'ansi-color)
(require 'comint)
(require 'ein-core)
(require 'ein-classes)
(require 'ein-log)
(require 'ein-node)
(require 'ein-kernel)
(require 'ein-output-area)
(require 'ein-skewer)
(require 'ein-hy)


;;; Faces

(defface ein:cell-input-prompt
  '((t :inherit header-line))
  "Face for cell input prompt"
  :group 'ein)

(defface ein:cell-input-area
  '((((class color) (background light))
     :background "honeydew1")
    (((class color) (background dark))
     :background "#383838"))
  "Face for cell input area"
  :group 'ein)

(defface ein:cell-output-area
  '()
  "Face for cell output area"
  :group 'ein)

(defface ein:cell-output-area-error
  '()
  "Face for cell output area errors"
  :group 'ein)

(defface ein:cell-heading-1
  '((t :height 1.1 :inherit ein:cell-heading-2))
  "Face for level 1 heading."
  :group 'ein)

(defface ein:cell-heading-2
  '((t :height 1.1 :inherit ein:cell-heading-3))
  "Face for level 2 heading."
  :group 'ein)

(defface ein:cell-heading-3
  '((t :height 1.1 :inherit ein:cell-heading-4))
  "Face for level 3 heading."
  :group 'ein)

(defface ein:cell-heading-4
  '((t :height 1.1 :inherit ein:cell-heading-5))
  "Face for level 4 heading."
  :group 'ein)

(defface ein:cell-heading-5
  '((t :height 1.1 :inherit ein:cell-heading-6))
  "Face for level 5 heading."
  :group 'ein)

(defface ein:cell-heading-6
  '((t :weight bold :inherit (variable-pitch ein:cell-input-area)))
  "Face for level 6 heading."
  :group 'ein)

(defface ein:cell-output-prompt
  '((t :inherit header-line))
  "Face for cell output prompt"
  :group 'ein)

(defface ein:cell-output-stderr
  '((((class color) (background light))
     :background "PeachPuff")
    (((class color) (background dark))
     :background "#8c5353"))
  "Face for stderr cell output"
  :group 'ein)

(defface ein:pos-tip-face
  '((t (:inherit 'popup-tip-face)))
  "Face for tooltip when using pos-tip backend."
  :group 'ein)


;;; Customization

(defcustom ein:enable-dynamic-javascript nil
    "[EXPERIMENTAL] When non-nil enable support in ein for
executing dynamic javascript. This feature requires installation
of the skewer package."
    :type 'boolean
    :group 'ein)

(defcustom ein:cell-traceback-level 1
  "Number of traceback stack to show.
Hidden tracebacks are not discarded.
You can view them using \\[ein:tb-show]."
  :type '(choice (integer :tag "Depth of stack to show" 1)
                 (const :tag "Show all traceback" nil))
  :group 'ein)

(defcustom ein:cell-max-num-outputs nil
  "Number of maximum outputs to be shown by default.
To view full output, use `ein:notebook-show-in-shared-output'."
  :type '(choice (integer :tag "Number of outputs to show" 5)
                 (const :tag "Show all traceback" nil))
  :group 'ein)

(defcustom ein:cell-autoexec-prompt "⚡"
  "String shown in the cell prompt when the auto-execution flag
is on.  See also `ein:connect-aotoexec-lighter'."
  :type 'string
  :group 'ein)

(defcustom ein:slice-image nil
  "[EXPERIMENTAL] When non-`nil', use `insert-sliced-image' when
drawing images.  If it is of the form of ``(ROWS COLS)``, it is
passed to the corresponding arguments of `insert-sliced-image'.

.. FIXME: ROWS and COLS must be determined dynamically by measuring
   the size of iamge and Emacs window.

See also: https://github.com/tkf/emacs-ipython-notebook/issues/94"
  :type 'boolean
  :group 'ein)

(defcustom ein:truncate-long-cell-output nil
  "When nil do not truncate cells with long outputs. When set to
a number will limit the number of lines in a cell output."
  :type '(choice (integer :tag "Number of lines to show in a cell" 5)
                 (const :tag "Do not truncate cells with long outputs" nil))
  :group 'ein)

(defcustom ein:on-execute-reply-functions nil
  "List of functions to call after receiving an \"execute_reply\"
  message on the shell channel, just before updating the
  worksheet. Each function should have the same call signature as
  `ein:cell--handle-execute-reply'."
  :type 'list
  :group 'ein)


;;; EIEIO related utils

(defmacro ein:oset-if-empty (obj slot value)
  `(unless (and (slot-boundp ,obj ,slot) (slot-value ,obj ,slot))
     (setf (slot-value ,obj, slot) ,value)))

(defmacro ein:oref-safe (obj slot)
  `(when (slot-boundp ,obj ,slot)
     (slot-value ,obj ,slot)))


;;; Utils
(defvar ein:mime-type-map
  '((image/svg . svg) (image/png . png) (image/jpeg . jpeg)))

(defun ein:insert-image (&rest args)
  ;; Try to insert the image, otherwise emit a warning message and proceed.
  (condition-case-unless-debug err
      (let* ((img (apply #'create-image args)))
        (if ein:slice-image
            (destructuring-bind (&optional rows cols)
                                (when (listp ein:slice-image) ein:slice-image)
                                (insert-sliced-image img "." nil (or rows 20) cols))
          (insert-image img ".")))
    (error (ein:log 'warn "Could not insert image: %s" err) nil)))


;;; Cell factory

(defun ein:cell-class-from-type (type)
  (ein:case-equal type
    (("code") 'ein:codecell)
    (("hy-code") 'ein:hy-codecell)
    (("text") 'ein:textcell)
    (("html") 'ein:htmlcell)
    (("markdown") 'ein:markdowncell)
    (("raw") 'ein:rawcell)
    (("heading") 'ein:headingcell)
    ;; Defined in ein-shared-output.el:
    (("shared-output") 'ein:shared-output-cell)
    (t (error "No cell type called %S" type))))

(defun ein:get-slide-show (cell)
  (let ((slide-type (slot-value cell 'slidetype))
        (ss-table (make-hash-table)))
    (setf (gethash 'slide_type ss-table) slide-type)
    ss-table))

(defun ein:preprocess-nb4-cell (cell-data)
  (let ((source (plist-get cell-data :source)))
    (when (and  (string= (plist-get cell-data :cell_type) "markdown")
                (string-match "\\(^#+\\)" source)
                (not (string-match "\n+" source)))
      (let ((heading-level (match-end 0)))
        (plist-put cell-data :cell_type "heading")
        (plist-put cell-data :level heading-level)
        (plist-put cell-data :source (substring source (1+ heading-level))))))
  cell-data)

(defun ein:cell-from-type (type &rest args)
  (apply (ein:cell-class-from-type type) args))

(defun ein:cell--determine-cell-type (json-data)
  (let ((base-type (plist-get json-data :cell_type))
        (metadata (plist-get json-data :metadata)))
    (if (and (string-equal base-type "code")
             (plist-get :metadata :ein.hycell)
             (not (eql (plist-get metadata :ein.hycell) :json-false)))
        "hy-code"
      base-type)))

(defun ein:cell-from-json (data &rest args)
  (let ((data (ein:preprocess-nb4-cell data))
        (cell (ein:cell-init (apply #'ein:cell-from-type
					                          (ein:cell--determine-cell-type data) args)
				                     data)))
    (when (plist-get data :metadata)
      (ein:oset-if-empty cell 'metadata (plist-get data :metadata))
      (ein:aif (plist-get (slot-value cell 'metadata) :slideshow)
          (let ((slide-type (nth 0 (cdr it))))
            (setf (slot-value cell 'slidetype) slide-type))))
    cell))

(cl-defmethod ein:cell-init ((cell ein:codecell) data)
  (ein:oset-if-empty cell 'outputs (plist-get data :outputs))
  (ein:oset-if-empty cell 'input (or (plist-get data :input)
                                     (plist-get data :source)))
  (ein:aif (plist-get data :prompt_number)
      (ein:oset-if-empty cell 'input-prompt-number it)
    (ein:aif (plist-get data :execution_count)
        (ein:oset-if-empty cell 'input-prompt-number it)))
  (ein:oset-if-empty cell 'collapsed
                     (let ((v (or (plist-get data :collapsed)
                                  (plist-get (slot-value cell 'metadata)
                                             :collapsed))))
                       (if (eql v json-false) nil v)))
  cell)

(cl-defmethod ein:cell-init ((cell ein:textcell) data)
  (ein:aif (plist-get data :source)
      (setf (slot-value cell 'input) it))
  cell)

(cl-defmethod ein:cell-init ((cell ein:headingcell) data) ;; FIXME: Was :after method
  (cl-call-next-method)
  (ein:aif (plist-get data :level)
      (setf (slot-value cell 'level) it))
  cell)

(cl-defmethod ein:cell-convert ((cell ein:basecell) type)
  (let ((new (ein:cell-from-type type)))
    ;; copy attributes
    (loop for k in '(read-only ewoc)
      do (setf (slot-value new k) (slot-value cell k)))
    ;; copy input
    (setf (slot-value new 'input) (if (ein:cell-active-p cell)
                                      (ein:cell-get-text cell)
                                    (slot-value cell 'input)))
    ;; copy slidetype
    (setf (slot-value new 'slidetype) (slot-value cell 'slidetype))
    ;; copy output when the new cell has it
    (when (memq :output (slot-value new 'element-names))
      (setf (slot-value new 'outputs) (mapcar 'identity (slot-value cell 'outputs))))
    new))

(cl-defmethod ein:cell-convert ((cell ein:codecell) type)
  (let ((new (cl-call-next-method)))
    (when (and (cl-typep new 'ein:codecell)
               (slot-boundp cell :kernel))
      (setf (slot-value new 'kernel) (slot-value cell 'kernel)))
    new))

(cl-defmethod ein:cell-convert ((cell ein:headingcell) type)
  (let ((new (cl-call-next-method)))
    (when (ein:headingcell-p new)
      (setf (slot-value new 'level) (slot-value cell 'level)))
    new))

(cl-defmethod ein:cell-copy ((cell ein:basecell))
  (ein:cell-convert cell (slot-value cell 'cell-type)))

(cl-defmethod ein:cell-convert-inplace ((cell ein:basecell) type)
  "Convert CELL to TYPE and redraw corresponding ewoc nodes."
  (let ((new (ein:cell-convert cell type)))
    ;; copy element attribute
    (loop for k in (slot-value new 'element-names)
          with old-element = (slot-value cell 'element)
          do (progn
               (setf (slot-value new 'element)
                     (plist-put (slot-value new 'element) k
                                (plist-get old-element k)))
	       )
	  )
    ;; setting ewoc nodes
    (loop for en in (ein:cell-all-element cell)
          for node = (ewoc-data en)
          do (setf (ein:$node-data node) new))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))         ; disable undo recording
      ;; delete ewoc nodes that is not copied
      (apply
       #'ewoc-delete (slot-value new 'ewoc)
       (apply
        #'append
        (loop for name in (slot-value cell 'element-names)
              unless (memq name (slot-value new 'element-names))
              collect (let ((ens (ein:cell-element-get cell name)))
                        (if (listp ens) ens (list ens))))))
      ;; draw ewoc node
      (loop with ewoc = (slot-value new 'ewoc)
            for en in (ein:cell-all-element new)
            do (ewoc-invalidate ewoc en)))
    new))

(cl-defmethod ein:cell-change-level ((cell ein:headingcell) level)
  (assert (integerp level))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))         ; disable undo recording
    (setf (slot-value cell 'level) level)
    ;; draw ewoc node
    (loop with ewoc = (slot-value cell 'ewoc)
          for en in (ein:cell-all-element cell)
          do (ewoc-invalidate ewoc en))))


;;; Getter/setter

(cl-defmethod ein:cell-num-outputs ((cell ein:codecell))
  (length (slot-value cell 'outputs)))

(cl-defmethod ein:cell-num-outputs ((cell ein:textcell))
  0)

(cl-defmethod ein:cell-element-get ((cell ein:basecell) prop &rest args)
  "Return ewoc node named PROP in CELL.
  If PROP is `:output' a list of ewoc nodes is returned.
  A specific node can be specified using optional ARGS."
  (if (memq prop (slot-value cell 'element-names))
      (plist-get (slot-value cell 'element) prop)
    (error "PROP %s is not supported." prop)))

(cl-defmethod ein:cell-element-get ((cell ein:codecell) prop &optional index)
  (let ((element (slot-value cell 'element)))
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
        (t (cl-call-next-method))))))

(cl-defmethod ein:cell-element-get ((cell ein:textcell) prop &rest args)
  (let ((element (slot-value cell 'element)))
    (case prop
      (:after-input (plist-get element :footer))
      (:before-input (plist-get element :prompt))
      (t (cl-call-next-method)))))

(cl-defmethod ein:cell-all-element ((cell ein:basecell))
  (list (ein:cell-element-get cell :prompt)
        (ein:cell-element-get cell :input)
        (ein:cell-element-get cell :footer)))

(cl-defmethod ein:cell-all-element  ((cell ein:codecell))
  (append (cl-call-next-method)
          (ein:cell-element-get cell :output)))

(cl-defmethod ein:cell-language ((cell ein:basecell))
  "Programming language used for CELL.
Return language name as a string or `nil' when not defined.
  (fn cell)")

(cl-defmethod ein:cell-language ((cell ein:codecell))
  (ein:and-let* ((kernel (slot-value cell 'kernel))
                 (kernelspec (ein:$kernel-kernelspec kernel)))
    (ein:$kernelspec-language kernelspec)))
(cl-defmethod ein:cell-language ((cell ein:markdowncell)) nil "markdown")
(cl-defmethod ein:cell-language ((cell ein:htmlcell)) nil "html")
(cl-defmethod ein:cell-language ((cell ein:rawcell)) nil "rst")


;; EWOC

(defun ein:cell-make-element (make-node num-outputs)
  (let ((buffer-undo-list t))           ; disable undo recording
    (list
     :prompt (funcall make-node 'prompt)
     :input  (funcall make-node 'input)
     :output (loop for i from 0 below num-outputs
                   collect (funcall make-node 'output i))
     :footer (funcall make-node 'footer))))

(cl-defmethod ein:cell-enter-last ((cell ein:basecell))
  (let* ((ewoc (slot-value cell 'ewoc))
         ;; Use `cell' as data for ewoc.  Use the whole cell data even
         ;; if it is not used, to access it from the notebook buffer.
         ;; It is equivalent to `this.element.data("cell", this)' in
         ;; IPython.Cell (see cell.js).
         (make-node
          (lambda (&rest path)
            (ewoc-enter-last ewoc (ein:node-new `(cell ,@path) cell))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs cell))))
    (setf (slot-value cell 'element) element)
    cell))

(cl-defmethod ein:cell-enter-first ((cell ein:basecell))
  (let* ((ewoc (slot-value cell 'ewoc))
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
    (setf (slot-value cell 'element) element)
    cell))

(cl-defmethod ein:cell-insert-below ((base-cell ein:basecell) other-cell)
  (let* ((ewoc (slot-value base-cell 'ewoc))
         (node (ein:cell-element-get base-cell :footer))
         (make-node
          (lambda (&rest path)
            (setq node (ewoc-enter-after
                        ewoc node (ein:node-new `(cell ,@path) other-cell)))))
         (element (ein:cell-make-element make-node
                                         (ein:cell-num-outputs other-cell))))
    (setf (slot-value other-cell 'element) element)
    other-cell))

(defun ein:cell-pp (path data)
  (case (car path)
    (prompt (ein:cell-insert-prompt data))
    (input  (ein:cell-insert-input data))
    (output (ein:cell-insert-output (cadr path) data))
    (footer (ein:cell-insert-footer data))))

(defun ein:maybe-show-slideshow-data (cell)
  (when (ein:worksheet--show-slide-data-p ein:%worksheet%)
    (format " - Slide [%s]:" (or (ein:oref-safe cell 'slidetype)  " "))))

(cl-defmethod ein:cell-insert-prompt ((cell ein:codecell))
  "Insert prompt of the CELL in the buffer.
  Called from ewoc pretty printer via `ein:cell-pp'."
  ;; Newline is inserted in `ein:cell-insert-input'.
  (ein:insert-read-only
   (concat
    (format "In [%s]:" (or (ein:oref-safe cell 'input-prompt-number)  " "))
    (ein:maybe-show-slideshow-data cell)
    (when (slot-value cell 'autoexec) " %s" ein:cell-autoexec-prompt))
   'font-lock-face 'ein:cell-input-prompt))

(cl-defmethod ein:cell-insert-prompt ((cell ein:textcell))
  (ein:insert-read-only
   (concat
    (format "%s:" (slot-value cell 'cell-type))
    (ein:maybe-show-slideshow-data cell))
   'font-lock-face 'ein:cell-input-prompt))

(cl-defmethod ein:cell-insert-prompt ((cell ein:headingcell))
  (ein:insert-read-only
   (concat
    (format "h%s:" (slot-value cell 'level))
    (ein:maybe-show-slideshow-data cell))
   'font-lock-face 'ein:cell-input-prompt))

(cl-defmethod ein:cell-insert-input ((cell ein:basecell))
  "Insert input of the CELL in the buffer.
  Called from ewoc pretty printer via `ein:cell-pp'."
  (let ((start (1+ (point))))
    ;; Newlines must allow insertion before/after its position.
    (insert (propertize "\n" 'read-only t 'rear-nonsticky t)
            (or (ein:oref-safe cell 'input) "")
            (propertize "\n" 'read-only t))
    ;; Highlight background using overlay.
    (let ((ol (make-overlay start (point))))
      (overlay-put ol 'face (ein:cell-get-input-area-face cell))
      ;; `evaporate' = `t': Overlay is deleted when the region become empty.
      (overlay-put ol 'evaporate t))))

(cl-defmethod ein:cell-get-input-area-face ((cell ein:basecell))
  "Return the face (symbol) for input area."
  'ein:cell-input-area)

(cl-defmethod ein:cell-get-input-area-face ((cell ein:headingcell))
  (intern (format "ein:cell-heading-%d" (slot-value cell 'level))))

(cl-defmethod ein:cell-get-output-area-face-for-output-type (output-type)
  "Return the face (symbol) for output area."
  (ein:case-equal output-type
    (("pyout")          'ein:cell-output-area)
    (("pyerr")          'ein:cell-output-area-error)
    (("error")          'ein:cell-output-area-error)
    (("display_data")   'ein:cell-output-area)
    (("execute_result") 'ein:cell-output-area)
    (("stream")         'ein:cell-output-area)))

(defun ein:cell-insert-output (index cell)
  "Insert INDEX-th output of the CELL in the buffer.
  Called from ewoc pretty printer via `ein:cell-pp'."
  (if (or (slot-value cell 'collapsed)
          (and ein:cell-max-num-outputs
               (>= index ein:cell-max-num-outputs)))
      (progn
        (when (and (not (slot-value cell 'collapsed))
                   (= index ein:cell-max-num-outputs)
                   (> (point) (point-at-bol)))
          ;; The first output which exceeds `ein:cell-max-num-outputs'.
          (ein:insert-read-only "\n"))
        (ein:insert-read-only "."))
    (let ((out (nth index (slot-value cell 'outputs))))
      ;; Handle newline for previous stream output.
      ;; In IPython JS, it is handled in `append_stream' because JS
      ;; does not need to care about newline (DOM does it for JS).
      ;; FIXME: Maybe I should abstract ewoc in some way and get rid
      ;;        of this.
      (let ((last-out (and (> index 0)
                           (nth (1- index) (slot-value cell 'outputs)))))
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
      (let ((start (point))
	    (output-type (plist-get out :output_type)))
	(ein:case-equal output-type
	  (("pyout")          (ein:cell-append-pyout        cell out))
	  (("pyerr")          (ein:cell-append-pyerr        cell out))
	  (("error")          (ein:cell-append-pyerr        cell out))
	  (("display_data")   (ein:cell-append-display-data cell out))
	  (("execute_result") (ein:cell-append-pyout        cell out))
	  (("stream")         (ein:cell-append-stream       cell out)))
	(let ((ol (make-overlay start (point))))
	  (overlay-put ol 'face (ein:cell-get-output-area-face-for-output-type output-type))
	  (overlay-put ol 'evaporate t))))))

(cl-defmethod ein:cell-insert-footer ((cell ein:basecell))
  "Insert footer (just a new line) of the CELL in the buffer.
  Called from ewoc pretty printer via `ein:cell-pp'."
  (ein:insert-read-only "\n"))

(cl-defmethod ein:cell-insert-footer :before ((cell ein:codecell))
  (if (or (slot-value cell 'collapsed)
          (and ein:cell-max-num-outputs
               (> (ein:cell-num-outputs cell) ein:cell-max-num-outputs)))
      ;; Add a newline after the last ".".
      (ein:insert-read-only "\n")
    (let ((last-out (car (last (slot-value cell 'outputs)))))
      (when (equal (plist-get last-out :output_type) "stream")
        (ein:cell-append-stream-text-fontified "\n" last-out)))))


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

(cl-defmethod ein:cell-input-pos-min ((cell ein:basecell))
  "Return editable minimum point in the input area of the CELL.
  If the input area of the CELL does not exist, return `nil'"
  (let* ((input-node (ein:cell-element-get cell :input)))
    ;; 1+ for skipping newline
    (when input-node (1+ (ewoc-location input-node)))))

(cl-defmethod ein:cell-input-pos-max ((cell ein:basecell))
  "Return editable maximum point in the input area of the CELL.
  If the input area of the CELL does not exist, return `nil'"
  (let* ((ewoc (slot-value cell 'ewoc))
         (input-node (ein:cell-element-get cell :input)))
    ;; 1- for skipping newline
    (when input-node (1- (ewoc-location (ewoc-next ewoc input-node))))))

(cl-defmethod ein:cell-get-text ((cell ein:basecell))
  "Grab text in the input area of the cell at point."
  (if (ein:cell-active-p cell)
      (let* ((beg (ein:cell-input-pos-min cell))
             (end (ein:cell-input-pos-max cell)))
        (buffer-substring beg end))
    (slot-value cell 'input)))

(cl-defmethod ein:cell-set-text ((cell ein:basecell) text)
  (let* ((input-node (ein:cell-element-get cell :input))
         (ewoc (slot-value cell 'ewoc))
           ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (save-excursion
      ;; probably it is better to set :input and update via ewoc?
      (goto-char beg)
      (delete-region beg end)
      (insert text))))

(cl-defmethod ein:cell-save-text ((cell ein:basecell))
  (setf (slot-value cell 'input) (ein:cell-get-text cell)))

(cl-defmethod ein:cell-deactivate ((cell ein:basecell))
  (setf (slot-value cell 'element) nil)
  cell)

(cl-defmethod ein:cell-active-p ((cell ein:basecell))
  (slot-value cell 'element))

(cl-defmethod ein:cell-running-set ((cell ein:codecell) running)
  ;; FIXME: change the appearance of the cell
  (setf (slot-value cell 'running) running))

(cl-defmethod ein:cell-set-collapsed ((cell ein:codecell) collapsed)
  "Set `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (unless (eq (slot-value cell 'collapsed) collapsed)
    (setf (slot-value cell 'collapsed) collapsed)
    (apply #'ewoc-invalidate
           (slot-value cell 'ewoc)
           (ein:cell-element-get cell :output))))

(cl-defmethod ein:cell-collapse ((cell ein:codecell))
  (ein:cell-set-collapsed cell t))

(cl-defmethod ein:cell-expand ((cell ein:codecell))
  (ein:cell-set-collapsed cell nil))

(cl-defmethod ein:cell-toggle-output ((cell ein:codecell))
  "Toggle `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (ein:cell-set-collapsed cell (not (slot-value cell 'collapsed))))

(cl-defmethod ein:cell-invalidate-prompt ((cell ein:codecell))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))           ; disable undo recording
    (ewoc-invalidate (slot-value cell 'ewoc)
                     (ein:cell-element-get cell :prompt))))

(cl-defmethod ein:cell-set-input-prompt ((cell ein:codecell) &optional number)
  (setf (slot-value cell 'input-prompt-number) number)
  (ein:cell-invalidate-prompt cell))

(cl-defmethod ein:cell-set-autoexec ((cell ein:codecell) bool)
  "Set auto-execution flag of CELL to BOOL and invalidate the
prompt EWOC node."
  (setf (slot-value cell 'autoexec) bool)
  (ein:cell-invalidate-prompt cell))

(cl-defmethod ein:cell-autoexec-p ((cell ein:basecell))
  "Auto-execution flag set to CELL.
Return `nil' always for non-code cells."
  nil)

(cl-defmethod ein:cell-autoexec-p ((cell ein:codecell))
  (slot-value cell 'autoexec))

(cl-defmethod ein:cell-toggle-autoexec ((cell ein:codecell))
  "Toggle auto-execution flag of CELL to BOOL and invalidate the
prompt EWOC node."
  (ein:cell-set-autoexec cell (not (ein:cell-autoexec-p cell))))

(cl-defmethod ein:cell-goto ((cell ein:basecell) &optional relpos prop)
  "Go to the input area of the given CELL.
RELPOS is the position relative to the input area.  Default is 0.
PROP is a name of cell element.  Default is `:input'.

\(fn cell relpos prop)"
  (unless relpos (setq relpos 0))
  (unless prop (setq prop :input))
  (ewoc-goto-node (slot-value cell 'ewoc) (ein:cell-element-get cell prop))
  (let ((offset (case prop
                  ((:input :before-output) 1)
                  (:after-input -1)
                  (t 0))))
    (forward-char (+ relpos offset))))

(cl-defmethod ein:cell-goto-line ((cell ein:basecell) &optional inputline prop)
  "Go to the input area of the given CELL.
INPUTLINE is the line number relative to the input area.  Default is 1.
PROP is a name of cell element.  Default is `:input'.

\(fn cell inputline prop)"
  (unless inputline (setq inputline 1))
  (unless prop (setq prop :input))
  (let ((goal-column nil))
    (ewoc-goto-node (slot-value cell 'ewoc) (ein:cell-element-get cell prop)))
  (let ((offset (case prop
                  ((:input :before-output) 1)
                  (:after-input -1)
                  (t 0))))
    (forward-char offset)
    (forward-line (- inputline 1))))


(cl-defmethod ein:cell-relative-point ((cell ein:basecell) &optional pos)
  "Return the point relative to the input area of CELL.
If the position POS is not given, current point is considered."
  (unless pos (setq pos (point)))
  (- pos (1+ (ewoc-location (ein:cell-element-get cell :input)))))

(cl-defmethod ein:cell-location ((cell ein:basecell) &optional elm end)
  "Return the starting location of CELL.
ELM is a name (keyword) of element that `ein:cell-element-get'
understands.  Note that you can't use `:output' since it returns
a list.  Use `:after-input' instead.
If END is non-`nil', return the location of next element."
  (unless elm (setq elm :prompt))
  (let ((element (slot-value cell 'element)))
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

(cl-defmethod ein:cell-buffer ((cell ein:basecell))
  "Return a buffer associated by CELL (if any)."
  (ein:aand (ein:oref-safe cell 'ewoc) (ewoc-buffer it)))


;; Data manipulation

(cl-defmethod ein:cell-clear-output ((cell ein:codecell) stdout stderr other)
  ;; codecell.js in IPython implements it using timeout and callback.
  ;; As it is unclear why timeout is needed, just clear output
  ;; instantaneously for now.
  (ein:log 'debug "cell-clear-output stdout=%s stderr=%s other=%s"
           stdout stderr other)
  (setf (slot-value cell 'traceback) nil)
  (let ((ewoc (slot-value cell 'ewoc))
        (output-nodes (ein:cell-element-get cell :output)))
    (if (and stdout stderr other)
        (progn
          ;; clear all
          (let ((inhibit-read-only t)
                (buffer-undo-list t))   ; disable undo recording
            (apply #'ewoc-delete ewoc output-nodes))
          (plist-put (slot-value cell 'element) :output nil)
          (setf (slot-value cell 'outputs) nil))
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
        (let* ((element (slot-value cell 'element))
               (old-output (plist-get element :output))
               (new-ouptut (ein:remove-by-index old-output indices)))
          (plist-put element :output new-ouptut))
        ;; remove cleared outputs from internal data
        (setf (slot-value cell 'outputs)
              (ein:remove-by-index (slot-value cell 'outputs) indices))))
    ;; Footer may have extra (possibly colored) newline due to the
    ;; last output type.  So invalidate it here.
    ;; See `ein:cell-insert-footer' (for codecell).
    (let ((buffer-undo-list t))   ; disable undo recording
      (ewoc-invalidate ewoc (ein:cell-element-get cell :footer)))))

(defun ein:cell-output-json-to-class (json)
  (ein:case-equal (plist-get json :output_type)
    (("pyout")
     '(output-subarea))
    (("pyerr")
     '(output-subarea))
    (("error")
     '(output-subarea))
    (("display_data")
     '(output-subarea))
    (("execute_result")
     '(output-subarea))
    (("stream")
     (list 'output-stream 'output-subarea
           (intern (format "output-%s" (plist-get json :stream)))))))

(cl-defmethod ein:cell-append-output ((cell ein:codecell) json dynamic)
  ;; When there is a python error, we actually get two identical tracebacks back
  ;; from the kernel, one from the "shell" channel, and one from the "iopub"
  ;; channel.  As a workaround, we remember the cell's traceback and ignore
  ;; traceback outputs that are identical to the one we already have.
  (let ((new-tb (plist-get json :traceback))
        (old-tb (slot-value cell 'traceback)))
    (when (or
           (null old-tb)
           (null new-tb)
           (not (equalp new-tb old-tb)))
      (ein:cell-actually-append-output cell json dynamic))
    (setf (slot-value cell 'traceback) new-tb)))

(cl-defmethod ein:cell-actually-append-output ((cell ein:codecell) json dynamic)
  (ein:cell-expand cell)
  ;; (ein:flush-clear-timeout)
  (setf (slot-value cell 'outputs)
        (append (slot-value cell 'outputs) (list json)))
  ;; enter last output element
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)           ; disable undo recording
         (ewoc (slot-value cell 'ewoc))
         (index (1- (ein:cell-num-outputs cell)))
         (path `(cell output ,index))
         (class (ein:cell-output-json-to-class json))
         (data (ein:node-new path cell class))
         (last-node (ein:cell-element-get cell :last-output))
         (ewoc-node (ewoc-enter-after ewoc last-node data))
         (element (slot-value cell 'element)))
    (plist-put element :output
               (append (plist-get element :output) (list ewoc-node)))
    (ewoc-invalidate ewoc (ein:cell-element-get cell :footer))))

(cl-defmethod ein:cell-append-pyout ((cell ein:codecell) json)
  "Insert pyout type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (ein:insert-read-only (format "Out [%s]:"
                                (or (plist-get json :prompt_number) " "))
                        'font-lock-face 'ein:cell-output-prompt)
  (ein:insert-read-only "\n")
  (ein:cell-append-mime-type json (slot-value cell 'dynamic))
  (ein:insert-read-only "\n"))

(cl-defmethod ein:cell-append-pyerr ((cell ein:codecell) json)
  "Insert pyerr type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (mapc (lambda (tb)
          (ein:cell-append-text tb)
          (ein:cell-append-text "\n"))
        (let ((tb (plist-get json :traceback))
              (level ein:cell-traceback-level))
          (if (and level (> (- (length tb) 2) level))
              (cons (substitute-command-keys
                     "\nTruncated Traceback (Use \\[ein:tb-show] to view full TB):")
                    (last tb (1+ level)))
            tb)))
  (ein:insert-read-only "\n"))

(ein:deflocal ein:%cell-append-stream-last-cell% nil
  "The last cell in which `ein:cell-append-stream' is used.")

(cl-defmethod ein:cell-append-stream ((cell ein:codecell) json)
  "Insert stream type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  ;; (unless (plist-get json :stream)
  ;;   (plist-put json :stream "stdout"))
  (unless (eq cell ein:%cell-append-stream-last-cell%)
    ;; Avoid applying unclosed ANSI escape code in the cell.  Note
    ;; that I don't need to distinguish stdout/stderr because it looks
    ;; like normal terminal does not.
    (setq ansi-color-context nil))
  (let ((start (point)))
    (ein:cell-append-stream-text-fontified (or (plist-get json :text) "") json)
    (comint-carriage-motion start (point)))
  ;; NOTE: newlines for stream is handled in `ein:cell-insert-output'.
  ;; So do not insert newline here.
  (setq ein:%cell-append-stream-last-cell% cell))

(defun ein:cell-append-stream-text-fontified (text json)
  "Insert TEXT with font properties defined by JSON data."
  (if (equal (plist-get json :stream) "stderr")
      (ein:cell-append-text text 'font-lock-face 'ein:cell-output-stderr)
    (ein:cell-append-text text)))

(cl-defmethod ein:cell-append-display-data ((cell ein:codecell) json)
  "Insert display-data type output in the buffer.
Called from ewoc pretty printer via `ein:cell-insert-output'."
  (if (and (plist-get json :javascript)
           (slot-value cell 'dynamic) ein:enable-dynamic-javascript)
      (ein:execute-javascript cell json)
    (progn
      (ein:cell-append-mime-type json (slot-value cell 'dynamic))
      (ein:insert-read-only "\n"))))

(defcustom ein:output-type-preference
  (if (and (fboundp 'shr-insert-document)
           (fboundp 'libxml-parse-xml-region))
      #'ein:output-type-prefer-pretty-text-over-html
    '(emacs-lisp svg image/svg png image/png jpeg image/jpeg html text/html latex text/latex javascript text/javascript text text/plain))
  "Output types to be used in notebook.
First output-type found in this list will be used.
This variable can be a list or a function returning a list given
DATA plist.
See also `ein:output-type-prefer-pretty-text-over-html'.

**Example**:
If you prefer HTML type over text type, you can set it as::

    (setq ein:output-type-preference
          '(emacs-lisp svg png jpeg html text latex javascript))

Note that ``html`` comes before ``text``."
  :type '(choice function (repeat symbol))
  :group 'ein)

(defvar ein:output-types-text-preferred
  '(emacs-lisp svg image/svg png image/png jpeg image/jpeg text text/plain html text/html latex text/latex javascript text/javascript))

(defvar ein:output-types-html-preferred
  '(emacs-lisp svg image/svg png image/png jpeg image/jpeg html text/html latex text/latex javascript text/javascript text text/plain))

(defun ein:output-type-prefer-pretty-text-over-html (data)
  "Use text type if it is a \"prettified\" text instead of HTML.
This is mostly for *not* using HTML table for pandas but using
HTML for other object.

If the text type output contains a newline, it is assumed be a
prettified text thus be used instead of HTML type."
  (if (ein:aand (plist-get data :text) (string-match-p "\n" it))
      ein:output-types-text-preferred
    ein:output-types-html-preferred))

(defun ein:fix-mime-type (type)
  (ein:aif (assoc type ein:mime-type-map)
      (cdr it)
    type))

(defun ein:cell-append-mime-type (json dynamic)
  (when (plist-get json :data)
    (setq json (plist-get json :data))) ;; For nbformat v4 support.
  (loop
   for key in (cond
               ((functionp ein:output-type-preference)
                (funcall ein:output-type-preference json))
               (t ein:output-type-preference))
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ;; NOTE: Normally `javascript' and `html' will not be inserted as
     ;; they come out after `text'.  Maybe it is better to inform user
     ;; when one of them is inserted.
     ((javascript text/javascript)
      (when dynamic
        (ein:log 'info (concat "ein:cell-append-mime-type does not support "
                               "dynamic javascript. got: %s") value))
      (ein:insert-read-only (plist-get json type)))
     (emacs-lisp
      (when dynamic
        (ein:cell-safe-read-eval-insert (plist-get json type))))
     ((html text/html)
      (funcall (ein:output-area-get-html-renderer) (plist-get json type)))
     ((latex text/latex text text/plain)
      (ein:insert-read-only (plist-get json type)))
     ((svg image/svg)
      (ein:insert-image value (ein:fix-mime-type key) t))
     ((png image/png jpeg image/jpeg)
      (ein:insert-image (base64-decode-string value) (ein:fix-mime-type key) t)))))

(defun ein:cell-append-text (data &rest properties)
  ;; escape ANSI in plaintext:
  (apply #'ein:insert-read-only (ansi-color-apply data) properties))

(defun ein:cell-safe-read-eval-insert (text)
  (ein:insert-read-only
   (condition-case err
       (save-excursion
         ;; given code can be `pop-to-buffer' or something.
         (format "%S" (eval (read text))))
     (error
      (ein:log 'warn "Got an error while executing: '%s'"
               text)
      (format "Error: %S" err)))))

(cl-defmethod ein:cell-to-json ((cell ein:codecell) &optional discard-output)
  "Return json-ready alist."
  `((input . ,(ein:cell-get-text cell))
    (cell_type . "code")
    ,@(ein:aif (ein:oref-safe cell 'input-prompt-number)
          `((prompt_number . ,it)))
    (outputs . ,(if discard-output [] (apply #'vector (slot-value cell 'outputs))))
    (language . "python")
    (collapsed . ,(if (slot-value cell 'collapsed) t json-false))))


(defvar ein:output-type-map
  '((:svg . :image/svg) (:png . :image/png) (:jpeg . :image/jpeg)
    (:text . :text/plain)
    (:html . :text/html) (:latex . :text/latex) (:javascript . :text/javascript)))

(defun ein:output-property-p (maybe-property)
  (assoc maybe-property ein:output-type-map))

(cl-defmethod ein:cell-to-nb4-json ((cell ein:codecell) wsidx &optional discard-output)
  (let* ((ss-table (ein:get-slide-show cell))
         (metadata (slot-value cell 'metadata))
         (outputs (if discard-output []
                    (slot-value cell 'outputs)))
         (renamed-outputs '())
         (execute-count (ein:aif (ein:oref-safe cell 'input-prompt-number)
                            (and (numberp it) it))))
    (setq metadata (plist-put metadata :collapsed (if (slot-value cell 'collapsed) t json-false)))
    (setq metadata (plist-put metadata :autoscroll json-false))
    (setq metadata (plist-put metadata :ein.tags (format "worksheet-%s" wsidx)))
    (setq metadata (plist-put metadata :ein.hycell (if (ein:hy-codecell-p cell)
                                                       t
                                                     json-false)))
    (setq metadata (plist-put metadata :slideshow ss-table))
    (unless discard-output
      (dolist (output outputs)
        (let ((otype (plist-get output :output_type)))
          (ein:log 'debug "Saving output of type %S" otype)
          (if (and (or (equal otype "display_data")
                       (equal otype "execute_result"))
                   (null (plist-get output :metadata)))
              (plist-put output :metadata (make-hash-table)))
          (setq renamed-outputs
                (append renamed-outputs
                        (list (let ((ocopy (copy-list output))
                                    (new-output '()))
                                (loop while ocopy
                                      do (let ((prop (pop ocopy))
                                               (value (pop ocopy)))
                                           (ein:log 'debug "Checking property %s for output type '%s'"
                                                    prop otype)
                                           (cond
                                            ((equal prop :stream) (progn (push value new-output)
                                                                         (push :name new-output)))

                                            ((and (equal otype "display_data")
                                                  (ein:output-property-p prop))
                                             (let ((new-prop (cdr (ein:output-property-p prop))))
                                               (if (plist-member new-output :data)
                                                   (setq new-output (plist-put new-output :data
                                                                               (append (plist-get new-output :data)
                                                                                       (list new-prop (list value))
                                                                                       )))
                                                 (push (list new-prop (list value)) new-output)
                                                 (push :data new-output))
                                               ))

                                            ((and (equal otype "display_data")
                                                  (equal prop :text))
                                             (ein:log 'debug "SAVE-NOTEBOOK: Skipping unnecessary :text data."))

                                            ((and (equal otype "execute_result")
                                                  (or (equal prop :text)
                                                      (equal prop :html)
                                                      (equal prop :latex)))
                                             (ein:log 'debug "Fixing execute_result (%s?)." otype)
                                             (let ((new-prop (cdr (ein:output-property-p prop))))
                                               (push (list new-prop (list value)) new-output)
                                               (push :data new-output)))


                                            ((and (equal otype "execute_result")
                                                  (equal prop :prompt_number))
                                             (ein:log 'debug "SAVE-NOTEBOOK: Fixing prompt_number property.")
                                             (push value new-output)
                                             (push :execution_count new-output))

                                            (t (progn (push value new-output) (push prop new-output)))))
                                      finally return new-output))))
                ))))
    `((source . ,(ein:cell-get-text cell))
      (cell_type . "code")
      ,@(if execute-count
            `((execution_count . ,execute-count))
          `((execution_count)))
      (outputs . ,(apply #'vector (or renamed-outputs outputs)))
      (metadata . ,metadata))))


(cl-defmethod ein:cell-to-json ((cell ein:textcell) &optional discard-output)
  `((cell_type . ,(slot-value cell 'cell-type))
    (source    . ,(ein:cell-get-text cell))))

(cl-defmethod ein:cell-to-nb4-json ((cell ein:textcell) wsidx &optional discard-output)
  (let ((metadata (slot-value cell 'metadata))
        (ss-table (ein:get-slide-show cell)))
    (setq metadata (plist-put metadata :ein.tags (format "worksheet-%s" wsidx)))
    (setq metadata (plist-put metadata :slideshow ss-table))
    `((cell_type . ,(slot-value cell 'cell-type))
      (source    . ,(ein:cell-get-text cell))
      (metadata . ,metadata))))

(cl-defmethod ein:cell-to-nb4-json ((cell ein:headingcell) wsidx &optional discard-output)
  (let ((metadata (slot-value cell 'metadata))
        (ss-table (ein:get-slide-show cell))
        (header (make-string (slot-value cell 'level) ?#)))
    (setq metadata (plist-put metadata :ein.tags (format "worksheet-%s" wsidx)))
    (setq metadata (plist-put metadata :slideshow ss-table))
    `((cell_type . "markdown")
      (source .  ,(format "%s %s" header (ein:cell-get-text cell)))
      (metadata . ,metadata))))

(cl-defmethod ein:cell-to-json ((cell ein:headingcell) &optional discard-output)
  (let ((json (cl-call-next-method)))
    (append json `((level . ,(slot-value cell 'level))))))

(cl-defmethod ein:cell-next ((cell ein:basecell))
  "Return next cell of the given CELL or nil if CELL is the last one."
  (ein:aif (ewoc-next (slot-value cell 'ewoc)
                      (ein:cell-element-get cell :footer))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (cl-typep cell 'ein:basecell)
          cell))))

(cl-defmethod ein:cell-prev ((cell ein:basecell))
  "Return previous cell of the given CELL or nil if CELL is the first one."
  (ein:aif (ewoc-prev (slot-value cell 'ewoc)
                      (ein:cell-element-get cell :prompt))
      (let ((cell (ein:$node-data (ewoc-data it))))
        (when (cl-typep cell 'ein:basecell)
          cell))))


;;; Kernel related calls.

(cl-defmethod ein:cell-set-kernel ((cell ein:codecell) kernel)
  (setf (slot-value cell 'kernel) kernel))


(cl-defmethod ein:cell-execute ((cell ein:codecell))
  (ein:cell-execute-internal cell
                             (slot-value cell 'kernel)
                             (ein:cell-get-text cell)
                             :silent nil))

(cl-defmethod ein:cell-execute-internal ((cell ein:codecell)
                                         kernel code &rest args)
  (ein:cell-running-set cell t)
  (ein:cell-clear-output cell t t t)
  (ein:cell-set-input-prompt cell "*")
  (setf (slot-value cell 'dynamic) t)
  (apply #'ein:kernel-execute kernel code (ein:cell-make-callbacks cell) args))

(cl-defmethod ein:cell-make-callbacks ((cell ein:codecell))
  (list
   :execute_reply  (cons #'ein:cell--handle-execute-reply  cell)
   :output         (cons #'ein:cell--handle-output         cell)
   :clear_output   (cons #'ein:cell--handle-clear-output   cell)
   :set_next_input (cons #'ein:cell--handle-set-next-input cell)))

(cl-defmethod ein:cell--handle-execute-reply ((cell ein:codecell) content
                                              metadata)
  (run-hook-with-args 'ein:on-execute-reply-functions cell content metadata)
  (ein:cell-set-input-prompt cell (plist-get content :execution_count))
  (ein:cell-running-set cell nil)
  (if (equal (plist-get content :status) "error")
      (ein:cell--handle-output cell "error" content metadata)
    (let ((events (slot-value cell 'events)))
      (ein:events-trigger events 'set_dirty.Worksheet (list :value t :cell cell))
      (ein:events-trigger events 'maybe_reset_undo.Worksheet cell))))

(cl-defmethod ein:cell--handle-set-next-input ((cell ein:codecell) text)
  (let ((events (slot-value cell 'events)))
    (ein:events-trigger events 'set_next_input.Worksheet
                        (list :cell cell :text text))
    (ein:events-trigger events 'maybe_reset_undo.Worksheet cell)
    ))



;;; Output area

;; These function should go to ein-output-area.el.  But as cell and
;; EWOC is connected in complicated way, I will leave them in
;; ein-cell.el.

(cl-defmethod ein:cell--handle-output ((cell ein:codecell) msg-type content
                                       _metadata)
  (let* ((json (list :output_type msg-type)))
    (ein:case-equal msg-type
      (("stream")
       (plist-put json :text (or (plist-get content :data)
                                 (plist-get content :text))) ;; Horrible hack to deal with version 5.0 of messaging protocol.
       (plist-put json :stream (plist-get content :name)))
      (("display_data" "pyout" "execute_result") ;; in v4 nbformat execute_result == pyout
       (when (or (equal msg-type "pyout")
                 (equal msg-type "execute_result"))
         (plist-put json :prompt_number (plist-get content :execution_count)))
       (setq json (ein:output-area-convert-mime-types
                   json (plist-get content :data)))
       )
      (("pyerr" "error")
       (plist-put json :ename (plist-get content :ename))
       (plist-put json :evalue (plist-get content :evalue))
       (plist-put json :traceback (plist-get content :traceback))))
    (ein:cell-append-output cell json t)
    ;; (setf (slot-value cell 'dirty) t)
    (ein:events-trigger (slot-value cell 'events) 'maybe_reset_undo.Worksheet cell)
    ))


(defun ein:output-area-convert-mime-types (json data)
  (loop for (prop . mime) in '((:text       . :text/plain)
                               (:html       . :text/html)
                               (:svg        . :image/svg+xml)
                               (:png        . :image/png)
                               (:jpeg       . :image/jpeg)
                               (:latex      . :text/latex)
                               (:json       . :application/json)
                               (:javascript . :application/javascript)
                               (:emacs-lisp . :application/emacs-lisp))
        when (plist-member data mime)
        do (plist-put json prop (plist-get data mime)))
  json)


(cl-defmethod ein:cell--handle-clear-output ((cell ein:codecell) content
                                             _metadata)
  ;; Jupyter messaging spec 5.0 no longer has stdout, stderr, or other fields for clear_output
  (ein:cell-clear-output cell
                         t ;;(plist-get content :stdout)
                         t ;;(plist-get content :stderr)
                         t ;;(plist-get content :other))
                         )
  (ein:events-trigger (slot-value cell 'events) 'maybe_reset_undo.Worksheet cell))


;;; Misc.

(cl-defmethod ein:cell-has-image-ouput-p ((cell ein:codecell))
  "Return `t' if given cell has image output, `nil' otherwise."
  (loop for out in (slot-value cell 'outputs)
        when (or (plist-member out :svg)
                 (plist-member out :image/svg)
                 (plist-member out :png)
                 (plist-member out :image/png)
                 (plist-member out :jpeg)
                 (plist-member out :image/jpeg))
        return t))

(cl-defmethod ein:cell-has-image-ouput-p ((cell ein:textcell))
  nil)

(cl-defmethod ein:cell-get-tb-data ((cell ein:codecell))
  (loop for out in (slot-value cell 'outputs)
        when (and
              (not (null (plist-get out :traceback)))
              (member (plist-get out :output_type) '("pyerr" "error")))
        return (plist-get out :traceback)))

(provide 'ein-cell)

;;; ein-cell.el ends here
