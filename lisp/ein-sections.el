;;; ein-sections.el --- Collapsable, read-only blocks of interactive text

;; This file is NOT part of GNU Emacs.

;; ein-sections.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-sections.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-sections.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)

;;; EIN Sections
;; A quick and dirty re-implementation of magit sections, focusing
;; on the ability to collapse and expand sections of text.

(cl-defstruct ein:section
  type value start content end hidden washer refined
  source diff-header process parent children)

(defvar-local ein:root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `ein:insert-section' and you should
never modify it.")
(put 'ein:root-section 'permanent-local t)

(defun ein:current-section ()
  "Return the section at point."
  (or (get-text-property (point) 'ein:section) ein:root-section))

(defun ein:section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (cons (cons (ein:section-type section)
              (ein:section-value section))
        (--when-let (ein:section-parent section)
          (ein:section-ident it))))

(defun ein:get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `ein:section-ident'."
  (setq ident (reverse ident))
  (let ((section (or root ein:root-section)))
    (when (eq (car (pop ident)) (ein:section-type section))
      (while (and ident
                  (setq section
                        (--first
                         (and (eq    (caar ident) (ein:section-type it))
                              (equal (cdar ident) (ein:section-value it)))
                         (ein:section-children section))))
        (pop ident))
      section)))

(defvar ein:insert-section--current nil "For internal use only.")
(defvar ein:insert-section--parent  nil "For internal use only.")
(defvar ein:insert-section--oldroot nil "For internal use only.")

;;;; Auxiliary

(defun magit-describe-section ()
  "Show information about the section at point.
This command is intended for debugging purposes."
  (interactive)
  (let ((section (magit-current-section)))
    (message "%S %S %s-%s"
             (magit-section-value section)
             (apply 'vector (mapcar 'car (magit-section-ident section)))
             (marker-position (magit-section-start section))
             (marker-position (magit-section-end section)))))

;;; Create

(defvar magit-insert-section-hook nil
  "Hook run after `magit-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro magit-insert-section (&rest args)
  "Insert a section at point.

TYPE is the section type, a symbol.  Many commands that act on
the current section behave differently depending on that type.
Also if a variable `magit-TYPE-section-map' exists, then use
that as the text-property `keymap' of all text belonging to the
section (but this may be overwritten in subsections).  TYPE can
also have the form `(eval FORM)' in which case FORM is evaluated
at runtime.

Optional VALUE is the value of the section, usually a string
that is required when acting on the section.

When optional HIDE is non-nil collapse the section body by
default, i.e. when first creating the section, but not when
refreshing the buffer.  Else expand it by default.  This can be
overwritten using `magit-section-set-visibility-hook'.  When a
section is recreated during a refresh, then the visibility of
predecessor is inherited and HIDE is ignored (but the hook is
still honored).

BODY is any number of forms that actually insert the section's
heading and body.  Optional NAME, if specified, has to be a
symbol, which is then bound to the struct of the section being
inserted.

Before BODY is evaluated the `start' of the section object is set
to the value of `point' and after BODY was evaluated its `end' is
set to the new value of `point'; BODY is responsible for moving
`point' forward.

If it turns out inside BODY that the section is empty, then
`magit-cancel-section' can be used to abort and remove all traces
of the partially inserted section.  This can happen when creating
a section by washing Git's output and Git didn't actually output
anything this time around.

\(fn [NAME] (TYPE &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp]
                   (&or [("eval" symbolp) &optional form form]
                        [symbolp &optional form form])
                   body)))
  (let ((s (if (symbolp (car args))
               (pop args)
             (cl-gensym "section"))))
    `(let* ((,s (make-magit-section
                 :type ,(let ((type (nth 0 (car args))))
                          (if (eq (car-safe type) 'eval)
                              (cadr type)
                            `',type))
                 :value ,(nth 1 (car args))
                 :start (point-marker)
                 :parent magit-insert-section--parent)))
       (setf (magit-section-hidden ,s)
             (-if-let (value (run-hook-with-args-until-success
                              'magit-section-set-visibility-hook ,s))
                 (eq value 'hide)
               (--if-let (and magit-insert-section--oldroot
                              (magit-get-section
                               (magit-section-ident ,s)
                               magit-insert-section--oldroot))
                   (magit-section-hidden it)
                 ,(nth 2 (car args)))))
       (let ((magit-insert-section--current ,s)
             (magit-insert-section--parent  ,s)
             (magit-insert-section--oldroot
              (or magit-insert-section--oldroot
                  (unless magit-insert-section--parent
                    (prog1 magit-root-section
                      (setq magit-root-section ,s))))))
         (catch 'cancel-section
           ,@(cdr args)
           (run-hooks 'magit-insert-section-hook)
           (magit-insert-child-count ,s)
           (set-marker-insertion-type (magit-section-start ,s) t)
           (let* ((end (setf (magit-section-end ,s) (point-marker)))
                  (map (intern (format "magit-%s-section-map"
                                       (magit-section-type ,s))))
                  (map (and (boundp map) (symbol-value map))))
             (save-excursion
               (goto-char (magit-section-start ,s))
               (while (< (point) end)
                 (let ((next (or (next-single-property-change
                                  (point) 'magit-section)
                                 end)))
                   (unless (get-text-property (point) 'magit-section)
                     (put-text-property (point) next 'magit-section ,s)
                     (when map
                       (put-text-property (point) next 'keymap map)))
                   (goto-char next)))))
           (if (eq ,s magit-root-section)
               (magit-section-show ,s)
             (setf (magit-section-children (magit-section-parent ,s))
                   (nconc (magit-section-children (magit-section-parent ,s))
                          (list ,s)))))
         ,s))))

(defun magit-cancel-section ()
  (when magit-insert-section--current
    (if (not (magit-section-parent magit-insert-section--current))
        (insert "(empty)\n")
      (delete-region (magit-section-start magit-insert-section--current)
                     (point))
      (setq magit-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun magit-insert-heading (&rest args)
  "Insert the heading for the section currently being inserted.

This function should only be used inside `magit-insert-section'.

When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line when this function is used like this.

When called with arguments ARGS, which have to be strings, then
insert those strings at point.  The section should not contain
any text before this happens and afterwards it should again only
contain a single line.  If the `face' property is set anywhere
inside any of these strings, then insert all of them unchanged.
Otherwise use the `magit-section-heading' face for all inserted
text.

The `content' property of the section struct is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end').  If the
value of `content' is nil, then the section has no heading and
its body cannot be collapsed.  If a section does have a heading
then its height must be exactly one line, including a trailing
newline character.  This isn't enforced, you are responsible for
getting it right.  The only exception is that this function does
insert a newline character if necessary."
  (declare (indent defun))
  (when args
    (let ((heading (apply #'concat args)))
      (insert (if (next-single-property-change 0 'face (concat "0" heading))
                  heading
                (propertize heading 'face 'magit-section-heading)))))
  (unless (bolp)
    (insert ?\n))
  (magit-maybe-make-margin-overlay)
  (setf (magit-section-content magit-insert-section--current) (point-marker)))

(defvar magit-insert-headers-hook nil "For internal use only.")

(defun magit-insert-headers (hooks)
  (let ((magit-insert-section-hook
         (cons 'magit-insert-remaining-headers
               (if (listp magit-insert-section-hook)
                   magit-insert-section-hook
                 (list magit-insert-section-hook))))
        (magit-insert-headers-hook hooks)
        wrapper)
    (while (and (setq wrapper (pop magit-insert-headers-hook))
                (= (point) (point-min)))
      (funcall wrapper))))

(defun magit-insert-remaining-headers ()
  (if (= (point) (point-min))
      (magit-cancel-section)
    (magit-insert-heading)
    (remove-hook 'magit-insert-section-hook 'magit-insert-remaining-headers)
    (mapc #'funcall magit-insert-headers-hook)
    (insert "\n")))

(defun magit-insert-child-count (section)
  "Modify SECTION's heading to contain number of child sections.

If `magit-section-show-child-count' is non-nil and the SECTION
has children and its heading ends with \":\", then replace that
with \" (N)\", where N is the number of child sections.

This function is called by `magit-insert-section' after that has
evaluated its BODY.  Admittedly that's a bit of a hack."
  ;; This has to be fast, not pretty!
  (let (content count)
    (when (and magit-section-show-child-count
               (setq count (length (magit-section-children section)))
               (> count 0)
               (setq content (magit-section-content section))
               (eq (char-before (1- content)) ?:))
      (save-excursion
        (goto-char (- content 2))
        (insert (format " (%s)" count))
        (delete-char 1)))))
