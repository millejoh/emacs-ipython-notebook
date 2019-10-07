;;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; ein-inspector.el --- An inspector, in emacs, for Python

;; Copyright (C) 2017 - John Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-inspector.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-inspector.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-inspector.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ein-pytools)

;;;###autoload
(defun ein:inspect-object (kernel object)
  (interactive (list (ein:get-kernel-or-error)
                     (ein:object-at-point-or-error)))
  (ein:kernel-execute kernel
                      (format "__import__('ein_inspector').generate_inspector_data('%s', globals(), locals())"
                              object)
                      (list
                       :output
                       (cons #'ein:prepare-inspector
                             (list kernel object)))))

(defun ein:prepare-inspector (packed _msg-type content _metadata)
  (cl-destructuring-bind (_kernel oname)
      packed
    (ein:aif (or (plist-get content :text) (plist-get content :data))
        (let ((oinfo (ein:json-read-from-string it)))
          (if (not (plist-get oinfo :type))
              (ein:log 'warn "[EIN:INSPECTOR]: %s" (plist-get oinfo :error))
            (ein:render-inspector oinfo)))
      (ein:log 'warn "[EIN:INSPECTOR]: Could not find inspect data for object %s." oname))))

(defclass ein:iobject ()
  ((name :accessor ein:iobject-name :documentation "String representation can be evaluated in python to generate the object being inspected.")
   (type :accessor ein:iobject-type :documentation "Python type of object, as returned by `type()`.")
   (repr :accessor ein:iobject-repr :documentation "Value of object, as returned by its `__str__` method.")
   (source-file :accessor ein:iobject-sfile :documentation "If availabe, the filename where the source for this object is to be found.")
   (source-lines :accessor ein:iobject-slines :documentation "If available, the line in the file where the source for this object is found.")
   (doc :accessor ein:iobject-doc :documentation "If available, the documentation string for this object."))
  :documentation "Class to hold information returned by Python `inspect` module for a Python object identified in the `name` slot.")

(defun ein:new-inspector-object (object-info)
  (make-instance 'ein:iobject
                 :name (plist-get object-info :name)
                 :type (plist-get object-info :type)
                 :repr (plist-get object-info :repr)
                 :source-file (plist-get object-info :source_file)
                 :source-lines (plist-get object-info :source_lines)
                 :doc (plist-get object-info :doc)))

(defvar ein:inspector-visit-source-map (make-sparse-keymap))

(defvar ein:inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    map)
  "Keymap for ein:inspector-mode.")

(define-derived-mode ein:inspector-mode special-mode "EIN:INSPECTOR-MODE"
  "Major mode for inspector Python objects from the emacs-ipython-notebook."
  )



(defun ein:render-inspector (oinfo)
  (let ((name (plist-get oinfo :name)))
    (switch-to-buffer (format "*EIN Inspector: %s*" name))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (let* ((type (plist-get oinfo :type))
           (repr (plist-get oinfo :repr))
           (sfile (plist-get oinfo :source_file))
           (slines (last (plist-get oinfo :source_lines)))
           (info-str (format "%s = {%s} %s" name type repr)))
      (if sfile
          (widget-create 'link
                         :notify
                         (lambda (&rest _ignore)
                           (ein:goto-file sfile (car slines)))
                         info-str)
        (widget-insert (propertize info-str 'face 'bold)))
      (widget-insert (format "\n\n%s\n\n" (make-string 80 ?\u2501)))
      (widget-insert (format "%s\n\n%s\n\n" (plist-get oinfo :doc) (make-string 80 ?\u2501)))
      (widget-insert (propertize (format "%s: %s\n" type name)
                                 'face 'bold))))
  (ein:inspector-mode)
  (widget-setup))



(defun ein:inspector-visit-source ()
  (message "Visit source!"))

(defun ein:inspector-visit-thing ())

(defun ein:inspector-section-toggle (_section))

(defun ein:inspector-section-show (_section))

(defun ein:inspector-section-hide (_section)
  )

(provide 'ein-inspector)
