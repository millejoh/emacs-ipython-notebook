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

(defun ein:prepare-inspector (packed msg-type content -metadata-not-used-)
  (destructuring-bind (kernel oname)
      packed
    (ein:aif (or (plist-get content :text) (plist-get content :data))
        (let ((oinfo (ein:json-read-from-string it)))
          (if (not (plist-get oinfo :type))
              (ein:log 'warn "[EIN:INSPECTOR]: %s" (plist-get oinfo :error))
            (ein:render-inspector oinfo)))
      (ein:log 'warn "[EIN:INSPECTOR]: Could not find inspect data for object at point!"))))

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
    (lexical-let* (
                   (type (plist-get oinfo :type))
                   (repr (plist-get oinfo :repr))
                   (sfile (plist-get oinfo :source_file))
                   (slines (last (plist-get oinfo :source_lines))))
      (if sfile
          (widget-create 'link
                         :notify
                         (lambda (&rest ignore)
                           (ein:goto-file sfile (car slines)))
                         repr)
        (widget-insert (propertize repr 'face 'bold)))
      (widget-insert (format "\n\n%s\n\n" (make-string 80 ?\u2501)))
      (widget-insert (format "%s\n\n%s\n\n" (plist-get oinfo :doc) (make-string 80 ?\u2501)))
      (widget-insert (propertize (format "%s: %s\n" type name)
                                 'face 'bold))))
  (ein:inspector-mode)
  (widget-setup))

(defun ein:inspector-visit-source ()
  (message "Visit source!"))

(defun ein:inspector-visit-thing ())

(defun ein:inspector-section-toggle (section))

(defun ein:inspector-section-show (section))

(defun ein:inspector-section-hide (section)
  )

(provide 'ein-inspector)
