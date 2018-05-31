;;; ein-hy.el --- Hylang Support-*- lexical-binding: t; -*-

;; (C) 2018 - John M. Miller

;; Author: John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-hy.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-hy.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-hy.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'ein-classes)

(defmethod ein:cell-insert-prompt ((cell ein:hy-codecell))
  "Insert prompt of the CELL in the buffer.
Called from ewoc pretty printer via `ein:cell-pp'."
  ;; Newline is inserted in `ein:cell-insert-input'.
  (ein:insert-read-only
   (concat
    (format "In (hy) [%s]" (or (ein:oref-safe cell 'input-prompt-number)  " "))
    (ein:maybe-show-slideshow-data cell)
    (when (slot-value cell 'autoexec) " %s" ein:cell-autoexec-prompt))
   'font-lock-face 'ein:cell-input-prompt))

(defmethod ein:cell-execute-internal ((cell ein:hy-codecell)
                                      kernel code &rest args)
  (ein:cell-clear-output cell t t t)
  (ein:cell-set-input-prompt cell "*")
  (ein:cell-running-set cell t)
  (setf (slot-value cell 'dynamic) t)
  (apply #'ein:kernel-execute kernel (ein:pytools-wrap-hy-code code) (ein:cell-make-callbacks cell) args))

(defmethod ein:cell-to-nb4-json :before ((cell ein:hy-codecell) _ &optional _ignore)
  (let ((metadata (slot-value cell 'metadata)))
    (setf metadata (plist-put metadata :ein.hy_cell t))))

(provide 'ein-hy)
