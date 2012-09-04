;;; ein-org-src.el --- Notebook mode using org-src.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-org-src.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-org-src.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-org-src.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org-src)

(require 'ein-worksheet)

(defun ein:org-src-fontify (limit)
  "Fontify next input area comes after the current point then
return `t' or `nil' if not found.
See info node `(elisp) Search-based Fontification'."
  (ein:log-ignore-errors
    (ein:org-src-fontify-1 limit)))

(defun ein:org-src-fontify-1 (limit)
  "Actual implementation of `ein:org-src-fontify'.
This function may raise an error."
  (ein:and-let* ((pos (point))
                 (node (ein:worksheet-get-nearest-cell-ewoc-node pos limit))
                 (cell (ein:worksheet-next-input-cell node))
                 (start (ein:cell-input-pos-min cell))
                 (end   (ein:cell-input-pos-max cell))
                 (lang (ein:cell-language cell)))
    (when (and (>= start pos) (<= end limit))
      (org-src-font-lock-fontify-block lang start end))
    t))

(defvar ein:org-src-font-lock-keywords
  '(ein:org-src-fontify)
  "Default `font-lock-keywords' for `ein:notebook-org-src-mode'.")

(defun ein:org-src-set-font-lock-defaults ()
  (set (make-local-variable 'font-lock-defaults)
       '(ein:org-src-font-lock-keywords)))

(define-derived-mode ein:notebook-org-src-mode fundamental-mode "ein:os"
  "Notebook mode with org-mode powered fontification."
  (ein:org-src-set-font-lock-defaults))

(provide 'ein-org-src)

;;; ein-org-src.el ends here
