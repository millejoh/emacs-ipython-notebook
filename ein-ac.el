;;; ein-ac.el --- Auto-complete extension

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-ac.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ac.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-ac.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'auto-complete)

(defvar ein:ac-dotty-syntax-table
  (let ((table (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Adapted from `python-dotty-syntax-table'.")

(defun ein:completer-finish-completing-ac (matched-text matches)
  "Invoke completion using `auto-complete'.
Only the argument MATCHES is used.  MATCHED-TEXT is for
compatibility with `ein:completer-finish-completing-default'."
  ;; I don't need to check if the point is at right position, as in
  ;; `ein:completer-finish-completing-default' because `auto-complete'
  ;; checks it anyway.
  (with-syntax-table ein:ac-dotty-syntax-table
    (auto-complete
     `(((candidates . ',matches)
        (symbol . "s"))))))

(provide 'ein-ac)

;;; ein-ac.el ends here
