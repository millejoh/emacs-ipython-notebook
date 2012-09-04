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

(require 'ein-notebook)

(defun ein:org-src-fontify (limit)
  "Fontify next input area comes after the current point then
return `t' or `nil' if not found.
See info node `(elisp) Search-based Fontification'."
  (ein:log-ignore-errors
    (ein:org-src-fontify-1 limit)))

(defun ein:org-src-current-or-next-input-cell (ewoc-node)
  "Almost identical to `ein:worksheet-next-input-cell' but return
the current cell if EWOC-NODE is the input area node."
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ein:$node-data ewoc-data))
         (path (ein:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element '(prompt input))
        cell
      (ein:cell-next cell))))

(defun ein:org-src-fontify-1 (limit)
  "Actual implementation of `ein:org-src-fontify'.
This function may raise an error."
  (ein:and-let* ((pos (point))
                 (node (ein:worksheet-get-nearest-cell-ewoc-node pos limit))
                 (cell (ein:org-src-current-or-next-input-cell node))
                 (start (ein:cell-input-pos-min cell))
                 (end   (ein:cell-input-pos-max cell))
                 ((<= end limit))
                 ((< start end))
                 (lang (ein:cell-language cell)))
    (let ((inhibit-read-only t))
      (org-src-font-lock-fontify-block lang start end)
      ;; Emacs fontification mechanism requires the function to move
      ;; the point.  Do *not* use `(goto-char end)'.  As END is in the
      ;; input area, fontification falls into an infinite loop.
      (ewoc-goto-node (oref cell :ewoc) (ein:cell-element-get cell :footer)))
    t))

(defun ein:org-src-back-to-prev-node ()
  (ein:aand (ein:worksheet-get-ewoc) (ewoc-goto-prev it 1)))

(defvar ein:org-src-font-lock-keywords
  '((ein:org-src-fontify))
  "Default `font-lock-keywords' for `ein:notebook-org-src-mode'.")

(defun ein:org-src-set-font-lock-defaults ()
  (set (make-local-variable 'font-lock-defaults)
       '(ein:org-src-font-lock-keywords
         ;; The following are adapted from org-mode but I am not sure
         ;; if I need them:
         t nil nil
         ein:org-src-back-to-prev-node)))

(define-derived-mode ein:notebook-org-src-mode fundamental-mode "ein:os"
  "Notebook mode with org-mode powered fontification."
  (ein:org-src-set-font-lock-defaults))

(provide 'ein-org-src)

;;; ein-org-src.el ends here
