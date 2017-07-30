;;; ein-multilang-fontify.el --- Syntax highlighting for multiple-languages

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-multilang-fontify.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ein-multilang-fontify.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-multilang-fontify.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; It would be nice if org-src is available, but this module should
;; work without org-src.  Data on `org-src-lang-modes' is used
;; if this variable is bound.
(require 'org-src nil t)

(defun ein:mlf-get-lang-mode (lang)
  "Return major mode for LANG.
Modified version of `org-src-get-lang-mode'."
  (when (symbolp lang)
    (setq lang (symbol-name lang)))
  (intern
   (format "%s-mode"
           (or (and (bound-and-true-p org-src-lang-modes)
                    (cdr (assoc lang org-src-lang-modes)))
               lang))))

(defun ein:mlf-font-lock-fontify-block (lang start end)
  "Patched version of `org-src-font-lock-fontify-block'."
  (let ((lang-mode (ein:mlf-get-lang-mode lang)))
    (if (fboundp lang-mode)
        (let ((string (buffer-substring-no-properties start end))
              (modified (buffer-modified-p))
              (orig-buffer (current-buffer))
              pos
              next)
          (remove-text-properties start end '(face nil))
          (with-current-buffer
              (get-buffer-create
               (concat " ein:mlf-fontification:" (symbol-name lang-mode)))
            (delete-region (point-min) (point-max))
            (insert (concat string " ")) ;; so there's a final property change
            (unless (eq major-mode lang-mode) (funcall lang-mode))
            (font-lock-fontify-buffer)
            (setq pos (point-min))
            (while (setq next (next-single-property-change pos 'face))
              (put-text-property
               ;; `font-lock-face' property is used instead of `font'.
               ;; This is the only difference from org-src.
               (+ start (1- pos)) (+ start next) 'font-lock-face
               (get-text-property pos 'face) orig-buffer)
              (setq pos next)))
          (add-text-properties
           start end
           '(font-lock-fontified t fontified t font-lock-multiline t))
          (set-buffer-modified-p modified)))))

(provide 'ein-multilang-fontify)

;;; ein-multilang-fontify.el ends here
