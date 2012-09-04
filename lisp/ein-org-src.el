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

(require 'org-src nil t)
(eval-when-compile (defvar markdown-mode-map))

(require 'ein-worksheet)

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
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (ein:org-src-keymap-setup-python)
  (ein:org-src-set-font-lock-defaults))


;;; Keymap setup functions

(defun ein:org-src-keymap-setup-python ()
  (when (boundp 'python-mode-map)
    (set-keymap-parent ein:notebook-org-src-mode-map python-mode-map))
  (cond
   ((featurep 'python)
    (setq indent-line-function #'python-indent-line-function)
    (setq indent-region-function #'python-indent-region))
   ((featurep 'python-mode)
    ;; FIXME: write keymap setup for python-mode.el
    )))

(defun ein:org-src-keymap-setup-markdown ()
  "Use `markdown-mode-map'.  NOTE: This function is not used now."
  (when (featurep 'markdown-mode)
    (set-keymap-parent ein:notebook-org-src-mode-map markdown-mode-map)))

;; FIXME: dynamically call ein:org-src-keymap-setup-LANG using
;;        `post-command-hook'.
;; FIMXE: add more ein:org-src-keymap-setup-LANG to switch kaymap.

(provide 'ein-org-src)

;;; ein-org-src.el ends here
