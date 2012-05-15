;;; ein-pager.el --- Pager module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-pager.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pager.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pager.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ansi-color)

(require 'ein-utils)

(defun ein:pager-new (name)
  name)

(defun ein:pager-clear (pager)
  (ein:with-read-only-buffer (get-buffer-create pager)
    (erase-buffer)))

(defun ein:pager-expand (pager)
  (pop-to-buffer (get-buffer-create pager))
  (goto-char (point-min)))

(defun ein:pager-append-text (pager text)
  (ein:with-read-only-buffer (get-buffer-create pager)
    (insert (ansi-color-apply text))
    (ein:pager-mode)))

(define-derived-mode ein:pager-mode fundamental-mode "ein:pager"
  "IPython notebook pager mode."
  (view-mode)
  (font-lock-mode))

(provide 'ein-pager)

;;; ein-pager.el ends here
