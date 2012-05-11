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

(require 'ein-log)

(defun ein:pager-new (name)
  (get-buffer-create name))

(defun ein:pager-clear (pager)
  (with-current-buffer pager
    (erase-buffer)))

(defun ein:pager-expand (pager)
  (pop-to-buffer pager)
  (goto-char (point-min))
  (unless font-lock-mode
    (font-lock-mode)))

(defun ein:pager-append-text (pager text)
  (with-current-buffer pager
    (save-excursion
      (insert (ansi-color-apply text)))))

(provide 'ein-pager)

;;; ein-pager.el ends here
