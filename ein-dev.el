;;; ein-dev.el --- Development tools

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-dev.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-dev.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-dev.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'rst)

(defun ein:dev-insert-notebook-mode-map ()
  "Insert mode-map into rst document.  For README.rst."
  (save-excursion
    (insert "\n\n::\n\n")
    (let ((beg (point)))
      (search-forward ".. // KEYS END //")
      (move-beginning-of-line nil)
      (delete-region beg (point))
      (insert "\n")
      (goto-char beg)
      (insert (substitute-command-keys "\\{ein:notebook-mode-map}"))
      (rst-shift-region beg (point) 1))))

(defvar ein:source-dir (file-name-directory load-file-name))

(defun ein:load-files (&optional regex dir)
  (let* ((dir (or dir ein:source-dir))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (mapc #'load files)))

(defun ein:dev-reload ()
  "Reload ein-*.el modules."
  (interactive)
  (ein:load-files "^ein-.*\\.el$"))


(provide 'ein-dev)

;;; ein-dev.el ends here
