;;; ein-ipynb-mode.el --- A simple mode for ipynb file

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-ipynb-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ipynb-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-ipynb-mode.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-process)

(defvar ein:ipynb-parent-mode 'js-mode
  "A mode (a symbol) to use for parent mode for `ein:ipynb-mode'.
Note that this variable must be set *before* compiling EIN.")

(defalias 'ein:ipynb-parent-mode ein:ipynb-parent-mode)

;;;###autoload
(define-derived-mode ein:ipynb-mode ein:ipynb-parent-mode "ein:ipynb"
  "A simple mode for ipynb file.")

(let ((map ein:ipynb-mode-map))
  (define-key map "\C-c\C-z" 'ein:process-find-file-callback)
  (define-key map "\C-c\C-o" 'ein:process-find-file-callback)
  (easy-menu-define ein:ipynb-menu map "EIN IPyNB Mode Menu"
    `("EIN IPyNB File"
      ,@(ein:generate-menu
         '(("Open notebook" ein:process-find-file-callback))))))

;;;###autoload
(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ein:ipynb-mode))

(provide 'ein-ipynb-mode)

;;; ein-ipynb-mode.el ends here
