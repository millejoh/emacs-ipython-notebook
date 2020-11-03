;;; ein-ipynb-mode.el --- A simple mode for ipynb file    -*- lexical-binding:t -*-

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
(require 'js)

;;;###autoload
(define-derived-mode ein:ipynb-mode js-mode "ein:ipynb"
  "A simple mode for ipynb file.

\\{ein:ipynb-mode-map}
"
  :group 'ein)

(let ((map ein:ipynb-mode-map))
  (set-keymap-parent map js-mode-map)
  (define-key map "\C-c\C-z" 'ein:process-find-file-callback)
  (define-key map "\C-c\C-o" 'ein:process-find-file-callback)
  (define-key map "\C-c\C-r" 'ein:gat-run-remote)
  (easy-menu-define ein:ipynb-menu map "EIN IPyNB Mode Menu"
    `("EIN IPyNB File"
      ,@(ein:generate-menu
         '(("Open notebook" ein:process-find-file-callback))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . ein:ipynb-mode))

(provide 'ein-ipynb-mode)

;;; ein-ipynb-mode.el ends here
