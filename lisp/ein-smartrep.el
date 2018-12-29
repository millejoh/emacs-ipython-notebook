;;; ein-smartrep.el --- smartrep integration

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-smartrep.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-smartrep.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-smartrep.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'smartrep nil t)

(defcustom ein:smartrep-notebook-mode-alist
  '(("C-t" . ein:worksheet-toggle-cell-type)
    ("C-l" . ein:worksheet-clear-output)
    ("C-k" . ein:worksheet-kill-cell)
    ("C-y" . ein:worksheet-yank-cell)
    ("C-a" . ein:worksheet-insert-cell-above)
    ("C-b" . ein:worksheet-insert-cell-below)
    ("C-n" . ein:worksheet-goto-next-input)
    ("C-p" . ein:worksheet-goto-prev-input)
    ("C-m" . ein:worksheet-merge-cell)
    ("<up>" . ein:worksheet-move-cell-up)
    ("<down>" . ein:worksheet-move-cell-down))
  "alist passed to `smartrep-define-key'."
  :type '(repeat (cons string function))
  :group 'ein)

(defmacro ein:smartrep-config (map)
  `(smartrep-define-key
    ,map
    "C-c"
    ein:smartrep-notebook-mode-alist))

(provide 'ein-smartrep)

;;; ein-smartrep.el ends here
