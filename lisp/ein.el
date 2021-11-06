;;; ein.el --- jupyter notebook client    -*- lexical-binding:t -*-

;; Copyright (C) 2012-2019 The Authors of the Emacs IPython Notebook (EIN)

;; Authors:  dickmao <github id: dickmao>
;;           John Miller <millejoh at millejoh.com>
;;           Takafumi Arakaki <aka.tkf at gmail.com>
;; URL: https://github.com/dickmao/emacs-ipython-notebook
;; Keywords: jupyter, literate programming, reproducible research

;; This file is NOT part of GNU Emacs.

;; ein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs IPython Notebook (EIN), despite its name, is a jupyter client for all
;; languages.  It does not work under non-WSL Windows environments.
;;
;; No require statements, e.g. ``(require 'ein)``, are necessary, contrary to the
;; `prevailing documentation`_, which should be disregarded.
;;
;; Org_ users please find ob-ein_, a jupyter Babel_ backend.
;;
;; `AWS GCE (Preview)`_ integration is in alpha.
;;
;; EIN was originally written by `[tkf]`_.  A jupyter Babel_ backend was first
;; introduced by `[gregsexton]`_.
;;

;;; Code:

(when (boundp 'mouse-buffer-menu-mode-groups)
  (add-to-list 'mouse-buffer-menu-mode-groups
               '("^ein:" . "ein")))

(provide 'ein)

;;; ein.el ends here
