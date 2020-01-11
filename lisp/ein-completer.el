;;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; ein-completer.el --- Completion module

;; Copyright (C) 2018- Takafumi Arakaki / John Miller

;; Author: Takafumi Arakaki <aka.tkf at gmail.com> / John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-completer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-completer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-completer.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This needs to get re-written.

;;; Code:

(make-obsolete-variable 'ein:complete-on-dot nil "0.15.0")
(make-obsolete-variable 'ein:completion-backend nil "0.17.0")

(provide 'ein-completer)

;;; ein-completer.el ends here
