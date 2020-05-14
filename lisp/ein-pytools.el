;;; ein-pytools.el --- Python tools build on top of kernel    -*- lexical-binding:t -*-

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-pytools.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pytools.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pytools.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-kernel)

(defun ein:pytools-jump-to-source-command (&optional other-window)
  "Jump to the source code of the object at point.
When the prefix argument ``C-u`` is given, open the source code
in the other window.  You can explicitly specify the object by
selecting it."
  (interactive "P")
  (cl-letf (((symbol-function 'xref--prompt-p) #'ignore))
    (if other-window
        (call-interactively #'xref-find-definitions-other-window)
      (call-interactively #'xref-find-definitions))))

(defun ein:pytools-jump-back-command (&optional _other-window)
  "Go back to the point where `ein:pytools-jump-to-source-command'
is executed last time.  When the prefix argument ``C-u`` is
given, open the last point in the other window."
  (interactive "P")
  (call-interactively #'xref-pop-marker-stack))

(define-obsolete-function-alias
  'ein:pytools-eval-string-internal
  'ein:shared-output-eval-string "0.1.2")

(provide 'ein-pytools)

;;; ein-pytools.el ends here
