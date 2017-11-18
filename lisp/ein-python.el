;;; ein-python.el --- Workarounds for python.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-python.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-python.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-python.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'python)

(require 'ein-worksheet)

(defvar ein:python-block-start
  (rx line-start
      symbol-start
      (or "def" "class" "if" "elif" "else" "try"
          "except" "finally" "for" "while" "with")
      symbol-end))

(defun ein:python-indent-calculate-indentation--around (orig &rest args)
  "False if there is no python block yet in this cell."
  (condition-case _
      (ein:and-let* ((cell (ein:worksheet-get-current-cell))
                     (beg (ein:cell-input-pos-min cell))
                     (p (point))
                     ((< beg (point))))
        (if (not (search-backward-regexp ein:python-block-start beg t))
            0
          (goto-char p)
          (apply orig args)))
    (error (apply orig args))))

(advice-add 'python-indent--calculate-indentation :around #'ein:python-indent-calculate-indentation--around)

;; (defadvice python-indent-calculate-indentation
;;     (around ein:python-indent-calculate-levels activate)
;;   "Hack `python-indent-calculate-levels' to reset indent per cell.

;; Let's say you have a notebook something like this::

;;   In [1]:
;;   def func():
;;       pass

;;   In [2]:
;;   something[]

;; Here, ``[]`` is the cursor position.  When you hit the tab here,
;; you don't expect it to indent.  However, python.el tries to follow
;; the indent of ``func()`` then you get indentation.  This advice
;; workaround this problem.

;; Note that this workaround does not work with the MuMaMo based
;; notebook mode."
;;   (if (ein:ein-block-start-p)
;;       ad-do-it
;;     0))


(provide 'ein-python)

;;; ein-python.el ends here
