;;; ein-company.el --- Company extension

;; Copyright (C) 2016 - John M. Miller

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;;       : John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-company.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; ein-ac.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; ein-ac.el. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide backend company autocompletion.

;;; Code:

(require cl-lib)
(require ein-ac)

;; Same as ein:ac-chunk-regex.
(defvar ein:company-chunk-regex
  (rx (group (| (syntax whitespace)
                (syntax open-parenthesis)
                (syntax close-parenthesis)
                (syntax string-quote) ; Complete files for `open("path/..`
                bol))
      (? (syntax punctuation))          ; to complete ``~/PATH/...``
      (* (+ (| (syntax word) (syntax symbol)))
         (syntax punctuation))
      (+ (| (syntax word) (syntax symbol)))
      (? (syntax punctuation))
      point)
  "A regexp that matches to a \"chunk\" containing words and dots.")

(defun ein:company-candidates (arg)
  )

(defun ein:company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend))
    (prefix (company-grab-symbol-cons ein:company-chunk-regex))
    (candidates (ein:ac-direct-get-matches))
    (annotation )
    (meta ac))
