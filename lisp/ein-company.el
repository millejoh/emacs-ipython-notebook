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

(require 'ein-core)
(require 'ein-kernel)

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

(defvar ein:company-direct-matches nil
  "The analog of ein:ac-direct-matches, but for company-mode. Stores
completion candidates for `company-mode' as returned by ein:completer-complete.")

(defun ein:company-chunk-beginning ()
  "Return the position where the chunk begins."
  (ignore-errors
    (save-excursion
      (+ (re-search-backward ein:ac-chunk-regex) (length (match-string 1))))))

(defun ein:company-chunk-candidates-from-list (chunk-list)
  "Return matched candidates in CHUNK-LIST."
  (let* ((start (ein:ac-chunk-beginning)))
    (when start
      (loop with prefix = (buffer-substring start (point))
            for cc in chunk-list
            when (string-prefix-p prefix cc)
            collect cc))))

(defun ein:company-candidates (arg)
  )

(defun ein:company-request-in-background ()
  (ein:and-let* ((kernel (ein:get-kernel))
                 ((ein:kernel-live-p kernel)))
    (ein:kernel-complete
     kernel
     (thing-at-point 'line)
     (current-column)
     kernel
     :callbacks
     (list :complete_reply
           (cons (lambda (_ content __)
                   (ein:company-prepare-completion (plist-get content :matches)))
                 nil)))))


(defun ein:company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend))
    (prefix (and (or ein:%notebook% (eq major-mode 'python-mode))
                 (company-grab-symbol-cons (ein:company-chunk-beginning))))
    (candidates (ein:ac-direct-get-matches))))

(provide 'ein-company)
