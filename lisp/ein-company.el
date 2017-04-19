;;; ein-company.el --- Support for completion using company back-end.

;; Copyright (C) 2017 - John Miller

;; Author: John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-company.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-company.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-ac.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'company nil t)
(require 'dash)

(require 'ein-core)

;;;###autoload
(defun ein:company-backend (command &optional arg &rest ignore)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend) )
    (prefix (and (--filter (and (boundp it) (symbol-value it) (eql it 'ein:notebook-minor-mode))
                           minor-mode-list)
                 (thing-at-point 'line)))
    (candidates () (lexical-let ((kernel (ein:get-kernel-or-error))
                                 (arg arg)
                                 (col (current-column)))
                     (cons :async
                           (lambda (cb)
                             (ein:kernel-complete kernel
                                                  arg
                                                  col
                                                  (list :complete_reply
                                                        (list #'ein:completer-finish-completing-company cb)))))))))

(cl-defun ein:completer-finish-completing-company (callback content -metadata-not-used-)
  (ein:log 'debug "EIN:COMPANY-FINISH-COMPLETING: content=%S" content)
  (let* ((beg (point))
         (delta (- (plist-get content :cursor_end)
                   (plist-get content :cursor_start)))
         (matched-text (buffer-substring beg (- beg delta)))
         (matches (plist-get content :matches)))
    (ein:log 'debug "EIN:COMPANY-FINISH-COMPLETING: matches=%s" matches)
    (condition-case err
        (funcall (car callback) matches)
      (error (error (format "Error %s running ein company completer." err))))))

(setq ein:complete-on-dot nil)

(provide 'ein-company)
