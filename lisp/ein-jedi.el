;;; ein-jedi.el --- ein Jedi

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-jedi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-jedi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-jedi.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'jedi)

(require 'ein-ac)
(require 'ein-completer)

(defvar ein:jedi-dot-complete-sources
  '(ac-source-jedi-direct ac-source-ein-direct))

(defun ein:jedi-complete ()
  ;; Should I make it parallel and call auto-complete at the end?
  (deferred:nextc (jedi:complete-request)
    (lambda ()
      (auto-complete ein:jedi-dot-complete-sources)))
  (ein:and-let* ((kernel (ein:get-kernel))
                 ((not (ac-cursor-on-diable-face-p)))
                 ((ein:kernel-live-p kernel)))
    (ein:completer-complete
     kernel
     '(:complete_reply ((lambda (_ matched-text matches)
                          (ein:completer-finish-completing-ac
                           matched-text matches
                           ein:jedi-dot-complete-sources))
                        . nil)))))

(defun ein:jedi-dot-complete ()
  (interactive)
  (insert ".")
  (ein:jedi-complete))

(defun ein:jedi-complete-on-dot-install (map)
  (ein:complete-on-dot-install map #'ein:jedi-dot-complete))

;; (ein:jedi-complete-on-dot-install ein:connect-mode-map)

(provide 'ein-jedi)

;;; ein-jedi.el ends here
