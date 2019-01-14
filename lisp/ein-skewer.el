;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; ein-skewer.el --- Cell module

;; (C) 2016 - John M Miller

;; Author: John M Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-skewer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-skewre.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-cell.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This depends on the skewer package, so likely will get split into
;; its own package at some point.

;;; Code:

(require 'skewer-mode)

(defvar *ein:skewer-running-p* nil "True if the emacs httpd server has been started.")

(defun ein:js-prepare-result (result type)
  (list :output_type type :text result))

(defun ein:update-javascript-output (cell json result)
  (let ((val (ein:js-prepare-result
              (cdr (assoc 'value result))
              (plist-get json :output_type))))
    (setf (slot-value cell 'outputs) (list val))
    (ein:cell-append-output cell val (slot-value cell 'dynamic))))

;; Format of result is ((id . STR) (type . STR) (status . STR) (value . STR) (time . FLOAT))
(defun ein:execute-javascript (cell json)
  (let ((payload (or (plist-get json :html)
                     (plist-get json :javascript))))
    (unless (httpd-running-p) ;; *ein:skewer-running-p*
      (run-skewer))
    (deferred:$
      (deferred:next
        (lambda ()
          (let ((result nil))
            (skewer-eval payload (lambda (v)
                                      (setq result v))
                         :type (if (plist-get json :html)
                                   "html"
                                 "eval"))
            (message "Result=%s" result)
            (cl-loop until result
                     do (accept-process-output nil 0.01)
                     finally (return result)))))
      (deferred:nextc it
        (lambda (result)
          (message "Result=%s" result)
          (ein:update-javascript-output cell json result))))))

(provide 'ein-skewer)
