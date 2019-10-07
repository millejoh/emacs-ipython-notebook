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
(require 'skewer-html)
(require 'simple-httpd)

(defvar *ein:skewer-running-p* nil "True if the emacs httpd server has been started.")

(defun ein:js-prepare-result (result type)
  (list :output_type type :text result))

(defun ein:update-javascript-output (cell json result)
  (let ((val (ein:js-prepare-result
              (or (cdr (assoc 'value result)) "See browser for result.")
              (plist-get json :output_type))))
    (setf (slot-value cell 'outputs) (list val))
    (ein:cell-append-output cell val (slot-value cell 'dynamic))))

(defservlet current-jupyter-cell-output text/html (path)
  (let ((cell-id (file-name-nondirectory path)))
    (insert (gethash cell-id *ein:skewer-cell-output-cache*))))

(defvar *ein:skewer-html-template*
  "<html>
   <head>
    <title>Emacs IPython Notebook</title>
    <script src=\"/skewer\"></script>
   </head>
   <body>
    %s
   </body>
  </html>")

(defvar *ein:skewer-cell-output-cache* (make-hash-table :test #'equal))

(defun ein:skewer--handle-html (cell string)
  (setf (gethash (slot-value cell 'cell-id) *ein:skewer-cell-output-cache*)
        (format *ein:skewer-html-template* string) ))

;; Format of result is ((id . STR) (type . STR) (status . STR) (value . STR) (time . FLOAT))
(defun ein:execute-javascript (cell json)
  (unless (httpd-running-p) ;; *ein:skewer-running-p*
    (run-skewer))
  (deferred:$
    (deferred:next
      (lambda ()
        (let ((result nil))
          (ein:aif (plist-get json :html)
              (progn
                (let ((cell-id (slot-value cell 'cell-id)))
                  (ein:skewer--handle-html cell it)
                  (setq result (list '(id . nil)
                                     '(type . str)
                                     '(stats . nil)
                                     (cons 'value  (format "Open http://localhost:8080/current-jupyter-cell-output/%s" cell-id))
                                     '(time . nil)))
                  (browse-url (format "http://localhost:8080/current-jupyter-cell-output/%s" cell-id))))
            (skewer-eval (plist-get json :javascript)
                         (lambda (v)
                           (setq result v))
                         :type (if (plist-get json :html)
                                   "html"
                                 "eval")))
          (cl-loop until result
            do (accept-process-output nil 0.01)
            finally (return result)))))
    (deferred:nextc it
      (lambda (result)
        (ein:update-javascript-output cell json result)))))

(provide 'ein-skewer)
