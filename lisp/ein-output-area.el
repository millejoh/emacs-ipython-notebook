;;; ein-output-area.el --- Output area module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-output-area.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; ein-output-area.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-output-area.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)

(require 'ein-core)



;;; XML/HTML utils

(defun ein:xml-parse-html-string (html-string)
  "Parse HTML-STRING and return a dom object which
can be handled by the xml module."
  (with-temp-buffer
    (erase-buffer)
    (insert html-string)
    (if (fboundp 'libxml-parse-html-region)
        (libxml-parse-html-region (point-min) (point-max)))))

(defalias 'ein:xml-node-p 'listp)

(defun ein:xml-tree-apply (dom operation)
  "Apply OPERATION on nodes in DOM.  Apply the same OPERATION on
the next level children when it returns `nil'."
  (loop for child in (xml-node-children dom)
        if (and (not (funcall operation child))
                (ein:xml-node-p child))
        do (ein:xml-tree-apply child operation)))

(defun ein:xml-replace-attributes (dom tag attr replace-p replacer)
  "Replace value of ATTR of TAG in DOM using REPLACER
when REPLACE-P returns non-`nil'."
  (ein:xml-tree-apply
   dom
   (lambda (node)
     (ein:and-let* (((ein:xml-node-p node))
                    ((eq (xml-node-name node) tag))
                    (attr-cell (assoc attr (xml-node-attributes node)))
                    (val (cdr attr-cell))
                    ((funcall replace-p val)))
       (setcdr attr-cell (funcall replacer val))
       t))))


;;; HTML renderer

(defun ein:output-area-get-html-renderer ()
  ;; FIXME: make this configurable
  (cond
   ((and (fboundp 'shr-insert-document)
         (fboundp 'libxml-parse-xml-region))
    #'ein:insert-html-shr)
   (t #'ein:insert-read-only)))

(defcustom ein:shr-env
  '((shr-table-horizontal-line ?-)
    (shr-table-vertical-line ?|)
    (shr-table-corner ?+))
  "Variables let-bound while calling `shr-insert-document'.

To use default shr setting::

    (setq ein:shr-env nil)

Draw boundaries for table (default)::

    (setq ein:shr-env
          '((shr-table-horizontal-line ?-)
            (shr-table-vertical-line ?|)
            (shr-table-corner ?+)))
"
  :type '(sexp)
  :group 'ein)

(defun ein:shr-insert-document (dom)
  "`shr-insert-document' with EIN setting."
  (eval `(let ,ein:shr-env (shr-insert-document dom))))

(defun ein:insert-html-shr (html-string)
  "Render HTML-STRING using `shr-insert-document'.

Usage::

    (ein:insert-html-shr \"<b>HTML</b> string\")

"
  (let ((dom (ein:xml-parse-html-string html-string))
        (start (point))
        end)
    (ein:insert-html--fix-urls dom)
    (ein:shr-insert-document dom)
    (setq end (point))
    (put-text-property start end 'read-only t)
    (put-text-property start end 'front-sticky t)))

(defun ein:insert-html--fix-urls (dom &optional url-or-port)
  "Destructively prepend notebook server URL to local URLs in DOM."
  (ein:and-let* ((url-or-port (or url-or-port (ein:get-url-or-port)))
                 (replace-p (lambda (val) (string-match-p "^/?files/" val)))
                 (replacer (lambda (val) (ein:url url-or-port val))))
    (ein:xml-replace-attributes dom 'a 'href replace-p replacer)
    (ein:xml-replace-attributes dom 'img 'src replace-p replacer)))


(provide 'ein-output-area)

;;; ein-output-area.el ends here
