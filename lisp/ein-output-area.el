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

(require 'ein-core)



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
  :group 'ein)

(defun ein:shr-insert-document (dom)
  "`shr-insert-document' with EIN setting."
  (eval `(let ,ein:shr-env (shr-insert-document dom))))

(defun ein:insert-html-shr (html-string)
  "Render HTML-STRING using `shr-insert-document'.

Usage::

    (ein:insert-html-shr \"<b>HTML</b> string\")

"
  (let ((start (point))
        end)
    (ein:shr-insert-document
     (with-temp-buffer
       (erase-buffer)
       (insert html-string)
       ;; FIXME: If URLs are local, they should be adapted here.
       (libxml-parse-html-region (point-min) (point-max))))
    (setq end (point))
    (put-text-property start end 'read-only t)
    (put-text-property start end 'front-sticky t)))


(provide 'ein-output-area)

;;; ein-output-area.el ends here
