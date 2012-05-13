;;; ein-utils.el --- Utility module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-utils.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-utils.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-utils.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)

(defvar ein:default-port 8888)


(defmacro ein:aif (test-form then-form &rest else-forms)
  "Anaphoric IF."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'ein:aif 'lisp-indent-function 2)

(defmacro ein:aand (test &rest rest)
  "Anaphoric AND."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(ein:aand ,@rest)) 'it))))


(defmacro ein:deflocal (name &optional initvalue docstring)
  "Define permanent buffer local variable named NAME.
INITVALUE and DOCSTRING are passed to `defvar'."
  (declare (indent defun))
  `(progn
     (defvar ,name ,initvalue ,docstring)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))


;;; URL utils

(defvar ein:url-localhost-template "http://127.0.0.1:%s")

(defun ein:url (url-or-port &rest paths)
  (loop with url = (if (integerp url-or-port)
                       (format ein:url-localhost-template url-or-port)
                     url-or-port)
        for p in paths
        do (setq url (concat (ein:trim-right url "/")
                             "/"
                             (ein:trim-left p "/")))
        finally return url))


;;; JSON utils

(defmacro ein:with-json-setting (&rest body)
  `(let ((json-object-type 'plist)
         (json-array-type 'list))
     ,@body))

(defun ein:json-read ()
  "Read json from `url-retrieve'-ed buffer.

* `json-object-type' is `plist'. This is mainly for readability.
* `json-array-type' is `list'.  Notebook data is edited locally thus
  data type must be edit-friendly.  `vector' type is not."
  (goto-char (point-max))
  (backward-sexp)
  (ein:with-json-setting
   (json-read)))

(defun ein:json-read-from-string (string)
  (ein:with-json-setting
   (json-read-from-string string)))


(defun ein:propertize-read-only (string)
  (propertize string 'read-only t 'front-sticky t))


(defun ein:insert-read-only (string)
  (insert (ein:propertize-read-only string)))


(defun ein:trim (string &optional regexp)
  (ein:trim-left (ein:trim-right string regexp) regexp))

(defun ein:trim-left (string &optional regexp)
  (unless regexp (setq regexp "\\s-\\|\n"))
  (ein:trim-regexp string (format "^\\(%s\\)+" regexp)))

(defun ein:trim-right (string &optional regexp)
  (unless regexp (setq regexp "\\s-\\|\n"))
  (ein:trim-regexp string (format "\\(%s\\)+$" regexp)))

(defun ein:trim-regexp (string regexp)
  (if (string-match regexp string)
      (replace-match "" t t string)
    string))

(defmacro ein:case-equal (str &rest clauses)
  "Similar to `case' but comparison is done by `equal'.
Adapted from twittering-mode.el's `case-string'."
  (declare (indent 1))
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

(defun ein:plist-iter (plist)
  "Return list of (key . value) in PLIST."
  (loop for p in plist
        for i from 0
        for key-p = (= (% i 2) 0)
        with key = nil
        if key-p do (setq key p)
        else collect `(,key . ,p)))

(defmacro ein:setf-default (place val)
  "Set VAL to PLACE using `setf' if the value of PLACE is `nil'."
  `(unless ,place
     (setf ,place ,val)))

(defun ein:remove-by-index (list indices)
  "Remove elements from LIST if its index is in INDICES.
NOTE: This function creates new list."
  (loop for l in list
        for i from 0
        when (not (memq i indices))
        collect l))

(defun ein:utils-uuid ()
  "Return string with random (version 4) UUID.
Adapted from org-mode's `org-id-uuid'."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random t)
			  (current-time)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))


(provide 'ein-utils)

;;; ein-utils.el ends here
