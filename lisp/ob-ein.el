;;; ob-ein.el --- org-babel functions for template evaluation

;; Copyright (C) John M. Miller

;; Author: John M. Miller <millejoh at mac.com>
;;

;;; License:


;; This file is NOT part of GNU Emacs.

;; ob-ein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ob-ein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


;; You should have received a copy of the GNU General Public License
;; along with ein-notebooklist.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'ob)
(require 'ob-python)
(require 'cl)
(require 'ein-notebook)
(require 'ein-shared-output)
(require 'ein-utils)
(require 'python)

;; declare default header arguments for this language
(defvar org-babel-default-header-args:ein '())
;; For managing sessions.
(defvar *ein:org-babel-sessions* (make-hash-table))

(add-to-list 'org-src-lang-modes '("ein" . python))

;;

(defun ein:write-base64-image (img-string file)
  (with-temp-file file
    (let ((buffer-read-only nil)
          (buffer-file-coding-system 'binary)
          (require-final-newline nil)
          (file-precious-flag t))
      (insert img-string)
      (base64-decode-region (point-min) (point-max)))))

(defun ein:return-mime-type (json file)
  (loop
   for key in (cond
               ((functionp ein:output-type-preference)
                (funcall ein:output-type-preference json))
               (t ein:output-type-preference))
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ((svg image/svg)
      (progn
        (ein:write-base64-decoded-image value file)
        (format "[[%s]]" file)))
     ((png image/png jpeg image/jpeg)
      (progn
        (ein:write-base64-image value file)
        (format "[[%s]]" file)))
     (t (plist-get json type)))))

(defun org-babel-ein-process-outputs (outputs params)
  (let ((file (cdr (assoc :file params))))
    (loop for o in outputs
          collecting (ein:return-mime-type o file))))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;

(defcustom ein:org-execute-timeout 30
  "Query timeout, in seconds, for executing ein source blocks in
  org files."
  :type 'number
  :group 'ein)

(defun org-babel-execute:ein (body params)
  "Execute a block of python code with org-babel by way of
emacs-ipython-notebook's facilities for communicating with
jupyter kernels.
 This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
         (kernelspec (cdr (assoc :kernelspec params)))
         ;; set the session if the session variable is non-nil
         (session-kernel (org-babel-ein-initiate-session
                          (cdr (assoc :session processed-params))
                          kernelspec))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (assoc :result-type processed-params)))
         ;; expand the body with `org-babel-expand-body:template'
         (full-body (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                   params (org-babel-variable-assignments:python params))))
    (ein:shared-output-eval-string full-body nil nil session-kernel)
    (let ((cell (ein:shared-output-get-cell)))
      (ein:wait-until #'(lambda ()
                          (null (slot-value cell 'running)))
                      nil ein:org-execute-timeout)
      (if (and (slot-boundp cell 'traceback)
               (slot-value cell 'traceback))
          (ansi-color-apply (apply #'concat (mapcar #'(lambda (s)
                                                        (format "%s\n" s))
                                                    (slot-value cell 'traceback))))
        (org-babel-ein-process-outputs (slot-value cell 'outputs) processed-params)))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:ein (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-ein-var-to-template (var)
  "Convert an elisp var into a string of template source code
 specifying a var of the same value."
  (format "%S" var))

(defun org-babel-ein-table-or-string (results)
  "If the results look like a table, then convert them into an
 Emacs-lisp table, otherwise return the results as a string."
  )

(defun ein:org-babel-clean-url (url-or-port)
  (if (search ":" url-or-port)
      url-or-port
    (string-to-number url-or-port)))

(defun ein:org-babel-parse-session (session)
  (cond ((numberp session)
         (values session nil))
        ((search "/" session)
         (let* ((url-or-port (ein:org-babel-clean-url (car (split-string session "/"))))
                (path (ein:join-str "/" (rest (split-string session "/")))))
           (values url-or-port path)))
        (t (values (ein:org-babel-clean-url session) nil))))

(defcustom ein:org-babel-default-session-name "ein_babel_session.ipynb"
  "Default name for org babel sessions running ein environments.
This is the name of the notebook used when no notebook path is
given in the session parameter."
  :type '(string :tag "Format string")
  :group 'ein)

(defun org-babel-ein-initiate-session (&optional session kernelspec)
  "If there is not a current inferior-process-buffer in SESSION then create.
 Return the initialized session."
  (when (and (stringp session) (string= session "none"))
    (error "You must specify a notebook or kernelspec as the session variable for ein code blocks."))
  (multiple-value-bind (url-or-port path) (ein:org-babel-parse-session session)
    (if (null (gethash url-or-port ein:available-kernelspecs))
        (ein:query-kernelspecs url-or-port))
    (if (null kernelspec)
        (setq kernelspec (ein:get-kernelspec url-or-port "default")))
    (cond ((null path)
           (let* ((name ein:org-babel-default-session-name)
                  (new-session (format "%s/%s" url-or-port name)))
             (ein:notebooklist-new-notebook-with-name name kernelspec url-or-port)
             (org-babel-ein-initiate-session new-session kernelspec)))
          (t (let ((nb (or (ein:notebook-get-opened-notebook url-or-port path)
                           (ein:notebook-open url-or-port path kernelspec
                                              (lexical-let ((session session)
                                                            (kernelspec kernelspec))
                                                (lambda ()
                                                  (org-babel-ein-initiate-session session kernelspec)))))))
               (ein:$notebook-kernel nb))))))

(provide 'ob-ein)
 ;;; ob-ein.el ends here
