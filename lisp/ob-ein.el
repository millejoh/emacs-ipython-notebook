;;; ob-ein.el --- org-babel functions for template evaluation

;; Copyright (C) John M. miller (

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
;; possibly require modes required for your language
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

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:template' function below.
(defun org-babel-expand-body:ein (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-template)
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-template-var-to-template (cdr pair))))
      vars "\n") "\n" body "\n")))

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
(defun org-babel-execute:ein (body params)
  "Execute a block of python code with org-babel by way of
emacs-ipython-notebook's facilities for communicating with
jupyter kernels.
 This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
         (file (cdr (assoc :file params)))
         ;; set the session if the session variable is non-nil
         (session-kernel (org-babel-ein-initiate-session
                          (cdr (assoc :session processed-params))))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (assoc :result-type processed-params)))
         ;; expand the body with `org-babel-expand-body:template'
         (full-body (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                   params (org-babel-variable-assignments:python params))))
    (ein:shared-output-eval-string full-body nil nil session-kernel)
    (let ((cell (ein:shared-output-get-cell)))
      (ein:wait-until #'(lambda ()
                          (not (null (slot-value cell 'outputs)))))
      (slot-value cell 'outputs))))

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
    (string-to-int url-or-port)))

(defun org-babel-ein-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
 Return the initialized session."
  (cond ((string= session "none")
         (error "You must specify a notebook or kernelspec as the session variable for ein code blocks."))
        ((search "/" session)
         (let* ((url-or-port (ein:org-babel-clean-url (car (split-string session "/"))))
                (path (ein:join-str "/" (rest (split-string session "/"))))
                (nb
                 (or (ein:notebook-get-opened-notebook url-or-port path)
                     (ein:notebook-open url-or-port path nil
                                        (lexical-let ((session session))
                                          (lambda ()
                                            (org-babel-ein-initiate-session session)))))))
           (ein:$notebook-kernel nb)))
        (t ;; Hope this is the name of a kernelspec we can start
         (ein:junk-new (ein:junk-notebook-name) session))))

(provide 'ob-ein)
 ;;; ob-ein.el ends here
