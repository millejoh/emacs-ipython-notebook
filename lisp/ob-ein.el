;; -*- lexical-binding: t -*-
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
;; along with ob-ein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Support executing org-babel source blocks using EIN worksheets.
;; Async support based on work by @khinsen on github in ob-ipython-async: https://github.com/khinsen/ob-ipython-async/blob/master/ob-ipython-async.el
;; which was in turn inspired by the scimax starter kit by @jkitchin: https://github.com/jkitchin/scimax

;;; Code:
(eval-when-compile (require 'cl))
(require 'ob)
(require 'ob-python)
(require 'ein-shared-output)
(require 'ein-utils)
(require 'python)

(autoload 'org-element-property "org-element")

(defcustom ein:org-async-p t
  "If non-nil run ein org-babel source blocks asyncronously."
  :group 'ein
  :type 'boolean)

(defcustom ein:org-inline-image-directory "ein-images"
  "Default directory where to save images generated from ein org-babel source blocks."
  :group 'ein
  :type '(directory))

;; declare default header arguments for this language
(defvar org-babel-default-header-args:ein '())

(add-to-list 'org-src-lang-modes '("ein" . python))
(add-to-list 'org-src-lang-modes '("ein-hy" . hy))

;; based on ob-ipython--configure-kernel
(defun ein:org-register-lang-mode (lang-name lang-mode)
  "Define org+ein language LANG-NAME with syntax highlighting from LANG-MODE.
For example, call (ein:org-register-lang-mode \"ein-R\" 'R) to define a language \"ein-R\" with R syntax highlighting for use with org-babel and ein."
  (add-to-list 'org-src-lang-modes `(,lang-name . ,lang-mode))
  (defvaralias (intern (concat "org-babel-default-header-args:" lang-name))
    'org-babel-default-header-args:ein)
  (defalias (intern (concat "org-babel-execute:" lang-name))
    'org-babel-execute:ein)
  (defalias (intern (concat "org-babel-" lang-name "-initiate-session"))
    'org-babel-ein-initiate-session))

;; Handling source block execution results
(defun ein:temp-inline-image-info (value)
  (let* ((f (md5 value))
         (d ein:org-inline-image-directory)
         (tf (concat d "/ob-ein-" f ".png")))
    (unless (file-directory-p d)
      (make-directory d 'parents))
    tf))

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
   for key in ein:output-types-text-preferred
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ((svg image/svg)
      (let ((file (or file (ein:temp-inline-image-info value))))
        (ein:write-base64-image value file)
        (format "[[file:%s]]" file)))
     ((png image/png jpeg image/jpeg)
      (let ((file (or file (ein:temp-inline-image-info value))))
        (ein:write-base64-image value file)
        (format "[[file:%s]]" file)))
     (t (plist-get json type)))))

(defun org-babel-ein-process-outputs (outputs params)
  (let ((file (cdr (assoc :image params))))
    (ein:join-str "\n"
                  (loop for o in outputs
                        collecting (ein:return-mime-type o file)))))

;; Asynchronous execution requires each source code block to
;; be named. For blocks that have no name, an automatically
;; generated name is added. This variable holds the function
;; that generates the name.
(defvar *ein:org-name-generator* 'ein:uuid-generator
  "Function to generate a name for a src block.
The default is `ein:uuid-generator'.")

(defun ein:uuid-generator ()
  (org-id-new 'none))

(defun ein:org-get-name-create ()
  "Get the name of a src block or add a uuid as the name."
  (if-let ((name (fifth (org-babel-get-src-block-info))))
      name
    (save-excursion
      (let ((el (org-element-context))
	          (id (funcall *ein:org-name-generator*)))
	      (goto-char (org-element-property :begin el))
	      (insert (format "#+NAME: %s\n" id))
	      id))))

(defcustom ein:org-execute-timeout 30
  "Query timeout, in seconds, for executing ein source blocks in
  org files."
  :type 'number
  :group 'ein)

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
         (kernelspec (cdr (assoc :kernelspec params)))
         ;; set the session if the session variable is non-nil
         (session-kernel (org-babel-ein-initiate-session
                          (cdr (assoc :session processed-params))
                          kernelspec))
         ;; either OUTPUT or VALUE which should behave as described above
         ;; (result-type (cdr (assoc :result-type processed-params)))
         ;; expand the body with `org-babel-expand-body:template'
         (full-body (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                   params
                                                   (org-babel-variable-assignments:python params))))
    (if ein:org-async-p
        (ein:ob-ein--execute-async full-body session-kernel processed-params (ein:org-get-name-create))
      (ein:ob-ein--execute full-body session-kernel processed-params))))

(defun org-babel-execute:ein-hy (body params)
  (org-babel-execute:ein (ein:pytools-wrap-hy-code body) params))

(defun ein:ob-ein--execute-async (body kernel params name)
  (let ((buffer (current-buffer))
        (name name)
        (body body)
        (kernel kernel)
        (params params))
    (deferred:$
      (deferred:next
        (lambda ()
          (message "Starting deferred ein execution: %s" name)
          (ein:shared-output-eval-string body nil nil kernel)))
      (deferred:nextc it
        (deferred:lambda ()
          (let ((cell (ein:shared-output-get-cell)))
            (if (not (null (slot-value cell 'running)))
                (deferred:nextc (deferred:wait 50) self)))))
      (deferred:nextc it
        (lambda ()
          (let ((cell (ein:shared-output-get-cell)))
            (if (and (slot-boundp cell 'traceback)
                     (slot-value cell 'traceback))
                (ansi-color-apply (apply #'concat (mapcar #'(lambda (s)
                                                              (format "%s\n" s))
                                                          (slot-value cell 'traceback))))
              (org-babel-ein-process-outputs (slot-value cell 'outputs) params)))))
      (deferred:nextc it
        (lambda (formatted-result)
          (ein:ob-ein--execute-async-update formatted-result buffer name))))
    (format "[[ob-ein-async-running: %s]]" name)))

(defun ein:ob-ein--execute-async-update (formatted-result buffer name)
  (message "Finished deferred ein execution: %s" name)
  (with-current-buffer buffer
    (save-excursion
      (org-babel-goto-named-result name)
      (search-forward (format "[[ob-ein-async-running: %s]]" name))
      (replace-match formatted-result t t)
      (org-redisplay-inline-images)
      ;; (when (member "drawer" (cdr (assoc :result-params params)))
      ;;   ;; open the results drawer
      ;;   (org-babel-goto-named-result name)
      ;;   (forward-line)
      ;; (org-flag-drawer nil))
      )))

(defun ein:ob-ein--execute (full-body session-kernel processed-params)
  (let* ((d (ein:shared-output-eval-string full-body nil nil session-kernel))
         (cell (ein:shared-output-get-cell)))
    (deferred:sync! d)
    (ein:wait-until #'(lambda ()
                        (null (slot-value cell 'running)))
                    nil ein:org-execute-timeout)
    (if (and (slot-boundp cell 'traceback)
             (slot-value cell 'traceback))
        (ansi-color-apply (apply #'concat (mapcar #'(lambda (s)
                                                      (format "%s\n" s))
                                                  (slot-value cell 'traceback))))
      (org-babel-ein-process-outputs (slot-value cell 'outputs) processed-params))))


(defun ein:org-find-or-open-session (session &optional kernelspec)
  (multiple-value-bind (url-or-port path) (ein:org-babel-parse-session session)
    (setf kernelspec (or kernelspec (ein:get-kernelspec url-or-port "default")))
    (let ((nb (or (ein:notebook-get-opened-notebook url-or-port path)
                  (ein:notebook-open url-or-port
                                     path
                                     kernelspec
                                     (apply-partially 
                                      (lambda (session* kernelspec* _notebook _created)
                                        (org-babel-ein-initiate-session session* kernelspec*))
                                      session kernelspec)))))

      (loop repeat 4
            until (ein:kernel-live-p (ein:$notebook-kernel nb))
            do (sit-for 1.0))
      nb)))

(defun org-babel-edit-prep:ein (babel-info)
  "Set up source code completion for editing an EIN source block."
  (let ((nb (ein:org-find-or-open-session (cdr (assoc :session (third babel-info))))))
    (ein:connect-buffer-to-notebook nb (current-buffer) t)
    (define-key ein:connect-mode-map "\C-c\C-c" 'org-babel-edit:ein-execute)))

(defun org-babel-edit:ein-execute ()
  (interactive)
  (org-edit-src-save)
  (when (boundp 'org-src--beg-marker)
    (let* ((beg org-src--beg-marker)
           (buf (marker-buffer beg)))
      (with-current-buffer buf
        (save-excursion
          (goto-char beg)
          (org-ctrl-c-ctrl-c))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:ein (_session _params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-ein-var-to-template (var)
  "Convert an elisp var into a string of template source code
 specifying a var of the same value."
  (format "%S" var))

(defun org-babel-ein-table-or-string (_results)
  "If the results look like a table, then convert them into an
 Emacs-lisp table, otherwise return the results as a string."
  )

(defun ein:org-babel-clean-url (url-or-port)
  (if (search ":" url-or-port)
      url-or-port
    (string-to-number url-or-port)))

(defun ein:org-babel-parse-session (session)
  (if (numberp session)
      (values (ein:url (format "http://localhost:%s" session)) nil)
    (let ((session-uri (url-generic-parse-url session)))
      (cond ((url-fullness session-uri)
             (values (ein:url (format "%s://%s:%s" (url-type session-uri) (url-host session-uri) (url-port session-uri)))
                     (url-filename session-uri)))
            (t (let* ((url-or-port (ein:org-babel-clean-url (car (split-string session "/"))))
                      (path (ein:join-str "/" (rest (split-string session "/")))))
                 (values (ein:url (format "http://localhost:%s" url-or-port)) path)))))))

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
    (when (null kernelspec)
      ;; Now is not the time to be getting kernelspecs.
      ;; If I must do so, need to inject a deferred callback chain like
      ;; in ein:notebooklist
      ;; (if (null (gethash url-or-port ein:available-kernelspecs))
      ;;     (ein:query-kernelspecs url-or-port))
      (setq kernelspec (ein:get-kernelspec url-or-port "default")))
    (cond ((null path)
           (let* ((name ein:org-babel-default-session-name)
                  (new-session (format "%s/%s" url-or-port name)))
             (ein:notebooklist-new-notebook-with-name name kernelspec url-or-port)
             (org-babel-ein-initiate-session new-session kernelspec)))
          (t (let ((nb (ein:org-find-or-open-session session kernelspec)))
               (ein:$notebook-kernel nb))))))

(provide 'ob-ein)
 ;;; ob-ein.el ends here
