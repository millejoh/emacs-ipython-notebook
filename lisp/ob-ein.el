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

;;; Credits:

;; Uses code from https://github.com/gregsexton/ob-ipython (MIT License)

;;; Code:
(require 'ob)
(require 'ein-utils)
(require 'ein-cell)
(require 'anaphora)

(autoload 'org-element-property "org-element")
(autoload 'org-element-context "org-element")
(autoload 'org-element-type "org-element")
(autoload 'org-id-new "org-id")
(autoload 'org-redisplay-inline-images "org" nil t)
(autoload 'ein:notebooklist-new-notebook-with-name "ein-notebooklist")
(autoload 'ein:notebooklist-canonical-url-or-port "ein-notebooklist")
(autoload 'ein:notebooklist-login "ein-notebooklist" nil t)
(autoload 'ein:notebook-get-opened-notebook "ein-notebook")
(autoload 'ein:notebook-url "ein-notebook")
(autoload 'ein:notebook-open "ein-notebook")
(autoload 'ein:notebook-close "ein-notebook")
(autoload 'ein:process-url-or-port "ein-process")
(autoload 'ein:process-url-match "ein-process")
(autoload 'ein:process-refresh-processes "ein-process")
(autoload 'ein:jupyter-my-url-or-port "ein-jupyter")
(autoload 'ein:jupyter-server-start "ein-jupyter" nil t)
(autoload 'ein:shared-output-get-cell "ein-shared-output")
(autoload 'ein:shared-output-eval-string "ein-shared-output")
(autoload 'ein:kernel-live-p "ein-kernel")
(autoload 'ein:query-singleton-ajax "ein-query")
(autoload 'ein:output-area-case-type "ein-output-area")
(autoload 'ein:log "ein-log")

(defvar *ob-ein-sentinel* "[....]"
  "Placeholder string replaced after async cell execution")

(defcustom ob-ein-timeout-seconds 600
  "Maximum seconds to wait for block to finish (for synchronous operations)."
  :type 'integer
  :group 'ein)

(defcustom ob-ein-languages
  '(("ein" . python)
    ("ein-python" . python)
    ("ein-R" . R)
    ("ein-r" . R)
    ("ein-julia" . julia))
  "ob-ein has knowledge of these (ein-LANG . LANG-MODE) pairs."
  :type '(repeat (cons string symbol))
  :group 'ein)

(defcustom ob-ein-anonymous-path "ob-%s.ipynb"
  "Applies when session header doesn't specify ipynb.
Prosecute all interactions for a given language in this throwaway
notebook (substitute %s with language)."
  :type '(string)
  :group 'ein)

(defun ob-ein-anonymous-p (path)
  "Return t if PATH looks like ob-ein-anonymous-path.  Fragile"
  (string-match (replace-regexp-in-string "%s" ".+"
                  (replace-regexp-in-string "\\." "\\\\." ob-ein-anonymous-path))
                path))

(defcustom ob-ein-inline-image-directory "ein-images"
  "Store ob-ein images here."
  :group 'ein
  :type 'directory)

(defcustom ob-ein-default-header-args:ein nil
  "No documentation."
  :group 'ein
  :type '(repeat string))

(defun ob-ein--inline-image-info (value)
  (let* ((f (md5 value))
         (d ob-ein-inline-image-directory)
         (tf (concat d "/ob-ein-" f ".png")))
    (unless (file-directory-p d)
      (make-directory d 'parents))
    tf))

(defun ob-ein--write-base64-image (img-string file)
  (with-temp-file file
    (let ((buffer-read-only nil)
          (buffer-file-coding-system 'binary)
          (require-final-newline nil)
          (file-precious-flag t))
      (insert img-string)
      (base64-decode-region (point-min) (point-max)))))

(defun ob-ein--proxy-images (json explicit-file)
  (let (result
        (ein:output-area-case-types '(:image/svg+xml :image/png :image/jpeg :text/plain :application/latex :application/tex :application/javascript)))
    (ein:output-area-case-type
     json
     (cl-case type
       ((:image/svg+xml :image/png :image/jpeg)
        (let ((file (or explicit-file (ob-ein--inline-image-info value))))
          (ob-ein--write-base64-image value file)
          (setq result (format "[[file:%s]]" file))))
       (otherwise
        (setq result value))))
    result))

(defun ob-ein--process-outputs (result-type cell params)
  (let* ((session (aand (cdr (assoc :session params))
			(unless (string= "none" it)
			  (format "%s" it))))
	 (render (let ((stdout-p
			(lambda (out)
			  (and (equal "stream" (plist-get out :output_type))
			       (equal "stdout" (plist-get out :name))))))
		   (if (eq result-type 'output)
		       (lambda (out)
			 (if (funcall stdout-p out)
			     (plist-get out :text)
			   (when session ;; should aways be true under ob-ein
			     (concat (ob-ein--proxy-images
				       out (cdr (assoc :image params)))
				     "\n"))))
		     (lambda (out)
		       (and (not (funcall stdout-p out))
			    (concat (ob-ein--proxy-images
				       out (cdr (assoc :image params)))
				     "\n"))))))
	 (outputs (cl-loop for out in (ein:oref-safe cell 'outputs)
			   collect (funcall render out))))
    (when outputs
      (ansi-color-apply (ein:join-str "" outputs)))))

(defun ob-ein--get-name-create (src-block-info)
  "Get the name of a src block or add a uuid as the name."
  (if-let ((name (cl-fifth src-block-info)))
      name
    (save-excursion
      (let ((el (org-element-context))
            (id (org-id-new 'none)))
        (goto-char (org-element-property :begin el))
        (back-to-indentation)
        (split-line)
        (insert (format "#+NAME: %s" id))
        id))))

(defun ob-ein--babelize-lang (lang-name lang-mode)
  "Stand-up LANG-NAME as a babelized language with LANG-MODE syntax table.

Based on ob-ipython--configure-kernel."
  (add-to-list 'org-src-lang-modes `(,lang-name . ,lang-mode))
  (defvaralias (intern (concat "org-babel-default-header-args:" lang-name))
    'ob-ein-default-header-args:ein)
  (fset (intern (concat "org-babel-execute:" lang-name))
        `(lambda (body params)
           "Should get rid of accommodating org-babel-variable-assignments.
We don't test it, and finding a module named ob-LANG-MODE won't work generally,
e.g., ob-c++ is not ob-C.el."
           (require (quote ,(intern (format "ob-%s" lang-mode))) nil t)
           ;; hack because ob-ein loads independently of ein
           (custom-set-variables '(python-indent-guess-indent-offset-verbose nil))
           (let ((parser
                  (quote
                   ,(intern (format "org-babel-variable-assignments:%s" lang-mode)))))
             (ob-ein--execute-body
              (if (fboundp parser)
                  (org-babel-expand-body:generic
                   body params (funcall (symbol-function parser) params))
                body)
              params)))))

(defun ob-ein--execute-body (body params)
  (let* ((buffer (current-buffer))
	 (result-type (cdr (assq :result-type params)))
	 (result-params (cdr (assq :result-params params)))
         (session (or (aand (cdr (assoc :session params))
                            (unless (string= "none" it)
                              (format "%s" it)))
                      ein:url-localhost))
         (lang (nth 0 (org-babel-get-src-block-info)))
         (kernelspec (or (cdr (assoc :kernelspec params))
                         (aif (cdr (assoc lang org-src-lang-modes))
                             (cons 'language (format "%s" it))
                           (error "ob-ein--execute-body: %s not among %s"
                                  lang (mapcar #'car org-src-lang-modes)))))
         (name (ob-ein--get-name-create (org-babel-get-src-block-info)))
         (callback (lambda (notebook)
                    (ob-ein--execute-async
                     buffer
                     body
                     (ein:$notebook-kernel notebook)
                     params
		     result-type
                     result-params
                     name))))
    (save-excursion
      (cl-assert (not (stringp (org-babel-goto-named-src-block name))))
      (org-babel-insert-result *ob-ein-sentinel* result-params))
    (ob-ein--initiate-session session kernelspec callback)
    (if (ein:eval-if-bound 'org-current-export-file)
        (save-excursion
          (cl-loop with interval = 2000
                with pending = t
                repeat (/ (* ob-ein-timeout-seconds 1000) interval)
                do (progn
                     (org-babel-goto-named-result name)
                     (forward-line 1)
                     (setq pending (re-search-forward
                                    (regexp-quote *ob-ein-sentinel*)
                                    (org-babel-result-end) t)))
                until (not pending)
                do (sleep-for 0 interval)
                finally return
                (if pending
                    (prog1 ""
                      (ein:log 'error "ob-ein--execute-body: %s timed out" name))
                  (ob-ein--process-outputs result-type
					   (ein:shared-output-get-cell)
					   params))))
      (org-babel-remove-result)
      *ob-ein-sentinel*)))

(defun ob-ein--execute-async-callback (buffer params result-type result-params name)
  "Return callback of 1-arity (the shared output cell) to update org buffer when
`ein:shared-output-eval-string' completes.

The callback returns t if results containt RESULT-TYPE outputs, nil otherwise."
  (apply-partially
   (lambda (buffer* params* result-type* result-params* name* cell)
     (when-let ((raw (aif (ein:oref-safe cell 'traceback)
			 (ansi-color-apply (ein:join-str "\n" it))
		       (ob-ein--process-outputs result-type* cell params*))))
       (prog1 t
	 (let ((result
		(let ((tmp-file (org-babel-temp-file "ein-")))
		  (with-temp-file tmp-file (insert raw))
		  (org-babel-result-cond result-params*
		    raw (org-babel-import-elisp-from-file tmp-file '(16)))))
	       (info (org-babel-get-src-block-info 'light)))
	   (ein:log 'debug "ob-ein--execute-async-callback %s \"%s\" %s"
		    name* result buffer*)
	   (save-excursion
	     (save-restriction
	       (with-current-buffer buffer*
		 (unless (stringp (org-babel-goto-named-src-block name*)) ;; stringp=error
		   (when (version-list-< (version-to-list (org-release)) '(9))
		     (when info ;; kill #+RESULTS: (no-name)
		       (setf (nth 4 info) nil)
		       (org-babel-remove-result info))
		     (org-babel-remove-result)) ;; kill #+RESULTS: name
		   (org-babel-insert-result
		    result
		    (cdr (assoc :result-params
				(cl-third (org-babel-get-src-block-info)))))
		   (org-redisplay-inline-images)))))))))
   buffer params result-type result-params name))

(defun ob-ein--execute-async-clear (buffer result-params name)
  "Return function of 0-arity to clear *ob-ein-sentinel*."
  (apply-partially
   (lambda (buffer* result-params* name*)
     (let ((info (org-babel-get-src-block-info 'light)))
       (save-excursion
	 (save-restriction
	   (with-current-buffer buffer*
	     (unless (stringp (org-babel-goto-named-src-block name*)) ;; stringp=error
	       (when info ;; kill #+RESULTS: (no-name)
		 (setf (nth 4 info) nil)
		 (org-babel-remove-result info))
	       (org-babel-remove-result) ;; kill #+RESULTS: name
	       (org-babel-insert-result "" result-params*)
	       (org-redisplay-inline-images)))))))
   buffer result-params name))

(defun ob-ein--execute-async (buffer body kernel params result-type result-params name)
  "As `ein:shared-output-get-cell' is a singleton, ob-ein can only execute blocks
one at a time.  Further, we do not order the queued up blocks!"
  (deferred:$
    (deferred:next
      (deferred:lambda ()
        (let ((cell (ein:shared-output-get-cell)))
          (if (eq (slot-value cell 'callback) #'ignore)
              (let ((callback (ob-ein--execute-async-callback
			       buffer params result-type
			       result-params name))
		    (clear (ob-ein--execute-async-clear buffer result-params name)))
                (setf (slot-value cell 'callback) callback)
		(setf (slot-value cell 'clear) clear))
            ;; still pending previous callback
            (deferred:nextc (deferred:wait 1200) self)))))
    (deferred:nextc it
      (lambda (_x)
        (ein:shared-output-eval-string kernel body)))))

(defun ob-ein--parse-session (session)
  (let* ((url-or-port (ein:jupyter-my-url-or-port))
         (tokens (split-string session "/"))
         (parsed-url (url-generic-parse-url session))
         (url-host (url-host parsed-url)))
    (cond ((null url-host)
           (let* ((candidate (apply #'ein:url (car tokens) (cdr tokens)))
                  (parsed-candidate (url-generic-parse-url candidate))
                  (missing (url-scheme-get-property
                            (url-type parsed-candidate)
                            'default-port)))
             (if (and url-or-port
                      (= (url-port parsed-candidate) missing))
                 (apply #'ein:url url-or-port (cdr tokens))
               candidate)))
          (t (ein:url session)))))

(defun ob-ein--initiate-session (session kernelspec callback)
  "Retrieve notebook of SESSION path and KERNELSPEC.
Start jupyter instance if necessary.
Install CALLBACK (i.e., cell execution) upon notebook retrieval."
  (let* ((nbpath (ob-ein--parse-session session))
         (info (org-babel-get-src-block-info))
         (anonymous-path (format ob-ein-anonymous-path (nth 0 info)))
         (parsed-url (url-generic-parse-url nbpath))
         (slash-path (car (url-path-and-query parsed-url)))
         (_ (awhen (cdr (url-path-and-query parsed-url))
              (error "Cannot handle :session `%s`" it)))
         (ipynb-p (file-name-extension (file-name-nondirectory slash-path)))
         (path (if ipynb-p
                   (file-name-nondirectory slash-path)
                 anonymous-path))
         (url-or-port (directory-file-name
                       (if ipynb-p
                           (cl-subseq nbpath 0 (- (length path)))
                         nbpath)))
         (notebook (ein:notebook-get-opened-notebook url-or-port path))
         (callback-nbopen (lambda (nb _created)
                            (cl-loop repeat 50
                                  for live-p = (ein:kernel-live-p (ein:$notebook-kernel nb))
                                  until live-p
                                  do (sleep-for 0 300)
                                  finally
                                  do (if (not live-p)
                                         (ein:log 'error
                                           "Kernel for %s failed to launch"
                                           (ein:$notebook-notebook-name nb))
                                       (funcall callback nb)))))
         (errback-nbopen (lambda (url-or-port status-code)
                           (if (eq status-code 404)
                               (ein:notebooklist-new-notebook-with-name
                                url-or-port kernelspec path callback-nbopen t))))
         (callback-login (lambda (_buffer url-or-port)
                           (ein:notebook-open url-or-port path kernelspec
                                              callback-nbopen errback-nbopen t))))
    (cond ((and notebook
                (string= path anonymous-path)
                (stringp kernelspec)
                (not (equal (ein:$kernelspec-name (ein:$notebook-kernelspec notebook))
                            kernelspec)))
           (ein:log 'debug "ob-ein--initiate-session: switching %s from %s to %s"
                    path (ein:$kernelspec-name (ein:$notebook-kernelspec notebook))
                    kernelspec)
           (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
             (ein:notebook-close notebook))
           (ein:query-singleton-ajax (ein:notebook-url notebook)
            :type "DELETE")
           (cl-loop repeat 8
                    with fullpath = (concat (file-name-as-directory nbpath) path)
                    for extant = (file-exists-p fullpath)
                    until (not extant)
                    do (sleep-for 0 500)
                    finally do (if extant
                                   (ein:display-warning
                                    (format "cannot delete path=%s nbpath=%s"
                                            fullpath nbpath))
                                 (ob-ein--initiate-session session kernelspec callback))))
          (notebook (funcall callback notebook))
          ((string= (url-host parsed-url) ein:url-localhost)
           (ein:process-refresh-processes)
           (aif (ein:process-url-match nbpath)
               (ein:notebooklist-login (ein:process-url-or-port it) callback-login)
             (ein:jupyter-server-start
              (executable-find (or (ein:eval-if-bound 'ein:jupyter-server-command)
                                   "jupyter"))
              (read-directory-name "Notebook directory: " default-directory)
              nil
              callback-login
              (let* ((port (url-port parsed-url))
                     (avoid (url-scheme-get-property (url-type parsed-url) 'default-port)))
                (cond ((= port avoid) nil)
                      (t (url-port parsed-url)))))))
          (t (ein:notebooklist-login url-or-port callback-login)))))

(cl-loop for (lang . mode) in ob-ein-languages
         do (ob-ein--babelize-lang lang mode))

(defun ob-ein-kernel-interrupt ()
  "Interrupt kernel associated with session."
  (interactive)
  (org-babel-when-in-src-block
   (-if-let* ((info (org-babel-get-src-block-info))
              (pparams (cl-callf org-babel-process-params (nth 2 info)))
              (params (nth 2 info))
              (session (or (aand (cdr (assoc :session params))
                                 (unless (string= "none" it)
                                   (format "%s" it)))
                           ein:url-localhost))
              (nbpath (ob-ein--parse-session session))
              (anonymous-path (format ob-ein-anonymous-path (nth 0 info)))
              (parsed-url (url-generic-parse-url nbpath))
              (slash-path (car (url-path-and-query parsed-url)))
              (path (if (string= slash-path "") anonymous-path
                      (substring slash-path 1)))
              (url-or-port (if (string= slash-path "")
                               nbpath
                             (substring nbpath 0 (- (length slash-path)))))
              (notebook (ein:notebook-get-opened-notebook url-or-port path))
              (kernel (ein:$notebook-kernel notebook)))
       (ein:kernel-interrupt kernel)
     (ein:log 'info "ob-ein-kernel-interrupt: nothing to interrupt"))))

(define-key org-babel-map "\C-k" 'ob-ein-kernel-interrupt)

;;;###autoload
(when (featurep 'org)
  (let* ((orig (get 'org-babel-load-languages 'custom-type))
         (orig-cdr (cdr orig))
         (choices (plist-get orig-cdr :key-type)))
    (push '(const :tag "Ein" ein) (nthcdr 1 choices))
    (put 'org-babel-load-languages 'custom-type
         (cons (car orig) (plist-put orig-cdr :key-type choices)))))

(provide 'ob-ein)
