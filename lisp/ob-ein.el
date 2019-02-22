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
;; Modelled after https://github.com/gregsexton/ob-ipython by Greg Sexton
;; Async support based on work by @khinsen on github in ob-ipython-async: https://github.com/khinsen/ob-ipython-async/blob/master/ob-ipython-async.el
;; which was in turn inspired by the scimax starter kit by @jkitchin: https://github.com/jkitchin/scimax

;;; Code:
(require 'ob-python)
(require 'ein-utils)
(require 'ein-notebooklist)
(require 'ein-process)

(defvar *ob-ein-sentinel* "[....]"
  "Placeholder string replaced after async cell execution")

(defcustom ob-ein-anonymous-path ".ob-ein.ipynb"
  "When session header specifies only server, prosecute all ob-ein interactions in this single anonymous notebook."
  :type '(string)
  :group 'ein)

(defcustom ob-ein-inline-image-directory "ein-images"
  "Default directory where to save images generated from ein org-babel source blocks."
  :group 'ein
  :type '(directory))

(defvar org-babel-default-header-args:ein nil)

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

(defun ob-ein--return-mime-type (json file)
  (loop
   for key in ein:output-types-text-preferred
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ((svg image/svg)
      (let ((file (or file (ob-ein--inline-image-info value))))
        (ob-ein--write-base64-image value file)
        (format "[[file:%s]]" file)))
     ((png image/png jpeg image/jpeg)
      (let ((file (or file (ob-ein--inline-image-info value))))
        (ob-ein--write-base64-image value file)
        (format "[[file:%s]]" file)))
     (t (plist-get json type)))))

(defun ob-ein--process-outputs (outputs params)
  (let ((file (cdr (assoc :image params))))
    (ein:join-str "\n"
                  (loop for o in outputs
                        collecting (ob-ein--return-mime-type o file)))))

(defun ob-ein--get-name-create (src-block-info)
  "Get the name of a src block or add a uuid as the name."
  (if-let ((name (fifth src-block-info)))
      name
    (save-excursion
      (let ((el (org-element-context))
            (id (org-id-new 'none)))
        (goto-char (org-element-property :begin el))
        (insert (format "#+NAME: %s\n" id))
        id))))

(defun ein:org-register-lang-mode (lang-name lang-mode)
  "Define org+ein language LANG-NAME with syntax highlighting from LANG-MODE.  Untested.

For example, call (ein:org-register-lang-mode \"ein-R\" 'R) to define a language \"ein-R\" with R syntax highlighting for use with org-babel and ein.

Based on ob-ipython--configure-kernel.
"
  (add-to-list 'org-src-lang-modes `(,lang-name . ,lang-mode))
  (defvaralias (intern (concat "org-babel-default-header-args:" lang-name))
    'org-babel-default-header-args:ein)
  (defalias (intern (concat "org-babel-execute:" lang-name))
    'org-babel-execute:ein))

;;;###autoload
(defun org-babel-execute:ein (body params)
  "This function is called by `org-babel-execute-src-block'."
  (let* ((buffer (current-buffer))
         (processed-params (org-babel-process-params params))
         (result-params (cdr (assq :result-params params)))
         (session (format "%s" (cdr (assoc :session processed-params))))
         (kernelspec (or (cdr (assoc :kernelspec processed-params)) "default"))
         (name (ob-ein--get-name-create (org-babel-get-src-block-info)))
         (full-body (org-babel-expand-body:generic
                     (encode-coding-string body 'utf-8)
                     params
                     (org-babel-variable-assignments:python params)))
         (callback (lambda (notebook)
                    (ob-ein--execute-async
                     buffer
                     full-body
                     (ein:$notebook-kernel notebook)
                     processed-params
                     result-params
                     name))))
    (ob-ein--initiate-session session kernelspec callback))
  *ob-ein-sentinel*)

;;;###autoload
(defun org-babel-execute:ein-hy (body params)
  (org-babel-execute:ein (ein:pytools-wrap-hy-code body) params))

(defsubst ob-ein--execute-async-callback (buffer params result-params name)
  "Callback of 1-arity (the shared output cell) to update org buffer when
`ein:shared-output-eval-string' completes."
  (apply-partially
   (lambda (buffer* params* result-params* name* cell)
     (let* ((raw (ein:aif (ein:oref-safe cell 'traceback)
                     (ansi-color-apply (ein:join-str "\n" it))
                   (ob-ein--process-outputs
                    (ein:oref-safe cell 'outputs) params*)))
            (result (org-babel-result-cond result-params*
                      raw (org-babel-python-table-or-string raw))))
       (ein:log 'debug "ob-ein--execute-async-callback %s %s" name* result)
       (save-excursion
         (save-restriction
           (with-current-buffer buffer*
             (org-babel-goto-named-src-block name*)
             (org-babel-remove-result)
             (org-babel-insert-result
              result
              (cdr (assoc :result-params
                          (third (org-babel-get-src-block-info)))))
             (org-redisplay-inline-images))))))
   buffer params result-params name))

(defun ob-ein--execute-async (buffer body kernel params result-params name)
  "As `ein:shared-output-get-cell' is a singleton, ob-ein can only execute blocks 
one at a time.  Further, we do not order the queued up blocks!"
  (deferred:$
    (deferred:next
      (deferred:lambda ()
        (let ((cell (ein:shared-output-get-cell)))
          (if (eq (slot-value cell 'callback) #'ignore)
              (let ((callback
                     (ob-ein--execute-async-callback buffer params
                                                     result-params name)))
                (setf (slot-value cell 'callback) callback))
            (deferred:nextc (deferred:wait 1200) self)))))
    (deferred:nextc it
      (lambda (_x)
        (ein:shared-output-eval-string kernel body nil)))))

(defun ob-ein--edit-ctrl-c-ctrl-c ()
  "C-c C-c mapping in ein:connect-mode-map."
  (interactive)
  (org-edit-src-save)
  (when (boundp 'org-src--beg-marker)
    (let* ((beg org-src--beg-marker)
           (buf (marker-buffer beg)))
      (with-current-buffer buf
        (save-excursion
          (goto-char beg)
          (org-ctrl-c-ctrl-c))))))

;;;###autoload
(defun org-babel-edit-prep:ein (babel-info)
  "C-c ' enters the lightly tested connect-to-notebook mode."
  (let* ((buffer (current-buffer))
         (processed-params (org-babel-process-params (third babel-info))))
    (ob-ein--initiate-session
     (format "%s" (cdr (assoc :session processed-params)))
     (or (cdr (assoc :kernelspec processed-params)) "default")
     (lambda (notebook)
       (ein:connect-buffer-to-notebook notebook buffer t)
       (define-key ein:connect-mode-map "\C-c\C-c" 'ob-ein--edit-ctrl-c-ctrl-c)))))

(defun ob-ein--parse-session (session)
  (multiple-value-bind (url-or-port _password) (ein:jupyter-server-conn-info)
    (let ((tokens (split-string session "/"))
          (parsed-url (url-generic-parse-url session)))
      (cond ((null (url-host parsed-url))
             (let* ((candidate (apply #'ein:url (car tokens) (cdr tokens)))
                    (parsed-candidate (url-generic-parse-url candidate))
                    (missing (url-scheme-get-property
                              (url-type parsed-candidate)
                              'default-port)))
               (if (and url-or-port
                        (= (url-port parsed-candidate) missing))
                   (apply #'ein:url url-or-port (cdr tokens))
                 candidate)))
            (t (ein:url session))))))

(defun ob-ein--initiate-session (session kernelspec callback)
  "Retrieve notebook based on SESSION path and KERNELSPEC, starting jupyter instance
if necessary.  Install CALLBACK (i.e., cell execution) upon notebook retrieval."
  (let* ((nbpath (ob-ein--parse-session session))
         (parsed-url (url-generic-parse-url nbpath))
         (slash-path (car (url-path-and-query parsed-url)))
         (path (if (string= slash-path "")
                   ob-ein-anonymous-path
                 (substring slash-path 1)))
         (url-or-port (if (string= slash-path "")
                          nbpath
                        (substring nbpath 0 (- (length slash-path)))))
         (notebook (ein:notebook-get-opened-notebook url-or-port path))
         (callback-nbopen (lambda (nb _created)
                            (loop repeat 50
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
    (cond (notebook (funcall callback notebook))
          ((string= (url-host parsed-url) ein:url-localhost)
           (ein:process-refresh-processes)
           (ein:aif (ein:process-url-match nbpath)
               (ein:notebooklist-login (ein:process-url-or-port it) callback-login)
             (ein:jupyter-server-start
              (executable-find ein:jupyter-default-server-command)
              (read-directory-name "Notebook directory: " default-directory)
              nil
              callback-login
              (let* ((port (url-port parsed-url))
                     (avoid (url-scheme-get-property (url-type parsed-url) 'default-port)))
                (cond ((= port avoid) nil)
                      (t (url-port parsed-url)))))))
          (t (ein:notebooklist-login url-or-port callback-login)))))

;;;###autoload
(with-eval-after-load "python"
  (setq python-indent-guess-indent-offset-verbose nil))

;;;###autoload
(add-hook 'org-mode-hook (lambda ()
                           (add-to-list 'org-src-lang-modes '("ein" . python))
                           (add-to-list 'org-src-lang-modes '("ein-hy" . hy))))

(provide 'ob-ein)
