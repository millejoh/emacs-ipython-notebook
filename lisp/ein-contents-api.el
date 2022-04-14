;;; ein-contents-api.el --- Interface to Jupyter's Contents API  -*- lexical-binding:t -*-

;; Copyright (C) 2015 - John Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-contents-api.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-contents-api.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebooklist.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; An interface to the Jupyter Contents API as described in
;;; https://github.com/ipython/ipython/wiki/IPEP-27%3A-Contents-Service.
;;;

;;

;;; Code:

(require 'ein-core)
(require 'ein-classes)
(require 'ein-utils)
(require 'ein-log)
(require 'ein-query)

(declare-function ein:notebook-to-json "ein-notebook")
(declare-function ein:notebooklist-url "ein-notebooklist")

(defcustom ein:content-query-max-depth 2
  "Don't recurse the directory tree deeper than this."
  :type 'integer
  :group 'ein)

(defcustom ein:content-query-max-branch 6
  "Don't descend into more than this number of directories per depth.
The total number of parallel queries should therefore be
O({max_branch}^{max_depth})."
  :type 'integer
  :group 'ein)

(make-obsolete-variable 'ein:content-query-timeout nil "0.17.0")

(defcustom ein:force-sync nil
  "When non-nil, force synchronous http requests."
  :type 'boolean
  :group 'ein)

(defun ein:content-query-contents (url-or-port path &optional callback errback iteration)
  "Register CALLBACK of arity 1 for the contents at PATH from the URL-OR-PORT.
ERRBACK of arity 1 for the contents."
  (setq callback (or callback #'ignore))
  (setq errback (or errback #'ignore))
  (setq iteration (or iteration 0))
  (ein:query-singleton-ajax
   (ein:notebooklist-url url-or-port path)
   :type "GET"
   :parser #'ein:json-read
   :complete (apply-partially #'ein:content-query-contents--complete url-or-port path)
   :success (apply-partially #'ein:content-query-contents--success url-or-port path callback)
   :error (apply-partially #'ein:content-query-contents--error url-or-port path callback errback iteration)))

(cl-defun ein:content-query-contents--complete
    (_url-or-port _path
     &key data _symbol-status response &allow-other-keys
     &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:query-contents--complete %s" resp-string))

(cl-defun ein:content-query-contents--error
    (url-or-port path callback errback iteration
     &key symbol-status response error-thrown data &allow-other-keys
     &aux
     (response-status (request-response-status-code response))
     (hub-p (request-response-header response "x-jupyterhub-version")))
  (cl-case response-status
    (404 (ein:log 'error "ein:content-query-contents--error %s %s"
                  response-status (plist-get data :message))
         (when errback (funcall errback url-or-port response-status)))
    (t (if (< iteration 3)
           (if (and hub-p data (eq response-status 405))
               (ein:content-query-contents--success url-or-port path callback :data data)
             (ein:log 'verbose "Retry content-query-contents #%s in response to %s"
                      iteration response-status)
             (sleep-for 0 (* (1+ iteration) 500))
             (ein:content-query-contents url-or-port path callback errback (1+ iteration)))
         (ein:log 'error "ein:content-query-contents--error %s REQUEST-STATUS %s DATA %s"
                  (concat (file-name-as-directory url-or-port) path)
                  symbol-status (cdr error-thrown))
         (when errback (funcall errback url-or-port response-status))))))

(cl-defun ein:content-query-contents--success
    (url-or-port path callback
     &key data _symbol-status _response &allow-other-keys)
  (when callback
    (funcall callback (ein:new-content url-or-port path data))))

(defun ein:content-to-json (content)
  (let ((path (if (>= (ein:$content-notebook-api-version content) 3)
                  (ein:$content-path content)
                (substring (ein:$content-path content)
                           0
                           (or (cl-position ?/ (ein:$content-path content) :from-end t)
                               0)))))
    (ignore-errors
      (ein:json-encode `((type . ,(ein:$content-type content))
                         (name . ,(ein:$content-name content))
                         (path . ,path)
                         (format . ,(or (ein:$content-format content) "json"))
                         (content ,@(ein:$content-raw-content content)))))))

(defun ein:content-from-notebook (nb)
  (let ((nb-content (ein:notebook-to-json nb)))
    (make-ein:$content :name (ein:$notebook-notebook-name nb)
                       :path (ein:$notebook-notebook-path nb)
                       :url-or-port (ein:$notebook-url-or-port nb)
                       :type "notebook"
                       :notebook-api-version (ein:$notebook-api-version nb)
                       :raw-content (append nb-content nil))))

;;; Managing/listing the content hierarchy

(defvar *ein:content-hierarchy* (make-hash-table :test #'equal)
  "Content tree keyed by URL-OR-PORT.")

(defun ein:content-need-hierarchy (url-or-port)
  "Callers assume ein:content-query-hierarchy succeeded.  If not, nil."
  (aif (gethash url-or-port *ein:content-hierarchy*) it
    (ein:log 'warn "No recorded content hierarchy for %s" url-or-port)
    nil))

(defun ein:new-content (url-or-port path data)
  ;; data is like (:size 72 :content nil :writable t :path Untitled7.ipynb :name Untitled7.ipynb :type notebook)
  (let ((content (make-ein:$content
                  :url-or-port url-or-port
                  :notebook-api-version (ein:notebook-api-version-numeric url-or-port)
                  :path path))
        (raw-content (if (vectorp (plist-get data :content))
                         (append (plist-get data :content) nil)
                       (plist-get data :content))))
    (setf (ein:$content-name content) (plist-get data :name)
          (ein:$content-path content) (plist-get data :path)
          (ein:$content-type content) (plist-get data :type)
          (ein:$content-created content) (plist-get data :created)
          (ein:$content-last-modified content) (plist-get data :last_modified)
          (ein:$content-format content) (plist-get data :format)
          (ein:$content-writable content) (plist-get data :writable)
          (ein:$content-mimetype content) (plist-get data :mimetype)
          (ein:$content-raw-content content) raw-content)
    content))

(defun ein:content-query-hierarchy* (url-or-port path callback sessions depth content)
  "Returns list (tree) of content objects.  CALLBACK accepts tree."
  (let* ((url-or-port url-or-port)
         (path path)
         (callback callback)
         (items (ein:$content-raw-content content))
         (directories (if (< depth ein:content-query-max-depth)
                          (cl-loop for item in items
                                   until (>= (length result) ein:content-query-max-branch)
                                   if (string= "directory" (plist-get item :type))
                                   collect (ein:new-content url-or-port path item)
                                   into result
                                   end
                                   finally return result)))
         (others (cl-loop for item in items
                          with c0
                          if (not (string= "directory" (plist-get item :type)))
                          do (setf c0 (ein:new-content url-or-port path item))
                          (setf (ein:$content-session-p c0)
                                (gethash (ein:$content-path c0) sessions))
                          and collect c0
                          end)))
    (deferred:$
      (apply
       #'deferred:parallel
       (cl-loop for c0 in directories
             collect
             (let ((c0 c0)
                   (d0 (deferred:new #'identity)))
               (ein:content-query-contents
                url-or-port
                (ein:$content-path c0)
                (apply-partially #'ein:content-query-hierarchy*
                                 url-or-port
                                 (ein:$content-path c0)
                                 (lambda (tree)
                                   (deferred:callback-post d0 (cons c0 tree)))
                                 sessions (1+ depth))
                (lambda (&rest _args) (deferred:callback-post d0 (cons c0 nil))))
               d0)))
      (deferred:nextc it
        (lambda (tree)
          (let ((result (append others tree)))
            (when (string= path "")
              (setf (gethash url-or-port *ein:content-hierarchy*) (-flatten result)))
            (funcall callback result)))))))

(defun ein:content-query-hierarchy (url-or-port &optional callback)
  "Get hierarchy of URL-OR-PORT with CALLBACK arity 1 for which hierarchy."
  (setq callback (or callback #'ignore))
  (ein:content-query-sessions
   url-or-port
   (apply-partially (lambda (url-or-port* callback* sessions)
                      (ein:content-query-contents url-or-port* ""
                       (apply-partially #'ein:content-query-hierarchy*
                                        url-or-port*
                                        ""
                                        callback* sessions 0)
                       (lambda (&rest _ignore)
                         (when callback* (funcall callback* nil)))))
                    url-or-port callback)
   callback))

;;; Save Content

(defsubst ein:content-url (content)
  (ein:notebooklist-url (ein:$content-url-or-port content)
                        (ein:$content-path content)))

(defun ein:content-save (content &optional callback cbargs errcb errcbargs)
  (ein:query-singleton-ajax
   (ein:content-url content)
   :type "PUT"
   :headers '(("Content-Type" . "application/json"))
   :data (encode-coding-string (ein:content-to-json content) buffer-file-coding-system)
   :success (apply-partially #'ein:content-save-success callback cbargs)
   :error (apply-partially #'ein:content-save-error
                           (ein:content-url content) errcb errcbargs)))

(cl-defun ein:content-save-success (callback cbargs &key _status _response &allow-other-keys)
  (when callback
    (apply callback cbargs)))

(cl-defun ein:content-save-error (url errcb errcbargs &key response &allow-other-keys)
  (ein:log 'error
    "ein:content-save-error: %s %s."
    url (error-message-string (request-response-error-thrown response)))
  (when errcb
    (apply errcb errcbargs)))

(defun ein:content-rename (content new-path &optional callback cbargs)
  (ein:query-singleton-ajax
   (ein:content-url content)
   :type "PATCH"
   :data (ein:json-encode `((path . ,new-path)))
   :parser #'ein:json-read
   :success (apply-partially #'update-content-path content callback cbargs)
   :error (apply-partially #'ein:content-rename-error (ein:$content-path content))))

(defun ein:session-rename (url-or-port session-id new-path)
  (ein:query-singleton-ajax
   (ein:url url-or-port "api/sessions" session-id)
   :type "PATCH"
   :data (ein:json-encode `((path . ,new-path)))
   :complete #'ein:session-rename--complete))

(cl-defun ein:session-rename--complete (&key data response _symbol-status &allow-other-keys
                                        &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:session-rename--complete %s" resp-string))

(cl-defun update-content-path (content callback cbargs &key data &allow-other-keys)
  (setf (ein:$content-path content) (plist-get data :path)
        (ein:$content-name content) (plist-get data :name)
        (ein:$content-last-modified content) (plist-get data :last_modified))
  (when callback
    (apply callback cbargs)))

(cl-defun ein:content-rename-error (path &key response data &allow-other-keys)
  (ein:log 'error
    "Renaming content %s failed %s %s."
    path (request-response-error-thrown response) (plist-get data :message)))

;;; Sessions

(defun ein:content-query-sessions (url-or-port &optional callback errback iteration)
  "Register CALLBACK of arity 1 to retrieve the sessions.
Call ERRBACK of arity 1 (contents) upon failure."
  (setq callback (or callback #'ignore))
  (setq errback (or errback #'ignore))
  (setq iteration (or iteration 0))
  (ein:query-singleton-ajax
   (ein:url url-or-port "api/sessions")
   :type "GET"
   :parser #'ein:json-read
   :complete (apply-partially #'ein:content-query-sessions--complete url-or-port callback)
   :success (apply-partially #'ein:content-query-sessions--success url-or-port callback)
   :error (apply-partially #'ein:content-query-sessions--error url-or-port callback errback iteration)))

(cl-defun ein:content-query-sessions--success (url-or-port callback &key data &allow-other-keys)
  (cl-flet ((read-name (nb-json)
                       (if (< (ein:notebook-api-version-numeric url-or-port) 3)
                           (if (string= (plist-get nb-json :path) "")
                               (plist-get nb-json :name)
                             (format "%s/%s" (plist-get nb-json :path) (plist-get nb-json :name)))
                         (plist-get nb-json :path))))
    (let ((session-hash (make-hash-table :test 'equal)))
      (dolist (s (append data nil) (funcall callback session-hash))
        (setf (gethash (read-name (plist-get s :notebook)) session-hash)
              (cons (plist-get s :id) (plist-get s :kernel)))))))

(cl-defun ein:content-query-sessions--error
    (url-or-port callback errback iteration
     &key data response error-thrown &allow-other-keys
     &aux
     (response-status (request-response-status-code response))
     (hub-p (request-response-header response "x-jupyterhub-version")))
  (if (< iteration 3)
      (if (and hub-p data (eq response-status 405))
          (ein:content-query-sessions--success url-or-port callback :data data)
        (ein:log 'verbose "Retry sessions #%s in response to %s %S" iteration response-status response)
        (sleep-for 0 (* (1+ iteration) 500))
        (ein:content-query-sessions url-or-port callback errback (1+ iteration)))
    (ein:log 'error "ein:content-query-sessions--error %s: ERROR %s DATA %s" url-or-port (car error-thrown) (cdr error-thrown))
    (when errback (funcall errback nil))))

(cl-defun ein:content-query-sessions--complete
    (_url-or-port _callback
     &key data response &allow-other-keys
     &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:query-sessions--complete %s" resp-string))

;;; Uploads


(defun ein:get-local-file (path)
  "Get contents of PATH.
Guess type of file (one of file, notebook, or directory)
and content format (one of json, text, or base64)."
  (unless (file-readable-p path)
    (error "File %s is not accessible and cannot be uploaded." path))
  (let ((name (file-name-nondirectory path))
        (type (file-name-extension path)))
    (with-temp-buffer
      (insert-file-contents path)
      (cond ((string= type "ipynb")
             (list name "notebook" "json" (buffer-string)))
            ((eql buffer-file-coding-system 'no-conversion)
             (list name "file" "base64" (buffer-string)))
            (t (list name "file" "text" (buffer-string)))))))

(provide 'ein-contents-api)
