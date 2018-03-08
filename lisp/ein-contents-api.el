;;; ein-contents-api.el --- Interface to Jupyter's Contents API

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

(require 'cl)
(require 'ein-core)
(require 'ein-classes)
(require 'ein-utils)
(require 'ein-log)
(require 'ein-query)

(provide 'ein-contents-api) ; must provide before requiring ein-notebook:
(require 'ein-notebook)     ; circular: depends on this file!

(defcustom ein:content-query-timeout (* 60 1000) ; 1 min
  "Query timeout for getting content from Jupyter/IPython notebook.
If you cannot open large notebooks because of a timeout error try
increasing this value.  Setting this value to `nil' means to use
global setting.  For global setting and more information, see
`ein:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defcustom ein:force-sync nil
  "If T, force ein to communicate with the IPython/Jupyter contents API synchronously. If not, use asynchronous communication. If you are seeing odd errors while using ein try setting this to T, though note that Emacs will likely be less responsive as it blocks while waiting for the IPython/Jupyter notebook server to respond"
  :type 'boolean
  :group 'ein)

(defun ein:content-url (content &rest params)
  (let ((url-or-port (ein:$content-url-or-port content))
        (path (ein:$content-path content)))
    (if params
        (url-encode-url (apply #'ein:url
                               url-or-port
                               "api/contents"
                               path
                               params))
      (url-encode-url (ein:url url-or-port "api/contents" path)))))

(defun ein:content-url-legacy (content &rest params)
  "Generate content url's for IPython Notebook version 2.x"
  (let ((url-or-port (ein:$content-url-or-port content))
        (path (ein:$content-path content)))
    (if params
        (url-encode-url (apply #'ein:url
                               url-or-port
                               "api/notebooks"
                               path
                               params))
      (url-encode-url (ein:url url-or-port "api/notebooks" path)))))

(defun ein:content-query-contents (path &optional url-or-port force-sync callback retry-p)
  "Return the contents of the object at the specified path from the Jupyter server."
  (condition-case err
      (let* ((url-or-port (or url-or-port (ein:default-url-or-port)))
             (new-content (make-ein:$content
                           :url-or-port url-or-port
                           :ipython-version (ein:query-ipython-version url-or-port)
                           :path path))
             (url (ein:content-url new-content)))
        (if (= 2 (ein:$content-ipython-version new-content))
            (setq new-content (ein:content-query-contents-legacy path url-or-port ein:force-sync callback))
          (ein:query-singleton-ajax
           (list 'content-query-contents url-or-port path)
           url
           :type "GET"
           :timeout ein:content-query-timeout
           :parser #'ein:json-read
           :sync (or force-sync ein:force-sync)
           :success (apply-partially #'ein:new-content new-content callback)
           :error (apply-partially #'ein:content-query-contents-error url retry-p
                                   (list path url-or-port force-sync callback t))))
        new-content)
    (error (progn (message "Error %s on query contents, try calling `ein:notebooklist-login` first..." err)
                  (if (>= ein:log-level (ein:log-level-name-to-int 'debug))
                      (throw 'error err))))))

(defun ein:content-query-contents-legacy (path &optional url-or-port force-sync callback)
  "Return contents of object at specified path for IPython Notebook versions 2.x"
  (let* ((url-or-port (or url-or-port (ein:default-url-or-port)))
         (new-content (make-ein:$content :url-or-port url-or-port
                                         :ipython-version (ein:query-ipython-version url-or-port)
                                         :path path))
         (url (ein:content-url-legacy new-content)))
    (ein:query-singleton-ajax
     (list 'content-query-contents-legacy url-or-port path)
     url
     :type "GET"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :sync ein:force-sync
     :success (apply-partially #'ein:query-contents-legacy-success path new-content callback)
     :error (apply-partially #'ein:content-query-contents-error url))
    new-content))

(defun ein:fix-legacy-content-data (data)
  (if (listp (car data))
      (loop for item in data
            collecting
            (ein:fix-legacy-content-data item))
    (if (string= (plist-get data :path) "")
        (plist-put data :path (plist-get data :name))
      (plist-put data :path (format "%s/%s" (plist-get data :path) (plist-get data :name))))))

(defun* ein:query-contents-legacy-success (path content callback &key data &allow-other-keys)
  (if (not (plist-get data :type))
      ;; Content API in 2.x a bit inconsistent.
      (progn
        (setf (ein:$content-name content) (substring path (or (cl-position ?/ path) 0))
              (ein:$content-path content) path
              (ein:$content-type content) "directory"
              ;;(ein:$content-created content) (plist-get data :created)
              ;;(ein:$content-last-modified content) (plist-get data :last_modified)
              (ein:$content-format content) nil
              (ein:$content-writable content) nil
              (ein:$content-mimetype content) nil
              (ein:$content-raw-content content) (ein:fix-legacy-content-data data))
        (when callback
          (funcall callback content))
        content)
    (ein:new-content content callback :data data)))

;; TODO: This is one place to check for redirects - update the url slot if so.
;; Will need to pass the response object and check either request-response-history
;; or request-response-url.
(defun* ein:new-content (content callback &key data response &allow-other-keys)
  (setf (ein:$content-name content) (plist-get data :name)
        (ein:$content-path content) (plist-get data :path)
        (ein:$content-type content) (plist-get data :type)
        (ein:$content-created content) (plist-get data :created)
        (ein:$content-last-modified content) (plist-get data :last_modified)
        (ein:$content-format content) (plist-get data :format)
        (ein:$content-writable content) (plist-get data :writable)
        (ein:$content-mimetype content) (plist-get data :mimetype)
        (ein:$content-raw-content content) (plist-get data :content))
  (ein:aif response
      (setf (ein:$content-url-or-port content) (ein:get-response-redirect it)))
  ;; (if (length (request-response-history response))
  ;;     (let ((url (url-generic-parse-url (format "%s" (request-response-url response)))))
  ;;       (setf (ein:$content-url-or-port content) (format "%s://%s:%s"
  ;;                                                        (url-type url)
  ;;                                                        (url-host url)
  ;;                                                        (url-port url)))))
  (when callback
    (funcall callback content))
  content)

(defun ein:content-to-json (content)
  (let ((path (if (>= (ein:$content-ipython-version content) 3)
                  (ein:$content-path content)
                (substring (ein:$content-path content)
                           0
                           (or (cl-position ?/ (ein:$content-path content) :from-end t)
                               0)))))
    (json-encode `((:type . ,(ein:$content-type content))
                   (:name . ,(ein:$content-name content))
                   (:path . ,path)
                   (:format . ,(or (ein:$content-format content) "json"))
                   (:content ,@(ein:$content-raw-content content))))))

(defun ein:content-from-notebook (nb)
  (let ((nb-content (ein:notebook-to-json nb)))
    (make-ein:$content :name (ein:$notebook-notebook-name nb)
                       :path (ein:$notebook-notebook-path nb)
                       :url-or-port (ein:$notebook-url-or-port nb)
                       :type "notebook"
                       :ipython-version (ein:$notebook-api-version nb)
                       :raw-content nb-content)))


(defun* ein:content-query-contents-error (url retry-p packed &key symbol-status response &allow-other-keys)
  (ein:gc-complete-operation)
  (if (and (eql symbol-status 'parse-error)
           (not retry-p))
      (progn
        (message "Content list call failed, maybe because curl hasn't updated it's cookie jar yet? Let's try one more time....")
        (apply #'ein:content-query-contents packed))
    (progn
      (ein:log 'verbose
        "Error thrown: %S" (request-response-error-thrown response))
      (ein:log 'error
        "Content list call %s failed with status %s." url symbol-status))))


;;; Managing/listing the content hierarchy

(defvar *ein:content-hierarchy* (make-hash-table :test #'equal))

(defun ein:get-content-hierarchy (url-or-port)
  (or (gethash url-or-port *ein:content-hierarchy*)
      (ein:refresh-content-hierarchy url-or-port)))

(defun ein:make-content-hierarchy (path url-or-port)
  (let* ((node (ein:content-query-contents path url-or-port t))
         (active-sessions (make-hash-table :test 'equal))
         (items (ein:$content-raw-content node)))
    (ein:content-query-sessions active-sessions url-or-port t)
    (ein:flatten (loop for item in items
                       for c = (make-ein:$content :url-or-port url-or-port)
                   do (ein:new-content c nil :data item)
                   collect
                   (cond ((string= (ein:$content-type c) "directory")
                          (cons c
                                (ein:make-content-hierarchy (ein:$content-path c) url-or-port)))
                         (t (progn
                              (setf (ein:$content-session-p c)
                                    (gethash (ein:$content-path c) active-sessions))
                              c)))))))

(defun ein:refresh-content-hierarchy (&optional url-or-port)
  (let ((url-or-port (or url-or-port (ein:default-url-or-port))))
    (setf (gethash url-or-port *ein:content-hierarchy*)
          (ein:make-content-hierarchy "" url-or-port))))


;;; Save Content
(defun ein:content-save-legacy (content &optional callback cbargs errcb errcbargs)
  (ein:query-singleton-ajax
       (list 'content-save (ein:$content-url-or-port content) (ein:$content-path content))
       (ein:content-url-legacy content)
       :type "PUT"
       :headers '(("Content-Type" . "application/json"))
       :timeout ein:content-query-timeout
       :data (ein:content-to-json content)
       :success (apply-partially #'ein:content-save-success callback cbargs)
       :error (apply-partially #'ein:content-save-error (ein:content-url-legacy content) errcb errcbargs)))

(defun ein:content-save (content &optional callback cbargs errcb errcbargs)
  (if (>= (ein:$content-ipython-version content) 3)
      (ein:query-singleton-ajax
       (list 'content-save (ein:$content-url-or-port content) (ein:$content-path content))
       (ein:content-url content)
       :type "PUT"
       :headers '(("Content-Type" . "application/json"))
       :timeout ein:content-query-timeout
       :data (encode-coding-string (ein:content-to-json content) buffer-file-coding-system)
       :success (apply-partially #'ein:content-save-success callback cbargs)
       :error (apply-partially #'ein:content-save-error (ein:content-url content) errcb errcbargs))
    (ein:content-save-legacy content callback cbargs)))

(defun* ein:content-save-success (callback cbargs &key status response &allow-other-keys)
  ;;(ein:log 'verbose "Saving content successful with status %s" status)
  (when callback
    (apply callback cbargs)))

(defun* ein:content-save-error (url errcb errcbargs &key response status-code &allow-other-keys)
  (ein:log 'error
    "Error thrown: %S" (request-response-error-thrown response))
  (ein:log 'error
    "Content save call %s failed with status %s." url status-code)
  (when errcb
    (apply errcb errcbargs)))


;;; Rename Content

(defun ein:content-legacy-rename (content new-path callback cbargs)
  (let ((path (substring new-path 0 (or (position ?/ new-path :from-end t) 0)))
        (name (substring new-path (or (position ?/ new-path :from-end t) 0))))
    (ein:query-singleton-ajax
     (list 'content-rename (ein:$content-url-or-port content) (ein:$content-path content))
     (ein:content-url-legacy content)
     :type "PATCH"
     :data (json-encode `((name . ,name)
                          (path . ,path)))
     :parser #'ein:json-read
     :success (apply-partially #'update-content-path-legacy content callback cbargs)
     :error (apply-partially #'ein:content-rename-error new-path))))

(defun* update-content-path-legacy (content callback cbargs &key data &allow-other-keys)
  (setf (ein:$content-path content) (ein:trim-left (format "%s/%s" (plist-get data :path) (plist-get data :name))
                                                   "/")
        (ein:$content-name content) (plist-get data :name)
        (ein:$content-last-modified content) (plist-get data :last_modified))
  (when callback
    (apply callback cbargs)))

(defun ein:content-rename (content new-path &optional callback cbargs)
  (if (>= (ein:$content-ipython-version content) 3)
      (ein:query-singleton-ajax
       (list 'content-rename (ein:$content-url-or-port content) (ein:$content-path content))
       (ein:content-url content)
       :type "PATCH"
       :data (json-encode `((path . ,new-path)))
       :parser #'ein:json-read
       :success (apply-partially #'update-content-path content callback cbargs)
       :error (apply-partially #'ein:content-rename-error (ein:$content-path content)))
    (ein:content-legacy-rename content new-path callback cbargs)))

(defun* update-content-path (content callback cbargs &key data &allow-other-keys)
  (setf (ein:$content-path content) (plist-get data :path)
        (ein:$content-name content) (plist-get data :name)
        (ein:$content-last-modified content) (plist-get data :last_modified))
  (when callback
    (apply callback cbargs)))

(defun* ein:content-rename-error (path &key symbol-status response &allow-other-keys)
  (ein:log 'verbose
    "Error thrown: %S" (request-response-error-thrown response))
  (ein:log 'error
    "Renaming content %s failed with status %s." path symbol-status))



;;; Sessions

(defun ein:content-query-sessions (session-hash url-or-port &optional force-sync)
  (unless force-sync
    (setq force-sync ein:force-sync))
  (ein:query-singleton-ajax
     (list 'content-query-sessions)
     (ein:url url-or-port "api/sessions")
     :type "GET"
     :parser #'ein:json-read
     :success (apply-partially #'ein:content-query-sessions-success session-hash url-or-port)
     :error #'ein:content-query-sessions-error
     :sync force-sync))

(defun* ein:content-query-sessions-success (session-hash url-or-port &key data &allow-other-keys)
  (cl-flet ((read-name (nb-json)
                       (if (= (ein:query-ipython-version url-or-port) 2)
                           (if (string= (plist-get nb-json :path) "")
                               (plist-get nb-json :name)
                             (format "%s/%s" (plist-get nb-json :path) (plist-get nb-json :name)))
                         (plist-get nb-json :path))))
    (dolist (s data)
      (setf (gethash (read-name (plist-get s :notebook)) session-hash)
            (cons (plist-get s :id) (plist-get s :kernel))))
    session-hash))

(defun* ein:content-query-sessions-error (&key symbol-status response &allow-other-keys)
  (ein:log 'error "Session query failed with status %s (%s)." symbol-status response))


;;; Checkpoints

(defun ein:content-query-checkpoints (content &optional callback cbargs)
  (let* ((url (ein:content-url content "checkpoints")))
    (ein:query-singleton-ajax
     (list 'content-query-checkpoints url)
     url
     :type "GET"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :sync ein:force-sync
     :success (apply-partially #'ein:content-query-checkpoints-success content callback cbargs)
     :error (apply-partially #'ein:content-query-checkpoints-error content))))

(defun ein:content-create-checkpoint (content &optional callback cbargs)
  (let* ((url (ein:content-url content "checkpoints")))
    (ein:query-singleton-ajax
     (list 'content-query-checkpoints url)
     url
     :type "POST"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :sync ein:force-sync
     :success (apply-partially #'ein:content-query-checkpoints-success content callback cbargs)
     :error (apply-partially #'ein:content-query-checkpoints-error content))))

(defun ein:content-restore-checkpoint (content checkpoint-id &optional callback cbargs)
  (let* ((url (ein:content-url content "checkpoints" checkpoint-id)))
    (ein:query-singleton-ajax
     (list 'content-query-checkpoints url)
     url
     :type "POST"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :sync ein:force-sync
     :success (when callback
                (apply callback cbargs))
     :error (apply-partially #'ein:content-query-checkpoints-error content))))

(defun ein:content-delete-checkpoint (content checkpoint-id &optional callback cbargs)
  (let* ((url (ein:content-url content "checkpoints" checkpoint-id)))
    (ein:query-singleton-ajax
     (list 'content-query-checkpoints url)
     url
     :type "DELETE"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :sync ein:force-sync
     :success (when callback
                (apply callback cbargs))
     :error (apply-partially #'ein:content-query-checkpoints-error content))))

(defun* ein:content-query-checkpoints-success (content cb cbargs &key data status response &allow-other-keys)
  (unless (listp (car data))
    (setq data (list data)))
  (setf (ein:$content-checkpoints content) data)
  (when cb
    (apply cb content cbargs)))

(defun* ein:content-query-checkpoints-error (content &key symbol-status response &allow-other-keys)
  (ein:log 'error "Content checkpoint operation failed with status %s (%s)." symbol-status response))


;;; Uploads

(defun ein:get-local-file (path)
  "If path exists, get contents and try to guess type of file (one of file, notebook, or directory)
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


(defun ein:content-upload (path uploaded-file-path &optional url-or-port)
  (multiple-value-bind (name type format contents) (ein:get-local-file uploaded-file-path)
    (let* ((content (make-ein:$content :url-or-port (or url-or-port (ein:default-url-or-port))
                                       :name name
                                       :path (concat path "/" name)
                                       :raw-content contents))
           (data (make-hash-table)))
      (setf (gethash 'path data) path
            (gethash 'name data) name
            (gethash 'type data) type
            (gethash 'format data) format
            (gethash 'content data) contents)
      (ein:query-singleton-ajax
       (list 'content-upload name)
       (ein:content-url content)
       :type "PUT"
       :headers '(("Content-Type" . "application/json"))
       :timeout ein:content-query-timeout
       :data (json-encode data)
       :success (lexical-let ((uploaded-file-path uploaded-file-path))
                  #'(lambda (&rest -ignore-) (message "File %s succesfully uploaded." uploaded-file-path)))
       :error (apply-partially #'ein:content-upload-error uploaded-file-path)))))

(cl-defun ein:content-upload-error (path &key symbol-status response &allow-other-keys)
  (ein:display-warning (format "Could not upload %s. Failed with status %s" path symbol-status)))
