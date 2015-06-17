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
(require 'ein-utils)

(defcustom ein:content-query-timeout (* 60 1000) ; 1 min
  "Query timeout for getting content from Jupyter/IPython notebook.
If you cannot open large notebooks because of a timeout error try
increasing this value.  Setting this value to `nil' means to use
global setting.  For global setting and more information, see
`ein:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)


(defstruct ein:$content
  "Content returned from the Jupyter notebook server:
`ein:$content-url-or-port'
  URL or port of Jupyter server.

`ein:$content-name 
  The name/filename of the content. Always equivalent to the last 
  part of the path field

`ein:$content-path
 The full file path. It will not start with /, and it will be /-delimited.

`ein:$content-type
 One of three values: :directory, :file, :notebook.

`ein:$content-writable
  Indicates if requester has permission to modified the requested content.

`ein:$content-created

`ein:$content-last-modified

`ein:$content-mimetype
  Specify the mime-type of :file content, null otherwise.

`ein:$content-raw-content
  Contents of resource as returned by Jupyter.  Depending on content-type will hold:
    :directory : JSON list of models for each item in the directory.
    :file      : Text of file as a string or base64 encoded string if mimetype
                 is other than 'text/plain'.
    :notebook  : JSON structure of the file.

`ein:$content-format
  Value will depend on content-type:
    :directory : :json.
    :file      : Either :text or :base64
    :notebook  : :json.
"
  url-or-port
  ipython-version
  name
  path
  type
  writable
  created
  last-modified
  mimetype
  raw-content
  format
  session-p)

(defun ein:content-url (content)
  (let ((url-or-port (ein:$content-url-or-port content))
        (path (ein:$content-path content)))
    (ein:url url-or-port "api/contents" (url-hexify-string path))))

(defun ein:content-url-legacy (content)
  "Generate content url's for IPython Notebook version 2.x"
  (let ((url-or-port (ein:$content-url-or-port content))
        (path  (ein:$content-path content)))
    (ein:url url-or-port "api/notebooks" (url-hexify-string path))))

(defun ein:content-query-contents (path &optional url-or-port force-sync callback)
  "Return the contents of the object at the specified path from the Jupyter server."
  (let* ((url-or-port (or url-or-port (ein:default-url-or-port)))
         (new-content (make-ein:$content
                       :url-or-port url-or-port
                       :ipython-version (ein:query-ipython-version url-or-port)
                       :path path))
         (url (ein:content-url new-content)))
    (if (= 2 (ein:$content-ipython-version new-content))
        (setq new-content (ein:content-query-contents-legacy path url-or-port force-sync callback))
      (ein:query-singleton-ajax
       (list 'content-query-contents url-or-port path)
       url
       :type "GET"
       :timeout ein:content-query-timeout
       :parser #'ein:json-read
       :sync force-sync
       :success (apply-partially #'ein:new-content new-content callback)
       :error (apply-partially #'ein:content-query-contents-error url)))
    new-content))

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
     :sync force-sync
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

(defun* ein:new-content (content callback &key data &allow-other-keys)
  (setf (ein:$content-name content) (plist-get data :name)
        (ein:$content-path content) (plist-get data :path)
        (ein:$content-type content) (plist-get data :type)
        (ein:$content-created content) (plist-get data :created)
        (ein:$content-last-modified content) (plist-get data :last_modified)
        (ein:$content-format content) (plist-get data :format)
        (ein:$content-writable content) (plist-get data :writable)
        (ein:$content-mimetype content) (plist-get data :mimetype)
        (ein:$content-raw-content content) (plist-get data :content))
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
                   (:format . "json")
                   (:content ,@(ein:$content-raw-content content))))))

(defun ein:content-from-notebook (nb)
  (let ((nb-content (ein:notebook-to-json nb)))
    (make-ein:$content :name (ein:$notebook-notebook-name nb)
                       :path (ein:$notebook-notebook-path nb)
                       :url-or-port (ein:$notebook-url-or-port nb)
                       :type "notebook"
                       :ipython-version (ein:$notebook-api-version nb)
                       :raw-content nb-content)))


(defun* ein:content-query-contents-error (url &key symbol-status response &allow-other-keys)
  (ein:log 'verbose
    "Error thrown: %S" (request-response-error-thrown response))
  (ein:log 'error
    "Content list call %s failed with status %s." url symbol-status))

;; ***

(defvar *ein:content-hierarchy* (make-hash-table))

(defun ein:make-content-hierarchy (path url-or-port)
  (let* ((node (ein:content-query-contents path url-or-port t))
         (active-sessions (make-hash-table :test 'equal))
         (items (ein:$content-raw-content node)))
    (ein:content-query-sessions url-or-port active-sessions t)
    (ein:flatten (loop for item in items
                       for c = (make-ein:$content :url-or-port url-or-port)
                   do (ein:new-content c nil :data item)
                   collect
                   (cond ((string= (ein:$content-type c) "directory")
                          (cons c
                                (ein:make-content-hierarchy (ein:$content-path c) url-or-port)))
                         (t (progv c
                                (setf (ein:$content-session-p c)
                                      (gethash active-sessions (ein:$content-path c))))))))))

(defun ein:refresh-content-hierarchy (&optional url-or-port)
  (let ((url-or-port (or url-or-port (ein:default-url-or-port))))
    (setf (gethash url-or-port *ein:content-hierarchy*)
          (ein:make-content-hierarchy "" url-or-port))))

;;; Save Content
(defun ein:content-save-legacy (content &optional callback cbargs)
  (ein:query-singleton-ajax
       (list 'content-save (ein:$content-url-or-port content) (ein:$content-path content))
       (ein:content-url-legacy content)
       :type "PUT"
       :headers '(("Content-Type" . "application/json"))
       :timeout ein:content-query-timeout
       :data (ein:content-to-json content)
       :success (apply-partially #'ein:content-save-success callback cbargs)
       :error (apply-partially #'ein:content-save-error (ein:content-url-legacy content))))

(defun ein:content-save (content &optional callback cbargs)
  (if (>= (ein:$content-ipython-version content) 3)
      (ein:query-singleton-ajax
       (list 'content-save (ein:$content-url-or-port content) (ein:$content-path content))
       (ein:content-url content)
       :type "PUT"
       :headers '(("Content-Type" . "application/json"))
       :timeout ein:content-query-timeout
       :data (ein:content-to-json content)
       :success (apply-partially #'ein:content-save-success callback cbargs)
       :error (apply-partially #'ein:content-save-error (ein:content-url content)))
    (ein:content-save-legacy content callback cbargs)))

(defun* ein:content-save-success (callback cbargs &key status response &allow-other-keys)
  (ein:log 'info "Saving content successful with status %s" status)
  (when callback
    (apply callback cbargs)))

(defun* ein:content-save-error (url &key symbol-status response &allow-other-keys)
  (ein:log 'verbose
    "Error thrown: %S" (request-response-error-thrown response))
  (ein:log 'error
    "Content list call %s failed with status %s." url symbol-status))


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
  (ein:query-singleton-ajax
     (list 'content-queyr-sessions)
     (ein:url url-or-port "api/sessions")
     :type "GET"
     :parser #'ein:json-read
     :success (apply-partially#'ein:content-query-sessions-success session-hash url-or-port)
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

(provide 'ein-contents-api)
