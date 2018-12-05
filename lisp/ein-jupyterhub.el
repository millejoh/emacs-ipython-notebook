;;; ein-jupyterhub.el --- Interface to Jupyterhub -*- lexical-binding: t -*-

;; Copyright (C) 2016 - John Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-jupyterhub.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-jupyterhub.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-jupyter.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;;
;;; An interface to the Jupyterhub login and management API as described in
;;; http://jupyterhub.readthedocs.io/en/latest/api/index.html
;;;

;;

;;; Code:
(require 'ein-query)
(require 'ein-websocket)
(require 'ein-notebooklist)

(defvar *ein:jupyterhub-connections* (make-hash-table :test #'equal))

(defstruct ein:$jh-conn
  "Data representing a connection to a jupyterhub server."
  url-or-port
  version
  user
  token)

(defstruct ein:$jh-user
  "A jupyterhub user, per https://jupyterhub.readthedocs.io/en/latest/_static/rest-api/index.html#/definitions/User"
  name
  admin
  groups
  server
  pending
  last-activity)

(defsubst ein:jupyterhub-user-path (url-or-port &rest paths)
  "Goes from URL-OR-PORT/PATHS to URL-OR-PORT/user/someone/PATHS"
  (let ((user-base (ein:aif (gethash url-or-port *ein:jupyterhub-connections*)
                       (ein:$jh-user-server (ein:$jh-conn-user it)))))
    (apply #'ein:url url-or-port user-base paths)))

(defsubst ein:jupyterhub-api-path (url-or-port &rest paths)
  (apply #'ein:url url-or-port "hub/api" paths))

(defun ein:jupyterhub--store-cookies (conn)
  "Websockets use the url-cookie API"
  (let* ((url-or-port (ein:$jh-conn-url-or-port conn))
         (parsed-url (url-generic-parse-url url-or-port))
         (host-port (if (url-port-if-non-default parsed-url)
                        (format "%s:%s" (url-host parsed-url) (url-port parsed-url))
                      (url-host parsed-url)))
         (securep (string= (url-type parsed-url) "https"))
         (cookies (append
                   (request-cookie-alist (url-host parsed-url) "/hub/" securep)
                   (ein:aand (ein:$jh-conn-user conn) (ein:$jh-user-server it)
                             (request-cookie-alist (url-host parsed-url) it securep)))))
    (dolist (c cookies)
      (ein:websocket-store-cookie c host-port
                                  (car (url-path-and-query parsed-url)) securep))))

(defun* ein:jupyterhub--login-complete (dobj conn &key response &allow-other-keys)
  (deferred:callback-post dobj (list conn response)))

(defmacro ein:jupyterhub--add-header (header)
  `(setq my-settings
         (plist-put my-settings :headers
                    (append (plist-get my-settings :headers) (list ,header)))))

(defmacro ein:jupyterhub-query (conn-key url cb cbargs &rest settings)
  `(let ((my-settings (list ,@settings)))
     (ein:and-let* ((conn (gethash ,conn-key *ein:jupyterhub-connections*)))
       (ein:jupyterhub--add-header
        (cons "Referer" (ein:url (ein:$jh-conn-url-or-port conn) "hub/login")))
       (ein:aif (ein:$jh-conn-token conn)
           (ein:jupyterhub--add-header
            (cons "Authorization" (format "token %s" it)))))
     (apply #'ein:query-singleton-ajax
            ,url ,url
            :error
            (lambda (&rest args)
              (ein:log 'error "ein:jupyterhub-query--error (%s) %s (%s)" ,url
                       (request-response-status-code (plist-get args :response))
                       (plist-get args :symbol-status)))
            :complete
            (lambda (&rest args)
              (ein:log 'debug "ein:jupyterhub-query--complete (%s) %s (%s)" ,url
                       (request-response-status-code (plist-get args :response))
                       (plist-get args :symbol-status)))
            :success
            (lambda (&rest args)
              (apply ,cb (request-response-data (plist-get args :response)) ,cbargs))
            my-settings)))

(defun ein:jupyterhub--receive-version (data url-or-port callback username password)
  (let ((conn (make-ein:$jh-conn
               :url-or-port url-or-port
               :version (plist-get data :version))))
    (setf (gethash url-or-port *ein:jupyterhub-connections*) conn)
    (ein:jupyterhub--query-login callback username password conn)))

(defun ein:jupyterhub--receive-user (data callback username password conn iteration)
  (let ((user (make-ein:$jh-user :name (plist-get data :name)
                                 :admin (plist-get data :admin)
                                 :groups (plist-get data :groups)
                                 :server (plist-get data :server)
                                 :pending (plist-get data :pending)
                                 :last-activity (plist-get data :last_activity))))
    (setf (ein:$jh-conn-user conn) user)
    (ein:jupyterhub--store-cookies conn)
    (if (not (ein:$jh-user-server user))
        (if (<= iteration 0)
            (ein:jupyterhub--query-token callback username password conn)
          (ein:display-warning "jupyterhub cannot start single-user server" :error))
      (ein:notebooklist-open*
       (ein:jupyterhub-user-path (ein:$jh-conn-url-or-port conn))
       nil nil callback))))

(defun ein:jupyterhub--receive-login (_data callback username password conn)
  (ein:jupyterhub--store-cookies conn)
  (ein:jupyterhub--query-user callback username password conn 0))

(defun ein:jupyterhub--receive-token (data callback username password conn)
  (setf (ein:$jh-conn-token conn) (plist-get data :token))
  (ein:jupyterhub--query-server callback username password conn))

(defun ein:jupyterhub--receive-server (_data callback username password conn)
  (ein:jupyterhub--query-user callback username password conn 1))

(defun ein:jupyterhub--query-token (callback username password conn)
  (ein:jupyterhub-query
   (ein:$jh-conn-url-or-port conn)
   (ein:jupyterhub-api-path (ein:$jh-conn-url-or-port conn)
                            "authorizations/token")
   #'ein:jupyterhub--receive-token
   `(,callback ,username ,password ,conn)
   :type "POST"
   :data (json-encode `((:username . ,username)
                        (:password . ,password)))
   :parser #'ein:json-read))

(defsubst ein:jupyterhub--query-server (callback username password conn)
  (ein:jupyterhub-query
   (ein:$jh-conn-url-or-port conn)
   (ein:jupyterhub-api-path (ein:$jh-conn-url-or-port conn)
                            "users" username "server")
   #'ein:jupyterhub--receive-server
   `(,callback ,username ,password ,conn)
   :type "POST"
   :parser #'ein:json-read))

(defsubst ein:jupyterhub--query-user (callback username password conn iteration)
  (ein:jupyterhub-query
   (ein:$jh-conn-url-or-port conn)
   (ein:jupyterhub-api-path (ein:$jh-conn-url-or-port conn) "users" username)
   #'ein:jupyterhub--receive-user
   `(,callback ,username ,password ,conn ,iteration)
   :type "GET"
   :parser #'ein:json-read))

(defsubst ein:jupyterhub--query-login (callback username password conn)
  (ein:jupyterhub-query
   (ein:$jh-conn-url-or-port conn)
   (ein:url (ein:$jh-conn-url-or-port conn) "hub/login")
   #'ein:jupyterhub--receive-login
   `(,callback ,username ,password ,conn)
   ;; :type "POST" ;; no type here else redirect will use POST
   :parser #'ignore
   :data `(("username" . ,username)
           ("password" . ,password))))

(defsubst ein:jupyterhub--query-version (url-or-port callback username password)
  (ein:jupyterhub-query
   url-or-port
   (ein:jupyterhub-api-path url-or-port)
   #'ein:jupyterhub--receive-version
   `(,url-or-port ,callback ,username ,password)
   :type "GET"
   :parser #'ein:json-read))

;;;###autoload
(defun ein:jupyterhub-connect (url-or-port username password callback)
  "Log on to a jupyterhub server using PAM authentication. Requires jupyterhub version 0.8 or greater.  CALLBACK takes two arguments, the resulting buffer and the singleuser url-or-port"
  (interactive (let ((url-or-port (ein:notebooklist-ask-url-or-port))
                     (pam-plist (ein:notebooklist-ask-user-pw-pair "User" "Password")))
                 (loop for (user pw) on pam-plist by (function cddr)
                       return (list url-or-port (symbol-name user) pw (lambda (buffer _url-or-port) (pop-to-buffer buffer))))))
  (ein:jupyterhub--query-version url-or-port callback username password))

(provide 'ein-jupyterhub)
