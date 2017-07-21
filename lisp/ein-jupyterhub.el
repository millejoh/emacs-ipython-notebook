;;; ein-jupyterhub.el --- Interface to Jupyterhub -*- mode: emacs-lisp; fill-column: 80; lexical-binding: t -*-

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
;;;
;;; An interface to the Jupyterhub login and management API as described in
;;; http://jupyterhub.readthedocs.io/en/latest/api/index.html
;;;

;;

;;; Code:
(require 'request-deferred)
(require 'deferred)
(require 'ein-query)

(defvar *ein:jupyterhub-servers* (make-hash-table :test #'equal))

(defstruct ein:$jh-conn
  "Data representing a connection to a jupyterhub server."
  url
  version
  token)

(defstruct ein:$jh-user
  "A jupyterhub user, per https://jupyterhub.readthedocs.io/en/latest/_static/rest-api/index.html#/definitions/User."
  name
  admin
  groups
  server
  pending
  last-activity)

(defun ein:get-jh-conn (url)
  (gethash url *ein:jupyterhub-servers*))

(defun ein:reset-jh-servers ()
  (setq *ein:jupyterhub-servers* (make-hash-table :test #'equal)))

(defun ein:jupyterhub-prepare-headers (conn settings &optional securep)
  "When present, add the authorizations token to the header of a
request to Jupyterhub's REST API. Also perform the usual header
preparation ala `ein:query-prepare-header', i.e. added XSRF token
and configure cookies."
  (let ((s (ein:query-prepare-header (ein:$jh-conn-url conn) settings securep)))
    (if (ein:$jh-conn-token conn)
        (setq s (plist-put s :headers (append (plist-get s :headers)
                                              (list (cons "Authorization"
                                                          (format "token %s"
                                                                  (ein:$jh-conn-token conn))))))))
    s))

(defun ein:jupyterhub-api-url (url-or-port command &rest args)
  (if args
      (apply #'ein:url url-or-port "hub/api" command args)
    (ein:url url-or-port "hub/api" command)))

(defun* ein:jupyterhub-query (conn url &rest settings)
  ""
  (apply #'request-deferred
         (url-encode-url (ein:jupyterhub-api-url (ein:$jh-conn-url conn) url))
         (ein:jupyterhub-prepare-headers conn settings)))

(defun ein:jupyterhub--do-connect (url-or-port user password)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url url-or-port "/")
     :type "GET"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (when (and response (request-response-data response))
          (let ((conn (make-ein:$jh-conn :url (or (ein:get-response-redirect response)
                                                  url-or-port)
                                         :version (plist-get (request-response-data response) :version))))
            conn))))
    (deferred:nextc it
      (lambda (conn)
        (unless conn
          (error "Connection to Jupyterhub server at %s failed! Maybe you used the wrong URL?" url-or-port))
        (ein:jupyterhub-token-request conn user password)))
    (deferred:nextc it
      (lambda (conn)
        (setf (gethash (ein:$jh-conn-url conn) *ein:jupyterhub-servers*) conn)
        (ein:jupyterhub-start-server conn user)))))

(defun ein:jupyterhub-token-request (conn username password)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url (ein:$jh-conn-url conn) "authorizations/token")
     :type "POST"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :data (json-encode`(("username" . ,username)
                         ("password" . , password))))
    (deferred:nextc it
      (lambda (response)
        (message "response-data: %s, %s"
                 (request-response-data response)
                 (cadr (request-response-data response))) ;; FIXME: Why doesn't plist-get work?
        (setf (ein:$jh-conn-token conn) (cadr (request-response-data response)))
        conn))))

(defun ein:jupyterhub-get-user (conn username)
  (deferred:$
    (ein:jupyterhub-query
     conn
     (ein:url "users" username)
     :type "GET"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (let ((data (request-response-data response)))
          (message "Response: %s" data)
          (make-ein:$jh-user :name (plist-get :name data)
                             :admin (plist-get :admin data)
                             :groups (plist-get :groups data)
                             :server (plist-get :server data)
                             :pending (plist-get :pending data)
                             :last-activity (plist-get :last_activity data)))))))

(defun ein:jupyterhub-start-server (conn username)
  (deferred:$
    (ein:jupyterhub-query
     conn
     (ein:url "users" username "server")
     :type "POST"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (if (eql 201 (request-response-status-code response))
            (ein:jupyterhub-get-user conn username)
          (message "Response status: %s" (request-response-status-code response))
          )))
    (deferred:nextc it
      (lambda (user)
        (when (ein:$jh-user-p user)
          (let ((serverurl (ein:url (ein:$jh-conn-url conn) (ein:$jh-user-server user))))
            (ein:notebooklist-open serverurl)))))))

(defun ein:jupyterhub-list-users (conn)
  (deferred:$
    (ein:jupyterhub-query
     conn
     "users"
     :type "GET"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (message "Response: %s" (request-response-data response))
        response))))

(defun ein:jupyterhub-connect (url user password)
  (interactive (list (ein:notebooklist-ask-url-or-port)
                     (read-string "User: ")
                     (read-passwd "Password: ")))
  (ein:jupyterhub--do-connect url user password))

(provide 'ein-jupyterhub)
