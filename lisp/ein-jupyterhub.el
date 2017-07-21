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
  last_activity)

(defun ein:get-jh-conn (url)
  (gethash url *ein:jupyterhub-servers*))

(defun ein:reset-jh-servers ()
  (setq *ein:jupyterhub-servers* (make-hash-table :test #'equal)))

(defun ein:jupyterhub-prepare-headers (conn settings &optional securep)
  "When present, add the authorizations token to the header of a
request to Jupyterhub's REST API. Also perform the usual header
preparation ala `ein:query-prepare-header', i.e. added XSRF token
and configure cookies."
  (let ((_settings (ein:query-prepare-header (ein:$jh-conn-url conn) settings securep)))
    (if (boundp (ein:$jh-conn-token conn))
        (setq _settings (plist-put _settings :headers (append (plist-get _settings :headers)
                                                              (list (cons "Authorization" (ein:$jh-conn-token conn)))))))
    _settings))

(defun ein:jupyterhub-api-url (url-or-port command &rest args)
  (if args
      (ein:url url-or-port "hub/api" command args)
    (ein:url url-or-port "hub/api" command)))

(defun ein:jupyterhub-connect (url-or-port user password)
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
        (setf (gethash (ein:$jh-conn-url conn) *ein:jupyterhub-servers*) conn)))))

(defun ein:jupyterhub-get-user (conn username)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url (ein:$jh-conn-url conn) "users" username))
    (deferred:nextc it
      (lambda (response)
        (message "Response: %s" (request-response-data response))))))

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

(defun ein:hub-url (url-or-port &optional path)
  (if path
      (ein:url url-or-port "hub/api" path)
    (ein:url url-or-port "hub/api")))

(defun ein:jupyterhub-version (url-or-port &optional callback cbargs)
  (let ((url (ein:hub-url url-or-port)))
    (ein:query-singleton-ajax
     (list 'jupyterhub-version url-or-port)
     url
     :type "GET"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     :success (apply-partially #'ein:jupyterhub-version-success callback cbargs))))

(defun* ein:jupyterhub-version-success (callback cbargs &key data &allow-other-keys)
  (let ((version (plist-get data :version)))
    (when callback
      (apply called-interactively-p-functions data cbargs))))


(defun ein:jupyterhub-login (url-or-port username password &optional callback)
  (let ((url (ein:url url-or-port "hub/login")))
    (ein:query-singleton-ajax
     (list 'jupyterhub-login url-or-port)
     url
     :type "POST"
     :data (concat "username=" (url-hexify-string username)
                   "&password=" (url-hexify-string password))
     :timeout ein:content-query-timeout
     :success #'ein:jupyterhub-login-success
     :status-code '((302 . #'ein:jupyterhub-login-redirect)))))

(defun* ein:jupyterhub-login-redirect (&key data response &allow-other-keys)
  (message "Login redirect!"))

(defun* ein:jupyterhub-login-success (&key data response &allow-other-keys)
  response
  )

(defun ein:jupyterhub-services (url-or-port)
  (let ((url (ein:hub-url url-or-port "services")))
    (ein:query-singleton-ajax
     (list 'jupyterhub-version url-or-port)
     url
     :type "GET"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read
     )))


(provide 'ein-jupyterhub)
