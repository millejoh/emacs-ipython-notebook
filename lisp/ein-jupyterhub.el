;; -*- mode: lisp; fill-column: 80; lexical-binding: t -*-
;;; ein-jupyterhub.el --- Interface to Jupyterhub

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
(require 'request)
(require 'deferred)

(defun ein:jupyterhub-authorization-url (url-or-port command)
  (ein:url url-or-port "authorizations" command))

(defun ein:jupyterhub-api-url (url-or-port command)
  (ein:url url-or-port "hub/api" command))

(defvar *ein:jupyterhub-version* nil)

(defun ein:jupyterhub-version (url-or-port)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url url-or-port "/")
     :type "GET"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (when (and response (request-response-data response))
          (setq *ein:jupyterhub-version* (plist-get (request-response-data response) :version)))))))

(defun ein:jupyterhub-get-user-from-token (url-or-port token)
  (deferred:$
    (ein:query-deferred
     (ein:url url-or-port "hub/api/authorizations/token" token))
    (deferred:nextc it
      (lambda (response)
        (message "Response: %s" (request-response-data response))))))

(defun ein:jupyterhub-token-request (url-or-port username password)
  (let ((url (ein:url url-or-port "hub/api/authorizations/token")))
    (deferred:$
      (deferred:try
        (deferred:$
          (ein:query-deferred
           url
           :type "POST"
           :timeout ein:content-query-timeout
           :parser #'ein:json-read
           :data (json-encode`(("username" . ,username)
                               ("password" . , password))))
          (deferred:nextc it
            (lambda (response)
              (message "Received response %s" (request-response-data response)))))
        :catch
        (lambda (err) (message "error: %s" err))))))

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
