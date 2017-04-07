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
