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
;;;
;;; An interface to the Jupyterhub login and management API as described in
;;; http://jupyterhub.readthedocs.io/en/latest/api/index.html
;;;

;;

;;; Code:
(require 'request-deferred)
(require 'deferred)
(require 'ein-query)


(defun ein:jupyterhub-api-url (url-or-port command &rest args)
  (if args
      (apply #'ein:url url-or-port "hub/api" command args)
    (ein:url url-or-port "hub/api" command)))

(defun ein:jh-ask-url-or-port ()
  (let* ((url-or-port-list (mapcar (lambda (x) (format "%s" x))
                                   ein:url-or-port))
         (default (format "%s" (ein:default-url-or-port)))
         (url-or-port
          (completing-read (format "URL or port number (default %s): " default)
                           url-or-port-list
                           nil nil nil nil
                           default)))
    (if (string-match "^[0-9]+$" url-or-port)
        (string-to-number url-or-port)
      url-or-port)))

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
        (ein:jupyterhub-login conn user password)))
    (deferred:nextc it
      (lambda (conn)
        (unless conn
          (error "Connection to Jupyterhub server at %s failed! Maybe you used the wrong URL?" url-or-port))
        (ein:jupyterhub-token-request conn)))
    (deferred:nextc it
      (lambda (conn)
        (setf (gethash (ein:$jh-conn-url conn) *ein:jupyterhub-servers*) conn)
        (ein:jupyterhub-start-server conn user)))))

(defun ein:jupyterhub-login (conn username password)
  (deferred:$
    (ein:query-deferred
     (ein:url (ein:$jh-conn-url conn) "hub/login")
     :type "POST"
     :parser #'ein:json-read
     :data (format "username=%s&password=%s" username password) ;; (json-encode`((username . ,username)
           ;;               (password . , password)))
     )
    (deferred:nextc it
      (lambda (response)
        (ein:log 'info "Login for user %s with response %s." username (request-response-status-code response))
        conn))))

(defun ein:jupyterhub-token-request (conn)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url (ein:$jh-conn-url conn)
                             "authorizations/token")
     :type "POST"
     :timeout ein:content-query-timeout
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (ein:log 'info "response-data: %s, %s"
                 (request-response-data response)
                 (cadr (request-response-data response))) ;; FIXME: Why doesn't plist-get work?
        (unless (eql (request-response-status-code response) 403)
          (setf (ein:$jh-conn-token conn) (cadr (request-response-data response))))
        conn))))

(defun ein:jupyterhub-get-user (conn username)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url (ein:$jh-conn-url conn)
                             "users"
                             username)
     :type "GET"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (let* ((data (request-response-data response))
               (user (make-ein:$jh-user :name (plist-get data :name)
                                        :admin (plist-get data :admin)
                                        :groups (plist-get data :groups)
                                        :server (plist-get data :server)
                                        :pending (plist-get data :pending)
                                        :last-activity (plist-get data :last_activity))))
          (ein:log 'info "Jupyterhub: Found user: %s" user)
          user)))))

(defun ein:jupyterhub-start-server (conn username)
  (deferred:$
    (ein:query-deferred
     (ein:jupyterhub-api-url (ein:$jh-conn-url conn)
                             "users"
                             username
                             "server")
     :type "POST"
     :parser #'ein:json-read)
    (deferred:nextc it
      (lambda (response)
        (ein:log 'info "Jupyterhub: Response status: %s" (request-response-status-code response))
        (case (request-response-status-code response)
          ((201 400)
           (ein:log 'info "Jupyterhub: Finding user: %s" username)
           (ein:jupyterhub-get-user conn username)))))
    (deferred:nextc it
      (lambda (user)
        (ein:log 'info "Jupyterhub: Found user? (%s)" user)
        (when (ein:$jh-user-p user)
          (setf (ein:$jh-conn-user conn) user)
          (ein:log 'info "Jupyterhub: Opening notebook at %s: " (ein:$jh-conn-url conn))
          (ein:notebooklist-open (ein:$jh-conn-url conn)))))))

;;;###autoload
(defun ein:jupyterhub-connect (url user password)
  (interactive (list (ein:jh-ask-url-or-port)
                     (read-string "User: ")
                     (read-passwd "Password: ")))
  (ein:jupyterhub--do-connect url user password))

(provide 'ein-jupyterhub)
