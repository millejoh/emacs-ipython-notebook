;;; ein-websocket.el --- Wrapper of websocket.el    -*- lexical-binding:t -*-

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-websocket.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-websocket.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-websocket.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'websocket)
(require 'ein-core)
(require 'ein-classes)
(require 'url-cookie)
(require 'request)

(defun ein:websocket-store-cookie (c host-port url-filename securep)
  (url-cookie-store (car c) (cdr c) nil host-port url-filename securep))

(defun ein:maybe-get-jhconn-user (url)
  (let ((paths (cl-rest (split-string (url-filename (url-generic-parse-url url)) "/"))))
    (when (string= (cl-first paths) "user")
      (list (format "/%s/%s/" (cl-first paths) (cl-second paths))))))

(defun ein:websocket--prepare-cookies (url)
  "Websocket gets its cookies using the url-cookie API, so we need
to transcribe any cookies stored in `request-cookie-alist' during
earlier calls to `request' (request.el)."
  (let* ((parsed-url (url-generic-parse-url url))
         (host-port (format "%s:%s" (url-host parsed-url) (url-port parsed-url)))
         (base-url (file-name-as-directory (url-filename parsed-url)))
         (securep (string-match "^wss://" url))
         (read-cookies-func (lambda (path)
                              (request-cookie-alist
                               (url-host parsed-url) path securep)))
         (cookies (cl-loop
                   repeat 4
                   for cand = (cl-mapcan read-cookies-func
                                         `("/"
                                           "/hub/"
                                           ,base-url
                                           ,@(ein:maybe-get-jhconn-user url)))
                   until (cl-some (lambda (x) (string= "_xsrf" (car x))) cand)
                   do (ein:log 'info
                        "ein:websocket--prepare-cookies: no _xsrf among %s, retrying."
                        cand)
                   do (sleep-for 0 300)
                   finally return cand)))
    (dolist (c cookies)
      (ein:websocket-store-cookie
       c host-port (car (url-path-and-query parsed-url)) securep))))

(defun ein:websocket (url kernel on-message on-close on-open)
  (ein:websocket--prepare-cookies (ein:$kernel-ws-url kernel))
  (let* ((ws (websocket-open url
                             :on-open on-open
                             :on-message on-message
                             :on-close on-close
                             :on-error (lambda (ws action err)
                                         (ein:log 'info "WS action [%s] %s (%s)"
                                                  err action (websocket-url ws)))))
         (websocket (make-ein:$websocket :ws ws :kernel kernel :closed-by-client nil)))
    (setf (websocket-client-data ws) websocket)
    websocket))

(defun ein:websocket-open-p (websocket)
  (eql (websocket-ready-state (ein:$websocket-ws websocket)) 'open))


(defun ein:websocket-send (websocket text)
  ;;  (ein:log 'info "WS: Sent message %s" text)
  (condition-case-unless-debug err
      (websocket-send-text (ein:$websocket-ws websocket) text)
    (error (message "Error %s on sending websocket message %s." err text))))


(defun ein:websocket-close (websocket)
  (setf (ein:$websocket-closed-by-client websocket) t)
  (websocket-close (ein:$websocket-ws websocket)))


(defun ein:websocket-send-shell-channel (kernel msg)
  (cond ((= (ein:$kernel-api-version kernel) 2)
         (ein:websocket-send
          (ein:$kernel-shell-channel kernel)
          (ein:json-encode msg)))
        ((>= (ein:$kernel-api-version kernel) 3)
         (ein:websocket-send
          (ein:$kernel-websocket kernel)
          (ein:json-encode (plist-put msg :channel "shell"))))))

(defun ein:websocket-send-stdin-channel (kernel msg)
  (cond ((= (ein:$kernel-api-version kernel) 2)
         (ein:log 'warn "Stdin messages only supported with IPython 3."))
        ((>= (ein:$kernel-api-version kernel) 3)
         (ein:websocket-send
          (ein:$kernel-websocket kernel)
          (ein:json-encode (plist-put msg :channel "stdin"))))))

(provide 'ein-websocket)

;;; ein-websocket.el ends here
