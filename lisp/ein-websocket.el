;;; ein-websocket.el --- Wrapper of websocket.el   -*- lexical-binding: t -*-

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

;; Fix issues reading cookies in request when using curl backend
(defun fix-request-netscape-cookie-parse (_next-method)
  "Parse Netscape/Mozilla cookie format."
  (goto-char (point-min))
  (let ((tsv-re (concat "^\\="
                        (cl-loop repeat 6 concat "\\([^\t\n]+\\)\t")
                        "\\(.*\\)"))
        cookies)
    (forward-line 3) ;; Skip header (first three lines)
    (while
        (and
         (cond
          ((re-search-forward "^\\=$" nil t))
          ((re-search-forward tsv-re)
           (push (cl-loop for i from 1 to 7 collect (match-string i))
                 cookies)
           t))
         (= (forward-line 1) 0)
         (not (= (point) (point-max)))))
    (setq cookies (nreverse cookies))
    (cl-loop for (domain flag path secure expiration name value) in cookies
             collect (list domain
                           (equal flag "TRUE")
                           path
                           (equal secure "TRUE")
                           (string-to-number expiration)
                           name
                           value))))

(defsubst ein:websocket-store-cookie (c host-port url-filename securep)
  (url-cookie-store (car c) (cdr c) nil host-port url-filename securep))

(defun ein:maybe-get-jhconn-user (url)
  (let ((paths (cl-rest (split-string (url-filename (url-generic-parse-url url)) "/"))))
    (if (string= (car paths) "user")
        (format "/%s/%s/" (car paths) (cadr paths))
      "")))

;;(advice-add 'request--netscape-cookie-parse :around #'fix-request-netscape-cookie-parse)
(defun ein:websocket--prepare-cookies (url)
  "Websocket gets its cookies using the url-cookie API, so we need to copy over
 any cookies that are made and stored during the contents API calls via
 emacs-request."
  (let* ((parsed-url (url-generic-parse-url url))
         (host-port (if (url-port-if-non-default parsed-url)
                        (format "%s:%s" (url-host parsed-url) (url-port parsed-url))
                      (url-host parsed-url)))
         (securep (string-match "^wss://" url))
         (read-cookies-func (lambda (path)
                              (request-cookie-alist
                               (url-host parsed-url) path securep)))
         (cookies (cl-loop repeat 4
                    for cand = (mapcan read-cookies-func
                                       `("/" "/hub/"
                                             ,(ein:maybe-get-jhconn-user url)))
                    until (cl-some (lambda (x) (string= "_xsrf" (car x))) cand)
                    do (ein:log 'info
                         "ein:websocket--prepare-cookies: no _xsrf among %s, retrying."
                         cand)
                    do (sleep-for 0 300)
                    finally return cand)))
    (dolist (c cookies)
      (ein:websocket-store-cookie c host-port
                                  (car (url-path-and-query parsed-url)) securep))))

(defun ein:websocket (url kernel on-message on-close on-open)
  (ein:websocket--prepare-cookies url)
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
          (json-encode msg)))
        ((>= (ein:$kernel-api-version kernel) 3)
         (ein:websocket-send
          (ein:$kernel-websocket kernel)
          (json-encode (plist-put msg :channel "shell"))))))

(defun ein:websocket-send-stdin-channel (kernel msg)
  (cond ((= (ein:$kernel-api-version kernel) 2)
         (ein:log 'warn "Stdin messages only supported with IPython 3."))
        ((>= (ein:$kernel-api-version kernel) 3)
         (ein:websocket-send
          (ein:$kernel-websocket kernel)
          (json-encode (plist-put msg :channel "stdin"))))))

(provide 'ein-websocket)

;;; ein-websocket.el ends here
