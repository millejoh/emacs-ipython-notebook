;;; ein-websocket.el --- Wrapper of websocket.el

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

(eval-when-compile (require 'cl))
(require 'websocket)
(require 'ein-core)
(require 'url-cookie)
(require 'request)

(defstruct ein:$websocket
  "A wrapper object of `websocket'.

`ein:$websocket-ws'               : an instance returned by `websocket-open'

`ein:$websocket-onmessage'        : function called with (PACKET &rest ARGS)'
`ein:$websocket-onclose'          : function called with (WEBSOCKET &rest ARGS)'
`ein:$websocket-onopen'           : function called with (&rest ARGS)'

`ein:$websocket-onmessage-args'   : optional arguments for onmessage callback'
`ein:$websocket-onclose-args'     : optional arguments for onclose callback'
`ein:$websocket-onopen-args'      : optional arguments for onopen callback'

`ein:$websocket-closed-by-client' : t/nil'
"
  ws
  onmessage
  onmessage-args
  onclose
  onclose-args
  onopen
  onopen-args
  closed-by-client)

;; Fix issues reading cookies in request when using curl backend
(defun fix-request-netscape-cookie-parse (next-method)
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

;;(advice-add 'request--netscape-cookie-parse :around #'fix-request-netscape-cookie-parse)

;; This seems redundant, but websocket does not work otherwise.
(defun ein:websocket--prepare-cookies (url)
  (let* ((jh-conn (ein:jupyterhub-url-p url))
         (parsed-url (url-generic-parse-url url))
         (host-port (if (url-port-if-non-default parsed-url)
                        (format "%s:%s" (url-host parsed-url) (url-port parsed-url))
                      (url-host parsed-url)))
         (securep (string-match "^wss://" url))
         (http-only-cookies (request-cookie-alist (concat "#HttpOnly_" (url-host (url-generic-parse-url url))) "/" securep)) ;; Current version of Jupyter store cookies as HttpOnly)
         (cookies (request-cookie-alist (url-host (url-generic-parse-url url)) "/" securep))
         (hub-cookies (request-cookie-alist (url-host (url-generic-parse-url url)) "/hub/" securep))
         (user-cookies (and jh-conn
                            (request-cookie-alist
                             (url-host (url-generic-parse-url url))
                             (ein:$jh-user-server (ein:$jh-conn-user jh-conn))
                             securep))))
    (when (or cookies http-only-cookies hub-cookies user-cookies)
      (ein:log 'debug "Storing cookies in prep for opening websocket (%s)" cookies)
      (dolist (c (append cookies http-only-cookies hub-cookies user-cookies))
        (url-cookie-store (car c) (cdr c) nil host-port (car (url-path-and-query parsed-url)) securep)))))


;; Issues opening websockets in IPython 2.0, think it is related to
;; http://stackoverflow.com/questions/22202182/error-on-websocket-when-try-to-use-ipython-notebook-in-emacs
(defun ein:websocket (url &optional onmessage onclose onopen
                          onmessage-args onclose-args onopen-args)
  (ein:websocket--prepare-cookies url)
  (let* ((websocket (make-ein:$websocket
                     :onmessage onmessage
                     :onclose onclose
                     :onopen onopen
                     :onmessage-args onmessage-args
                     :onclose-args onclose-args
                     :onopen-args onopen-args))
         (ws (websocket-open
              url
              :on-open
              (lambda (ws)
                (let ((websocket (websocket-client-data ws)))
                  (ein:aif (ein:$websocket-onopen websocket)
                      (apply it (ein:$websocket-onopen-args websocket)))))
              :on-message
              (lambda (ws frame)
                (let ((websocket (websocket-client-data ws))
                      (packet (websocket-frame-payload frame)))
                  (ein:aif (ein:$websocket-onmessage websocket)
                      (when packet
                        (apply it packet
                               (ein:$websocket-onmessage-args websocket))))))
              :on-close
              (lambda (ws)
                (let ((websocket (websocket-client-data ws)))
                  (ein:aif (ein:$websocket-onclose websocket)
                      (apply it websocket
                             (ein:$websocket-onclose-args websocket)))))
              :on-error
              (lambda (ws action err)
                (ein:log 'error "Error %s on websocket %s action %s." err ws action)))))
    (setf (websocket-client-data ws) websocket)
    (setf (ein:$websocket-ws websocket) ws)
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
          (ein:$kernel-channels kernel)
          (json-encode (plist-put msg :channel "shell"))))))

(defun ein:websocket-send-stdin-channel (kernel msg)
  (cond ((= (ein:$kernel-api-version kernel) 2)
         (ein:log 'warn "Stdin messages only supported with IPython 3."))
        ((>= (ein:$kernel-api-version kernel) 3)
         (ein:websocket-send
          (ein:$kernel-channels kernel)
          (json-encode (plist-put msg :channel "stdin"))))))

(provide 'ein-websocket)

;;; ein-websocket.el ends here
