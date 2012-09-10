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


(defun ein:websocket (url &optional onmessage onclose onopen
                          onmessage-args onclose-args onopen-args)
  (let ((websocket (make-ein:$websocket
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
                            (ein:$websocket-onclose-args websocket))))))))
    (setf (websocket-client-data ws) websocket)
    (setf (ein:$websocket-ws websocket) ws)
    websocket))


(defun ein:websocket-open-p (websocket)
  (eql (websocket-ready-state (ein:$websocket-ws websocket)) 'open))


(defun ein:websocket-send (websocket text)
  (websocket-send-text (ein:$websocket-ws websocket) text))


(defun ein:websocket-close (websocket)
  (setf (ein:$websocket-closed-by-client websocket) t)
  (websocket-close (ein:$websocket-ws websocket)))


(provide 'ein-websocket)

;;; ein-websocket.el ends here
