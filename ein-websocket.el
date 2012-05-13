;;; ein-websocket.el --- Wrapper of websocket.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

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

(require 'ein-utils)
(require 'ein-log)


(defstruct ein:$websocket
  "A wrapper object of `websocket'.

`ein:$websocket-ws'               : an instance returned by `websocket-open'
`ein:$websocket-readyState'       : one of '(connecting open closing closed)

`ein:$websocket-onmessage'        : function called with (PACKET &rest ARGS)'
`ein:$websocket-onclose'          : function called with (WEBSOCKET &rest ARGS)'
`ein:$websocket-onopen'           : function called with (&rest ARGS)'

`ein:$websocket-onmessage-args'   : optional arguments for onmessage callback'
`ein:$websocket-onclose-args'     : optional arguments for onclose callback'
`ein:$websocket-onopen-args'      : optional arguments for onopen callback'

`ein:$websocket-closed-by-client' : t/nil'
"
  ws
  readyState
  onmessage
  onmessage-args
  onclose
  onclose-args
  onopen
  onopen-args
  closed-by-client)
;; FIXME: probably, first arguments of any callback must be WEBSOCKET.


(defun ein:websocket (url &optional onmessage onclose onopen
                          onmessage-args onclose-args onopen-args)
  (let ((websocket (make-ein:$websocket
                    :readyState 'connecting
                    :onmessage onmessage
                    :onclose onclose
                    :onopen onopen
                    :onmessage-args onmessage-args
                    :onclose-args onclose-args
                    :onopen-args onopen-args)))
    (setf (ein:$websocket-ws websocket)
          (lexical-let ((websocket websocket))
            (websocket-open
             url
             (lambda (packet) (ein:websocket-filter websocket packet))
             (lambda () (ein:websocket-onclose websocket)))))
    ;; Pseudo onopen callback.  Until websocket.el supports it.
    (run-at-time 1 nil
                 (lambda (ws)
                   (ein:aif (ein:$websocket-onopen ws)
                       (apply it (ein:$websocket-onopen-args ws)))
                   (setf (ein:$websocket-readyState ws) 'open))
                 websocket)
    websocket))


(defun ein:websocket-open-p (websocket)
  (eql (ein:$websocket-readyState websocket) 'open))


(defun ein:websocket-send (websocket text)
  (websocket-send (ein:$websocket-ws websocket) text))


(defun ein:websocket-close (websocket)
  (setf (ein:$websocket-closed-by-client websocket) t)
  (websocket-close (ein:$websocket-ws websocket)))


(defun ein:websocket-filter (websocket packet)
  (let ((onmessage (ein:$websocket-onmessage websocket)))
    (when onmessage
      ;; Workaround.  Make onmessage function little bit robust when
      ;; websocket.el failed to ignore handshake.
      ;; See: https://github.com/ahyatt/emacs-websocket/issues/8
      ;; Note that this workaround only works for JSON based
      ;; communication, which is OK for using with IPython.
      (unless (string-prefix-p "{" packet)
        (let ((start-point (string-match "\0{" packet))
              lost)
          (if start-point
              (progn
                (setq packet (substring packet (1+ start-point)))
                (setq lost (substring packet 0 (1- start-point))))
            (setq lost packet)
            (setq packet nil))
          (ein:log 'verbose
            (concat "I am sorry, you may have lost message from kernel. "
                    "Notebook data is safe."))
          (ein:log 'debug "Discarded data: %s" lost)))
      ;; Do not call callback when you lost all data
      (when packet
        (apply onmessage packet (ein:$websocket-onmessage-args websocket))))))


(defun ein:websocket-onclose (websocket)
  (setf (ein:$websocket-readyState websocket) 'closing)
  (ein:aif (ein:$websocket-onclose websocket)
      (apply it websocket (ein:$websocket-onclose-args websocket)))
  (setf (ein:$websocket-readyState websocket) 'closed))


(provide 'ein-websocket)

;;; ein-websocket.el ends here
