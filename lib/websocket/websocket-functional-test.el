;;; websocket-functional-test.el --- Simple functional testing

;; Copyright (c) 2013, 2016  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage: emacs -batch -Q -L . -l websocket-functional-test.el
;;
;; Note: this functional tests requires that you have python with the
;; Tornado web server.  See http://www.tornadoweb.org/en/stable/ for
;; information on aquiring.

(require 'tls)   ;; tests a particular bug we had on emacs 23
(setq debug-on-error t)
(require 'websocket)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;
;; Local server test ;;
;;;;;;;;;;;;;;;;;;;;;;;

(message "Testing with local server")

(setq websocket-debug t)

(defvar wstest-server-buffer (get-buffer-create "*wstest-server*"))
(defvar wstest-server-name "wstest-server")
(defvar wstest-server-proc
  (start-process wstest-server-name wstest-server-buffer
                 "python" "testserver.py" "--log_to_stderr" "--logging=debug"))
(sleep-for 1)

(defvar wstest-msgs nil)
(defvar wstest-closed nil)

(message "Opening the websocket")

(defvar wstest-ws
  (websocket-open
   "ws://127.0.0.1:9999"
   :on-message (lambda (_websocket frame)
                 (push (websocket-frame-payload frame) wstest-msgs)
                 (message "ws frame: %S" (websocket-frame-payload frame))
                 (error "Test error (expected)"))
   :on-close (lambda (_websocket) (setq wstest-closed t))))

(defun wstest-pop-to-debug ()
  "Open websocket log buffer. Not used in testing. Just for debugging."
  (interactive)
  (pop-to-buffer (websocket-get-debug-buffer-create wstest-ws)))

(sleep-for 0.1)
(assert (websocket-openp wstest-ws))

(assert (null wstest-msgs))

(websocket-send-text wstest-ws "Hi!")

(sleep-for 0.1)
(assert (equal (car wstest-msgs) "You said: Hi!"))
(setf (websocket-on-error wstest-ws) (lambda (_ws _type _err)))
(websocket-send-text wstest-ws "Hi after error!")
(sleep-for 0.1)
(assert (equal (car wstest-msgs) "You said: Hi after error!"))

(websocket-close wstest-ws)
(assert (null (websocket-openp wstest-ws)))

(if (not (eq system-type 'windows-nt))
    ; Windows doesn't have support for the SIGSTP signal, so we'll just kill
    ; the process.
    (stop-process wstest-server-proc))
(kill-process wstest-server-proc)

;; Make sure the processes are closed.  This happens asynchronously,
;; so let's wait for it.
(sleep-for 1)
(assert (null (process-list)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote server test, with wss ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= (string-to-number (substring emacs-version 0 2)) 24)
  (message "Testing with wss://echo.websocket.org")
  (when (eq system-type 'windows-nt)
    (message "Windows users must have gnutls DLLs in the emacs bin directory."))
  (setq wstest-ws
        (websocket-open
         "wss://echo.websocket.org"
         :on-open (lambda (_websocket)
                    (message "Websocket opened"))
         :on-message (lambda (_websocket frame)
                       (push (websocket-frame-payload frame) wstest-msgs)
                       (message "ws frame: %S" (websocket-frame-payload frame)))
         :on-close (lambda (_websocket)
                     (message "Websocket closed")
                     (setq wstest-closed t)))
        wstest-msgs nil)
  (sleep-for 0.3)
  (assert (websocket-openp wstest-ws))
  (assert (eq 'open (websocket-ready-state wstest-ws)))
  (assert (null wstest-msgs))
  (websocket-send-text wstest-ws "Hi!")
  (sleep-for 1)
  (assert (equal (car wstest-msgs) "Hi!"))
  (websocket-close wstest-ws))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local client and server ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Testing with emacs websocket server.")
(message "If this does not pass, make sure your firewall allows the connection.")
(setq wstest-closed nil)
(let ((server-conn (websocket-server
                    9998
                    :host 'local
                    :on-message (lambda (ws frame)
                                  (message "Server received text!")
                                  (websocket-send-text ws
                                                       (websocket-frame-payload frame)))
                    :on-open (lambda (_websocket) "Client connection opened!")
                    :on-close (lambda (_websocket)
                                (setq wstest-closed t)))))

  (setq wstest-msgs nil
        wstest-ws
        (websocket-open
         "ws://localhost:9998"
         :on-message (lambda (_websocket frame)
                       (push (websocket-frame-payload frame) wstest-msgs)
                       (message "ws frame: %S" (websocket-frame-payload frame)))))

  (assert (websocket-openp wstest-ws))
  (websocket-send-text wstest-ws "Hi to self!")
  (sleep-for 0.3)
  (assert (equal (car wstest-msgs) "Hi to self!"))
  (websocket-server-close server-conn))
(assert wstest-closed)
(websocket-close wstest-ws)

(sleep-for 1)
(assert (null (process-list)) t)
(message "\nAll tests passed!\n")
