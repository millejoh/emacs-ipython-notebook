;;; ein-kernel.el --- Communicate with IPython notebook server

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-kernel.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-kernel.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ansi-color)

(require 'ein-core)
(require 'ein-log)
;; FIXME: use websocket.el directly once v1.0 is released.
(require 'ein-websocket)
(require 'ein-events)
(require 'ein-query)
(require 'ein-ipdb)

;; FIXME: Rewrite `ein:$kernel' using `defclass'.  It should ease
;;        testing since I can mock I/O using method overriding.
(defstruct ein:$kernel
  "Hold kernel variables.

`ein:$kernel-url-or-port'
  URL or port of IPython server.
"
  url-or-port
  events
  api-version
  session-id
  kernel-id
  shell-channel
  iopub-channel
  channels                              ; For IPython 3.x+
  base-url                              ; /api/kernels/
  kernel-url                            ; /api/kernels/<KERNEL-ID>
  ws-url                                ; ws://<URL>[:<PORT>]
  stdin-activep
  running
  username
  msg-callbacks
  ;; FIXME: Use event instead of hook.
  after-start-hook
  after-execute-hook)

;; "Public" getters.  Use them outside of this package.

(defun ein:$kernel-session-url (kernel)
  (concat "/api/sessions/" (ein:$kernel-session-id kernel)))

;;;###autoload
(defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port)

;;;###autoload
(defalias 'ein:kernel-id 'ein:$kernel-kernel-id)


;;; Initialization and connection.

(defun ein:kernel-new (url-or-port base-url events &optional api-version)
  (make-ein:$kernel
   :url-or-port url-or-port
   :events events
   :api-version (or api-version 2)
   :session-id (ein:utils-uuid)
   :kernel-id nil
   :channels nil
   :shell-channel nil
   :iopub-channel nil
   :base-url base-url
   :running nil
   :stdin-activep nil
   :username "username"
   :msg-callbacks (make-hash-table :test 'equal)))


(defun ein:kernel-del (kernel)
  "Destructor for `ein:$kernel'."
  (ein:kernel-disconnect kernel))


(defun ein:kernel--get-msg (kernel msg-type content)
  (list
   :header (list
            :msg_id (ein:utils-uuid)
            :username (ein:$kernel-username kernel)
            :session (ein:$kernel-session-id kernel)
            :msg_type msg-type)
   :metadata (make-hash-table)
   :content content
   :parent_header (make-hash-table)))


(defun ein:kernel-start (kernel notebook)
  "Start kernel of the notebook whose id is NOTEBOOK-ID."
  (unless (ein:$kernel-running kernel)
    (if (= (ein:$kernel-api-version kernel) 2)
        (let ((path (substring (ein:$notebook-notebook-path notebook)
                               0
                               (or (position ?/ (ein:$notebook-notebook-path notebook)
                                             :from-end t)
                                   0))))
          (ein:kernel-start--legacy kernel
                                    (ein:$notebook-notebook-name notebook)
                                    path))
      (let ((kernelspec (ein:$notebook-kernelspec notebook)))
        (ein:query-singleton-ajax
         (list 'kernel-start (ein:$kernel-kernel-id kernel))
         (ein:url (ein:$kernel-url-or-port kernel)
                  "api/sessions")
         :type "POST"
         :data (json-encode `(("notebook" .
                               (("path" . ,(ein:$notebook-notebook-path notebook))))
                              ,@(if kernelspec
                                    `(("kernel" .
                                       (("name" . ,(ein:$kernelspec-name kernelspec))))))))
         :parser #'ein:json-read
         :success (apply-partially #'ein:kernel--kernel-started kernel))))))

(defun ein:kernel-start--legacy (kernel notebook-id path)
  (unless (ein:$kernel-running kernel)
    (if (not path)
        (setq path ""))
    (ein:query-singleton-ajax
     (list 'kernel-start notebook-id)
     (ein:url (ein:$kernel-url-or-port kernel)
              "api/sessions")
     :type "POST"
     :data (json-encode `(("notebook" .
                           (("name" . ,notebook-id)
                            ("path" . ,path)))))
     :parser #'ein:json-read
     :success (apply-partially #'ein:kernel--kernel-started kernel))))

(defun ein:kernel-restart (kernel)
  (ein:events-trigger (ein:$kernel-events kernel)
                      'status_restarting.Kernel)
  (ein:log 'info "Restarting kernel - local settings will be lost!")
  (when (ein:$kernel-running kernel)
    (ein:kernel-kill kernel
                     (apply-partially #'ein:kernel-start kernel (ein:get-notebook-or-error)))
    ;; (ein:query-singleton-ajax
    ;;  (list 'kernel-restart (ein:$kernel-kernel-id kernel))
    ;;  (ein:url (ein:$kernel-url-or-port kernel)
    ;;           (ein:$kernel-kernel-url kernel)
    ;;           "restart")
    ;;  :type "POST"
    ;;  :parser #'ein:json-read
    ;;  :success (apply-partially #'ein:kernel--kernel-started kernel))
    ))


(defun* ein:kernel--kernel-started (kernel &key data &allow-other-keys)
  (let ((session-id (plist-get data :id)))
    (if (plist-get data :kernel)
        (setq data (plist-get data :kernel)))
    (destructuring-bind (&key id &allow-other-keys) data
      (unless id
        (error "Failed to start kernel.  No `kernel_id'.  Got %S."
               data))
      (ein:log 'info "Kernel started: %s" id)
      (setf (ein:$kernel-running kernel) t)
      (setf (ein:$kernel-kernel-id kernel) id)
      (setf (ein:$kernel-session-id kernel) session-id)
      (setf (ein:$kernel-ws-url kernel) (ein:kernel--ws-url (ein:$kernel-url-or-port kernel)))
      (setf (ein:$kernel-kernel-url kernel)
            (concat (ein:$kernel-base-url kernel) "/" id)))
    (ein:kernel-start-channels kernel)
    (if (= (ein:$kernel-api-version kernel) 2)
        (let ((shell-channel (ein:$kernel-shell-channel kernel))
              (iopub-channel (ein:$kernel-iopub-channel kernel)))
          ;; FIXME: get rid of lexical-let
          (lexical-let ((kernel kernel))
            (setf (ein:$websocket-onmessage shell-channel)
                  (lambda (packet)
                    (ein:kernel--handle-shell-reply kernel packet)))
            (setf (ein:$websocket-onmessage iopub-channel)
                  (lambda (packet)
                    (ein:kernel--handle-iopub-reply kernel packet)))))
      (lexical-let ((kernel kernel))
        (setf (ein:$websocket-onmessage (ein:$kernel-channels kernel))
              (lambda (packet)
                (ein:kernel--handle-channels-reply kernel packet)))))))


(defun ein:kernel--ws-url (url-or-port &optional securep)
  "Use `ein:$kernel-url-or-port' if BASE_URL is an empty string.
See: https://github.com/ipython/ipython/pull/3307"
  (let ((protocol (if (or securep
                          (and (stringp url-or-port)
                               (string-match "^https://" url-or-port)))
                      "wss"
                    "ws")))
    (if (integerp url-or-port)
        (format "%s://127.0.0.1:%s" protocol url-or-port)
      (let* ((url (if (string-match "^https?://" url-or-port)
                      url-or-port
                    (format "http://%s" url-or-port)))
             (parsed-url (url-generic-parse-url url)))
        (format "%s://%s:%s" protocol (url-host parsed-url) (url-port parsed-url))))))


(defun ein:kernel--websocket-closed (kernel ws-url early)
  (if early
      (ein:display-warning
       "Websocket connection to %s could not be established.
  You will NOT be able to run code.  Your websocket.el may not be
  compatible with the websocket version in the server, or if the
  url does not look right, there could be an error in the
  server's configuration." ws-url)
    (ein:display-warning "Websocket connection closed unexpectedly.
  The kernel will no longer be responsive.")))


(defun ein:kernel-send-cookie (channel host)
  ;; cookie can be an empty string for IPython server with no password,
  ;; but something must be sent to start channel.
  (let ((cookie (ein:query-get-cookie host "/")))
    (ein:websocket-send channel cookie)))


(defun ein:kernel--ws-closed-callback (websocket kernel arg)
  ;; NOTE: The argument ARG should not be "unpacked" using `&rest'.
  ;; It must be given as a list to hold state `already-called-onclose'
  ;; so it can be modified in this function.
  (destructuring-bind (&key already-called-onclose ws-url early)
      arg
    (unless already-called-onclose
      (plist-put arg :already-called-onclose t)
      (unless (ein:$websocket-closed-by-client websocket)
        ;; Use "event-was-clean" when it is implemented in websocket.el.
        (ein:kernel--websocket-closed kernel ws-url early)))))

(defun ein:start-channels-multiple-websocket (kernel)
  "Start kernel channels for IPython notebook v2.x"
  (let* ((ws-url (concat (ein:$kernel-ws-url kernel)
                         (ein:$kernel-kernel-url kernel)))
         (onclose-arg (list :ws-url ws-url
                            :already-called-onclose nil
                            :early t)))
    (ein:log 'info "Starting WS channels: %S" ws-url)
    (setf (ein:$kernel-shell-channel kernel) (ein:websocket (concat ws-url "/shell")))
    (setf (ein:$kernel-iopub-channel kernel) (ein:websocket (concat ws-url "/iopub")))
    (loop for c in (list (ein:$kernel-shell-channel kernel)
                         (ein:$kernel-iopub-channel kernel))
          do (setf (ein:$websocket-onclose-args c) (list kernel onclose-arg))
          do (setf (ein:$websocket-onopen c)
                   (lexical-let ((channel c)
                                 (kernel kernel)
                                 (host (let (url-or-port
                                             (ein:$kernel-url-or-port kernel))
                                         (if (stringp url-or-port)
                                             url-or-port
                                           ein:url-localhost))))
                     (lambda ()
                       (ein:kernel-send-cookie channel host)
                       ;; run `ein:$kernel-after-start-hook' if both
                       ;; channels are ready.
                       (when (ein:kernel-live-p kernel)
                         (ein:kernel-run-after-start-hook kernel)))))
          do (setf (ein:$websocket-onclose c)
                   #'ein:kernel--ws-closed-callback))))

(defun ein:start-channels-single-websocket (kernel)
  (let* ((ws-url (concat (ein:$kernel-ws-url kernel)
                         (ein:$kernel-kernel-url kernel)))
         (channels-url (concat ws-url "/channels?session_id="
                               (ein:$kernel-session-id kernel)))
         (onclose-arg (list :ws-url ws-url
                            :already-called-onclose nil
                            :early t)))
    (ein:log 'info "Starting channels WS: %S" channels-url)
    (setf (ein:$kernel-channels kernel) (ein:websocket channels-url))
    (let ((c (ein:$kernel-channels kernel)))
      (setf (ein:$websocket-onclose-args c) (list kernel onclose-arg))
      (setf (ein:$websocket-onopen c)
            (lexical-let ((kernel kernel))
              (lambda ()
                (ein:kernel-connect-request kernel (list :kernel_connect_reply (cons 'ein:kernel-on-connect kernel)))
                ;; run `ein:$kernel-after-start-hook' if both
                ;; channels are ready.
                (when (ein:kernel-live-p kernel)
                  (ein:kernel-run-after-start-hook kernel)))))
      (setf (ein:$websocket-onclose c)
            #'ein:kernel--ws-closed-callback))))

(defun ein:kernel-start-channels (kernel)
  ;(ein:kernel-stop kernel)
  (let* ((api-version (ein:$kernel-api-version kernel))
         (ws-url (concat (ein:$kernel-ws-url kernel)
                         (ein:$kernel-kernel-url kernel)))
         (onclose-arg (list :ws-url ws-url
                            :already-called-onclose nil
                            :early t)))
    (cond ((= api-version 2)
           (ein:start-channels-multiple-websocket kernel))
          ((= api-version 3)
           (ein:start-channels-single-websocket kernel)))
    ;; switch from early-close to late-close message after 1s
    (run-at-time
     2 nil
     (lambda (onclose-arg)
       (plist-put onclose-arg :early nil)
       (ein:log 'debug "(via run-at-time) onclose-arg changed to: %S"
                onclose-arg))
     onclose-arg)))

;; NOTE: `onclose-arg' can be accessed as:
;; (nth 1 (ein:$websocket-onclose-args (ein:$kernel-shell-channel (ein:$notebook-kernel ein:notebook))))

(defun ein:kernel-on-connect (kernel content -metadata-not-used-)
  (ein:log 'info "Kernel connect_request_reply received."))

(defun ein:kernel-run-after-start-hook (kernel)
  (ein:log 'debug "EIN:KERNEL-RUN-AFTER-START-HOOK")
  (mapc #'ein:funcall-packed
        (ein:$kernel-after-start-hook kernel)))

(defun ein:kernel-disconnect (kernel &optional callback)
  "Disconnect websocket connection to running kernel, but do not
kill the kernel."
  (when (ein:$kernel-channels kernel)
    (setf (ein:$websocket-onclose (ein:$kernel-channels kernel)) nil)
    (ein:websocket-close (ein:$kernel-channels kernel))
    (setf (ein:$kernel-channels kernel) nil))
  (when (ein:$kernel-shell-channel kernel)
    (setf (ein:$websocket-onclose (ein:$kernel-shell-channel kernel)) nil)
    (ein:websocket-close (ein:$kernel-shell-channel kernel))
    (setf (ein:$kernel-shell-channel kernel) nil))
  (when (ein:$kernel-iopub-channel kernel)
    (setf (ein:$websocket-onclose (ein:$kernel-iopub-channel kernel)) nil)
    (ein:websocket-close (ein:$kernel-iopub-channel kernel))
    (setf (ein:$kernel-iopub-channel kernel) nil))
  (setf (ein:$kernel-running kernel) nil)
  (when callback
    (funcall callback)))

(defun ein:kernel-reconnect (kernel notebook)
  (ein:kernel-disconnect kernel)
  (ein:kernel-start kernel notebook))

(defun ein:kernel-live-p (kernel)
  (and
   (ein:$kernel-p kernel)
   (or
    (ein:aand (ein:$kernel-channels kernel) (ein:websocket-open-p it))
    (and
     (ein:aand (ein:$kernel-shell-channel kernel) (ein:websocket-open-p it))
     (ein:aand (ein:$kernel-iopub-channel kernel) (ein:websocket-open-p it))))))


(defmacro ein:kernel-if-ready (kernel &rest body)
  "Execute BODY if KERNEL is ready.  Warn user otherwise."
  (declare (indent 1))
  `(if (ein:kernel-live-p ,kernel)
       (progn ,@body)
     (ein:log 'warn "Kernel is not ready yet! (or closed already.)")))


;;; Main public methods

;; NOTE: The argument CALLBACKS for the following functions is almost
;;       same as the JS implementation in IPython.  However, as Emacs
;;       lisp does not support closure, value is "packed" using
;;       `cons': `car' is the actual callback function and `cdr' is
;;       its first argument.  It's like using `cons' instead of
;;       `$.proxy'.

(defun ein:kernel-object-info-request (kernel objname callbacks)
  "Send object info request of OBJNAME to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:object_info_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `object_info_reply' message.

`object_info_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#object-information
"
  (assert (ein:kernel-live-p kernel) nil "object_info_reply: Kernel is not active.")
  (when objname
    (let ((content (list :oname (format "%s" objname)))
          msg
          msg-id)
      (if (>= (ein:$kernel-api-version kernel) 3)
          (setf msg (ein:kernel--get-msg kernel "inspect_request"
                                         (append content (list :detail_level 1)))
                msg-id (plist-get (plist-get msg :header) :msg_id))
        (setf msg (ein:kernel--get-msg kernel "object_info_request" content)
              msg-id (plist-get (plist-get msg :header) :msg_id)))
      (ein:websocket-send-shell-channel kernel msg)
      (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks))))


(defun* ein:kernel-execute (kernel code &optional callbacks
                                   &key
                                   (silent t)
                                   (user-variables [])
                                   (user-expressions (make-hash-table))
                                   (allow-stdin t))
  "Execute CODE on KERNEL.

When calling this method pass a CALLBACKS structure of the form:

  (:execute_reply  EXECUTE-REPLY-CALLBACK
   :output         OUTPUT-CALLBACK
   :clear_output   CLEAR-OUTPUT-CALLBACK
   :set_next_input SET-NEXT-INPUT)

Objects end with -CALLBACK above must pack a FUNCTION and its
first ARGUMENT in a `cons'::

  (FUNCTION . ARGUMENT)

Call signature
--------------
::

  (`funcall' EXECUTE-REPLY-CALLBACK ARGUMENT          CONTENT METADATA)
  (`funcall' OUTPUT-CALLBACK        ARGUMENT MSG-TYPE CONTENT METADATA)
  (`funcall' CLEAR-OUTPUT-CALLBACK  ARGUMENT          CONTENT METADATA)
  (`funcall' SET-NEXT-INPUT         ARGUMENT TEXT)

* Both CONTENT and METADATA objects are plist.
* The MSG-TYPE argument for OUTPUT-CALLBACK is a string
  (one of `stream', `display_data', `pyout' and `pyerr').
* The CONTENT object for CLEAR-OUTPUT-CALLBACK has
  `stdout', `stderr' and `other' fields that are booleans.
* The SET-NEXT-INPUT callback will be passed the `set_next_input' payload,
  which is a string.
  See `ein:kernel--handle-shell-reply' for how the callbacks are called.

Links
-----
* For general description of CONTENT and METADATA:
  http://ipython.org/ipython-doc/dev/development/messaging.html#general-message-format
* `execute_reply' message is documented here:
  http://ipython.org/ipython-doc/dev/development/messaging.html#execute
* Output type messages is documented here:
  http://ipython.org/ipython-doc/dev/development/messaging.html#messages-on-the-pub-sub-socket

Sample implementations
----------------------
* `ein:cell--handle-execute-reply'
* `ein:cell--handle-output'
* `ein:cell--handle-clear-output'
* `ein:cell--handle-set-next-input'
"
  ;; FIXME: Consider changing callback to use `&key'.
  ;;        Otherwise, adding new arguments to callback requires
  ;;        backward incompatible changes (hence a big diff), unlike
  ;;        Javascript.  Downside of this is that there is no short way
  ;;        to write anonymous callback because there is no `lambda*'.
  ;;        You can use `function*', but that's bit long...

  ;; FIXME: Consider allowing a list of fixed argument so that the
  ;;        call signature becomes something like:
  ;;           (funcall FUNCTION [ARG ...] CONTENT METADATA)

  (assert (ein:kernel-live-p kernel) nil "execute_reply: Kernel is not active.")
  (if (not (ein:$kernel-stdin-activep kernel))
    (let* ((content (list
                     :code code
                     :silent (or silent json-false)
                     :user_variables user-variables
                     :user_expressions user-expressions
                     :allow_stdin allow-stdin))
           (msg (ein:kernel--get-msg kernel "execute_request" content))
           (msg-id (plist-get (plist-get msg :header) :msg_id)))
      (ein:websocket-send-shell-channel kernel msg)
      (unless (plist-get callbacks :execute_reply)
        (ein:log 'debug "code: %s" code))
      (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
      (unless silent
        (mapc #'ein:funcall-packed
              (ein:$kernel-after-execute-hook kernel)))
      msg-id)
    (message "[ein]: stdin active, cannot communicate with kernel.")))


(defun ein:kernel-complete (kernel line cursor-pos callbacks)
  "Complete code at CURSOR-POS in a string LINE on KERNEL.

CURSOR-POS is the position in the string LINE, not in the buffer.

When calling this method pass a CALLBACKS structure of the form:

    (:complete_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `complete_reply' message.

`complete_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#complete
"
  (assert (ein:kernel-live-p kernel) nil "complete_reply: Kernel is not active.")
  (let* ((content (list
                   :text ""
                   :line line
                   :cursor_pos cursor-pos))
         (msg (ein:kernel--get-msg kernel "complete_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun* ein:kernel-history-request (kernel callbacks
                                           &key
                                           (output nil)
                                           (raw t)
                                           (hist-access-type "tail")
                                           session
                                           start
                                           stop
                                           (n 10)
                                           pattern
                                           unique)
  "Request execution history to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:history_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `history_reply' message.

`history_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#history

Relevant Python code:

* :py:method:`IPython.zmq.ipkernel.Kernel.history_request`
* :py:class:`IPython.core.history.HistoryAccessor`
"
  (assert (ein:kernel-live-p kernel) nil "history_reply: Kernel is not active.")
  (let* ((content (list
                   :output (ein:json-any-to-bool output)
                   :raw (ein:json-any-to-bool raw)
                   :hist_access_type hist-access-type
                   :session session
                   :start start
                   :stop stop
                   :n n
                   :pattern pattern
                   :unique unique))
         (msg (ein:kernel--get-msg kernel "history_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))

(defun ein:kernel-connect-request (kernel callbacks)
  "Request basic information for a KERNEL.

When calling this method pass a CALLBACKS structure of the form::

  (:connect_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `kernel_info_reply' message.

`connect_request' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#connect

Example::

  (ein:kernel-connect-request
   (ein:get-kernel)
   '(:kernel_connect_reply (message . \"CONTENT: %S\\nMETADATA: %S\")))
"
  ;(assert (ein:kernel-live-p kernel) nil "connect_reply: Kernel is not active.")
  (let* ((msg (ein:kernel--get-msg kernel "connect_request" (make-hash-table)))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))

(defun ein:kernel-kernel-info-request (kernel callbacks)
  "Request core information of KERNEL.

When calling this method pass a CALLBACKS structure of the form::

  (:kernel_info_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `kernel_info_reply' message.

`kernel_info_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#kernel-info

Example::

  (ein:kernel-kernel-info-request
   (ein:get-kernel)
   '(:kernel_info_reply (message . \"CONTENT: %S\\nMETADATA: %S\")))
"
  (assert (ein:kernel-live-p kernel) nil "kernel_info_reply: Kernel is not active.")
  (let* ((msg (ein:kernel--get-msg kernel "kernel_info_request" nil))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun ein:kernel-interrupt (kernel)
  (when (ein:$kernel-running kernel)
    (ein:log 'info "Interrupting kernel")
    (ein:query-singleton-ajax
     (list 'kernel-interrupt (ein:$kernel-kernel-id kernel))
     (ein:url (ein:$kernel-url-or-port kernel)
              (ein:$kernel-kernel-url kernel)
              "interrupt")
     :type "POST"
     :success (lambda (&rest ignore)
                (ein:log 'info "Sent interruption command.")))))


(defun ein:kernel-kill (kernel &optional callback cbargs)
  (when kernel
    (ein:query-singleton-ajax
     (list 'kernel-kill (ein:$kernel-session-id kernel))
     (ein:url (ein:$kernel-url-or-port kernel)
              "api/sessions"
              (ein:$kernel-session-id kernel))
     :type "DELETE"
     :success (apply-partially
               (lambda (kernel callback cbargs &rest ignore)
                 (ein:log 'info "Notebook session killed.")
                 (if kernel
                     (setf (ein:$kernel-running kernel) nil))
                 (when callback (apply callback cbargs)))
               kernel callback cbargs))))

;; Reply handlers.

(defun ein:kernel-get-callbacks-for-msg (kernel msg-id)
  (gethash msg-id (ein:$kernel-msg-callbacks kernel)))

(defun ein:kernel-set-callbacks-for-msg (kernel msg-id callbacks)
  (puthash msg-id callbacks (ein:$kernel-msg-callbacks kernel)))

(defun ein:kernel--handle-channels-reply (kernel packet)
  (ein:log 'debug "KERNEL--HANDLE_CHANNELS-REPLY")
  (let ((channel (plist-get (ein:json-read-from-string packet) :channel)))
    (cond ((string-equal channel "iopub")
           (ein:kernel--handle-iopub-reply kernel packet))
          ((string-equal channel "shell")
           (ein:kernel--handle-shell-reply kernel packet))
          ((string-equal channel "stdin")
           (ein:kernel--handle-stdin-reply kernel packet))
          (t (ein:log 'warn "Received reply from unkown channel %s" channel)))))

(defun ein:kernel--handle-stdin-reply (kernel packet)
  (ein:log 'debug "KERNEL--HANDLE-STDIN-REPLY")
  (setf (ein:$kernel-stdin-activep kernel) t)
  (destructuring-bind
      (&key header parent_header metadata content &allow-other-keys)
      (ein:json-read-from-string packet)
    (let ((msg-type (plist-get header :msg_type))
          (msg-id (plist-get header :msg_id))
          (password (plist-get content :password)))
      (cond ((string-equal msg-type "input_request")
             (if (not (eql password :json-false))
                 (let* ((passwd (read-passwd (plist-get content :prompt)))
                        (content (list :value passwd))
                        (msg (ein:kernel--get-msg kernel "input_reply" content)))
                   (ein:websocket-send-stdin-channel kernel msg)
                   (setf (ein:$kernel-stdin-activep kernel) nil))
               (cond ((or (string-match "ipdb>" (plist-get content :prompt))
                          (string-match "(Pdb)" (plist-get content :prompt)))
                      (ein:run-ipdb-session kernel (plist-get content :prompt))))))))))

(defun ein:kernel--handle-shell-reply (kernel packet)
  (ein:log 'debug "KERNEL--HANDLE-SHELL-REPLY")
  (destructuring-bind
      (&key header content metadata parent_header &allow-other-keys)
      (ein:json-read-from-string packet)
    (let* ((msg-type (plist-get header :msg_type))
           (msg-id (plist-get parent_header :msg_id))
           (callbacks (ein:kernel-get-callbacks-for-msg kernel msg-id))
           (cb (plist-get callbacks (intern (format ":%s" msg-type)))))
      (ein:log 'debug "KERNEL--HANDLE-SHELL-REPLY: msg_type = %s" msg-type)
      (if cb
          (ein:funcall-packed cb content metadata)
        (ein:log 'debug "no callback for: msg_type=%s msg_id=%s"
                 msg-type msg-id))
      (ein:aif (plist-get content :payload)
          (ein:kernel--handle-payload kernel callbacks it))
      (let ((events (ein:$kernel-events kernel)))
        (ein:case-equal msg-type
          (("execute_reply")
           (ein:aif (plist-get content :execution_count)
               ;; It can be `nil' for silent execution
               (ein:events-trigger events 'execution_count.Kernel it)))))))
  (ein:log 'debug "KERNEL--HANDLE-SHELL-REPLY: finished"))

(defun ein:kernel--handle-payload (kernel callbacks payload)
  (loop with events = (ein:$kernel-events kernel)
        for p in payload
        for text = (if (= (ein:$kernel-api-version kernel) 2)
                       (plist-get p :text)
                     (plist-get (plist-get p :data)
                                :text/plain))
        for source = (plist-get p :source)
        if (member source '("IPython.kernel.zmq.page.page"
                            "IPython.zmq.page.page"
                            "page"))
        do (when (not (equal (ein:trim text) ""))
             (ein:events-trigger
              events 'open_with_text.Pager (list :text text)))
        else if
        (member
         source
         '("IPython.kernel.zmq.zmqshell.ZMQInteractiveShell.set_next_input"
           "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input"))
        do (let ((cb (plist-get callbacks :set_next_input)))
             (when cb (ein:funcall-packed cb text)))))

(defun ein:kernel--handle-iopub-reply (kernel packet)
  (ein:log 'debug "KERNEL--HANDLE-IOPUB-REPLY")
  (if (ein:$kernel-stdin-activep kernel)
      (ein:ipdb--handle-iopub-reply kernel packet)
    (destructuring-bind
        (&key content metadata parent_header header &allow-other-keys)
        (ein:json-read-from-string packet)
      (let* ((msg-type (plist-get header :msg_type))
             (callbacks (ein:kernel-get-callbacks-for-msg
                         kernel (plist-get parent_header :msg_id)))
             (events (ein:$kernel-events kernel)))
        (ein:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: msg_type = %s" msg-type)
        (if (and (not (equal msg-type "status")) (null callbacks))
            (ein:log 'verbose "Got message not from this notebook.")
          (ein:case-equal msg-type
            (("stream" "display_data" "pyout" "pyerr" "error" "execute_result")
             (ein:aif (plist-get callbacks :output)
                 (ein:funcall-packed it msg-type content metadata)))
            (("status")
             (ein:case-equal (plist-get content :execution_state)
               (("busy")
                (ein:events-trigger events 'status_busy.Kernel))
               (("idle")
                (ein:events-trigger events 'status_idle.Kernel))
               (("dead")
                (ein:kernel-disconnect kernel)
                (ein:events-trigger events 'status_dead.Kernel))))
            (("data_pub")
             (ein:log 'verbose (format "Received data_pub message w/content %s" packet)))
            (("clear_output")
             (ein:aif (plist-get callbacks :clear_output)
                 (ein:funcall-packed it content metadata))))))))
  (ein:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: finished"))


;;; Utility functions

(defun ein:kernel-filename-to-python (kernel filename)
  "See: `ein:filename-to-python'."
  (ein:filename-to-python (ein:$kernel-url-or-port kernel) filename))

(defun ein:kernel-filename-from-python (kernel filename)
  "See: `ein:filename-from-python'."
  (ein:filename-from-python (ein:$kernel-url-or-port kernel) filename))

(defun ein:kernel-construct-defstring (content)
  "Construct call signature from CONTENT of ``:object_info_reply``.
Used in `ein:pytools-finish-tooltip', etc."
  (or (plist-get content :call_def)
      (plist-get content :init_definition)
      (plist-get content :definition)))

(defun ein:kernel-construct-help-string (content)
  "Construct help string from CONTENT of ``:object_info_reply``.
Used in `ein:pytools-finish-tooltip', etc."
  (ein:log 'debug "KERNEL-CONSTRUCT-HELP-STRING")
  (let* ((defstring (ein:aand
                     (ein:kernel-construct-defstring content)
                     (ansi-color-apply it)
                     (ein:string-fill-paragraph it)))
         (docstring (ein:aand
                     (or (plist-get content :call_docstring)
                         (plist-get content :init_docstring)
                         (plist-get content :docstring)
                         ;; "<empty docstring>"
                         )
                     (ansi-color-apply it)))
         (help (ein:aand
                (ein:filter 'identity (list defstring docstring))
                (ein:join-str "\n" it))))
    (ein:log 'debug "KERNEL-CONSTRUCT-HELP-STRING: help=%s" help)
    help))

(defun ein:kernel-request-stream (kernel code func &optional args)
  "Run lisp callback FUNC with the output stream returned by Python CODE.

The first argument to the lisp function FUNC is the stream output
as a string and the rest of the argument is the optional ARGS."
  (ein:kernel-execute
   kernel
   code
   (list :output (cons (lambda (packed msg-type content -metadata-not-used-)
                         (let ((func (car packed))
                               (args (cdr packed)))
                           (when (equal msg-type "stream")
                             (ein:aif (plist-get content :data)
                                 (apply func it args)))))
                       (cons func args)))))

(defun* ein:kernel-history-request-synchronously
    (kernel &rest args &key (timeout 0.5) (tick-time 0.05) &allow-other-keys)
  "Send the history request and wait TIMEOUT seconds.
Return a list (CONTENT METADATA).
This function checks the request reply every TICK-TIME seconds.
See `ein:kernel-history-request' for other usable options."
  ;; As `result' and `finished' are set in callback, make sure they
  ;; won't be trapped in other let-bindings.
  (lexical-let (result finished)
    (apply
     #'ein:kernel-history-request
     kernel
     (list :history_reply
           (cons (lambda (-ignore- content metadata)
                   (setq result (list content metadata))
                   (setq finished t))
                 nil))
     args)
    (loop repeat (floor (/ timeout tick-time))
          do (sit-for tick-time)
          when finished
          return t
          finally (error "Timeout"))
    result))

(defun ein:kernel-history-search-synchronously (kernel pattern &rest args)
  "Search execution history in KERNEL using PATTERN.
Return matched history as a list of strings.
See `ein:kernel-history-request-synchronously' and
`ein:kernel-history-request' for usable options."
  (let ((reply
         (apply #'ein:kernel-history-request-synchronously
                kernel
                :hist-access-type "search"
                :pattern pattern
                args)))
    (mapcar #'caddr (plist-get (car reply) :history))))

(provide 'ein-kernel)

;;; ein-kernel.el ends here
