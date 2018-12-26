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
;; `ein:kernel' is the proxy class of notebook server state.
;; It agglomerates both the "kernel" and "session" objects of server described here
;; https://github.com/jupyter/jupyter/wiki/Jupyter-Notebook-Server-API
;; It may have been better to keep them separate to allow parallel reasoning with
;; the notebook server, but that time is past.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ansi-color)

(require 'ein-core)
(require 'ein-classes)
(require 'ein-log)
;; FIXME: use websocket.el directly once v1.0 is released.
(require 'ein-websocket)
(require 'ein-events)
(require 'ein-query)
(require 'ein-ipdb)
;; "Public" getters.  Use them outside of this package.

(defun ein:$kernel-session-url (kernel)
  (concat "/api/sessions/" (ein:$kernel-session-id kernel)))

;;;###autoload
(defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port)

;;;###autoload
(defalias 'ein:kernel-id 'ein:$kernel-kernel-id)

(defcustom ein:pre-kernel-execute-functions nil
  "List of functions to call before sending a message to the kernel for execution. Each function is called with the message (see `ein:kernel--get-msg') about to be sent."
  :type 'list
  :group 'ein)

(defcustom ein:on-shell-reply-functions nil
  "List of functions to call when the kernel responds on the shell channel.
  Each function should have the call signature: msg-id header content metadata"
  :type 'list
  :group 'ein)



;;; Initialization and connection.

(defun ein:kernel-new (url-or-port path kernelspec base-url events &optional api-version)
  (make-ein:$kernel
   :url-or-port url-or-port
   :path path
   :kernelspec kernelspec
   :events events
   :api-version (or api-version 5)
   :session-id (ein:utils-uuid)
   :kernel-id nil
   :websocket nil
   :base-url base-url
   :stdin-activep nil
   :oinfo-cache (make-hash-table :test #'equal)
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
            :version "5.0"
            :date (format-time-string "%Y-%m-%dT%T" (current-time)) ; ISO 8601 timestamp
            :msg_type msg-type)
   :metadata (make-hash-table)
   :content content
   :parent_header (make-hash-table)))

(defun* ein:kernel-session-p (kernel callback &optional iteration)
  "Don't make any changes on the server side.  CALLBACK with arity 2, kernel and a boolean whether session exists on server."
  (unless iteration
    (setq iteration 0))
  (let ((session-id (ein:$kernel-session-id kernel)))
    (ein:query-singleton-ajax
     (list 'kernel-session-p session-id)
     (ein:url (ein:$kernel-url-or-port kernel) "api/sessions" session-id)
     :type "GET"
     :sync ein:force-sync
     :parser #'ein:json-read
     :complete (apply-partially #'ein:kernel-session-p--complete session-id)
     :success (apply-partially #'ein:kernel-session-p--success kernel session-id callback)
     :error (apply-partially #'ein:kernel-session-p--error kernel callback iteration))))

(defun* ein:kernel-session-p--complete (session-id &key data response
                                                   &allow-other-keys
                                                   &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:kernel-session-p--complete %s" resp-string))

(defun* ein:kernel-session-p--error (kernel callback iteration &key error-thrown symbol-status data &allow-other-keys)
  (if (ein:aand (plist-get data :message) (search "not found" it))
      (when callback (funcall callback kernel nil))
    (let* ((max-tries 3)
           (tries-left (1- (- max-tries iteration))))
      (ein:log 'verbose "ein:kernel-session-p--error [%s], %s tries left"
               (car error-thrown) tries-left)
      (if (> tries-left 0)
          (ein:kernel-session-p kernel callback (1+ iteration))))))

(defun* ein:kernel-session-p--success (kernel session-id callback &key data &allow-other-keys)
  (let ((session-p (equal (plist-get data :id) session-id)))
    (ein:log 'verbose "ein:kernel-session-p--success: session-id=%s session-p=%s"
             session-id session-p)
    (when callback (funcall callback kernel session-p))))

(defun* ein:kernel-restart-session (kernel)
  "Server side delete of KERNEL session and subsequent restart with all new state"
  (ein:kernel-delete-session
   kernel
   (lambda (kernel)
     (ein:events-trigger (ein:$kernel-events kernel) 'status_restarting.Kernel)
     (ein:kernel-retrieve-session kernel 0
                                  (lambda (kernel)
                                    (ein:events-trigger (ein:$kernel-events kernel)
                                                        'status_restarted.Kernel))))))

(defun* ein:kernel-retrieve-session (kernel &optional iteration callback)
  "Formerly ein:kernel-start, but that was misnomer because 1. the server really starts a session (and an accompanying kernel), and 2. it may not even start a session if one exists for the same path.

If 'picking up from where we last left off', that is, we restart emacs and reconnect to same server, jupyter will hand us back the original, still running session.

The server logic is here (could not find other documentation)
https://github.com/jupyter/notebook/blob/04a686dbaf9dfe553324a03cb9e6f778cf1e3da1/notebook/services/sessions/handlers.py#L56-L81

CALLBACK of arity 1, the kernel.
"
  (unless iteration
    (setq iteration 0))
  (if (<= (ein:$kernel-api-version kernel) 2)
      (error "Api %s unsupported" (ein:$kernel-api-version kernel))
    (let ((kernel-id (ein:$kernel-kernel-id kernel))
          (kernelspec (ein:$kernel-kernelspec kernel))
          (path (ein:$kernel-path kernel)))
      (ein:query-singleton-ajax
       (list 'kernel-retrieve-session kernel-id)
       (ein:url (ein:$kernel-url-or-port kernel) "api/sessions")
       :type "POST"
       :data (json-encode
              (cond ((<= (ein:$kernel-api-version kernel) 4)
                     `(("notebook" .
                        (("path" . ,path)))
                       ,@(if kernelspec
                             `(("kernel" .
                                (("name" . ,(ein:$kernelspec-name kernelspec))))))))
                    (t `(("path" . ,path)
                         ("type" . "notebook")
                         ,@(if kernelspec
                               `(("kernel" .
                                  (("name" . ,(ein:$kernelspec-name kernelspec))
                                   ,@(if kernel-id
                                         `(("id" . ,kernel-id)))))))))))
       :sync ein:force-sync
       :parser #'ein:json-read
       :complete (apply-partially #'ein:kernel-retrieve-session--complete kernel callback)
       :success (apply-partially #'ein:kernel-retrieve-session--success kernel callback)
       :error (apply-partially #'ein:kernel-retrieve-session--error kernel iteration callback)))))

(defun* ein:kernel-retrieve-session--complete (kernel callback &key data response
                                           &allow-other-keys
                                           &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:kernel-retrieve-session--complete %s" resp-string))

(defun* ein:kernel-retrieve-session--error (kernel iteration callback &key error-thrown symbol-status &allow-other-keys)
  (let* ((max-tries 3)
         (tries-left (1- (- max-tries iteration))))
    (ein:log 'verbose "ein:kernel-retrieve-session--error [%s], %s tries left"
             (car error-thrown) tries-left)
    (sleep-for 0 (* (1+ iteration) 500))
    (if (> tries-left 0)
        (ein:kernel-retrieve-session kernel (1+ iteration) callback))))

(defun* ein:kernel-retrieve-session--success (kernel callback &key data &allow-other-keys)
  (let ((session-id (plist-get data :id)))
    (if (plist-get data :kernel)
        (setq data (plist-get data :kernel)))
    (destructuring-bind (&key id &allow-other-keys) data
      (ein:log 'verbose "ein:kernel-retrieve-session--success: kernel-id=%s session-id=%s"
               id session-id)
      (setf (ein:$kernel-kernel-id kernel) id)
      (setf (ein:$kernel-session-id kernel) session-id)
      (setf (ein:$kernel-ws-url kernel) (ein:kernel--ws-url (ein:$kernel-url-or-port kernel)))
      (setf (ein:$kernel-kernel-url kernel)
            (concat (file-name-as-directory (ein:$kernel-base-url kernel)) id)))
    (ein:kernel-start-websocket kernel callback)))

(defun ein:kernel-reconnect-session (kernel &optional callback)
  "Check if session still exists.  If it does, retrieve it.  If it doesn't, ask the user to create a new session (ein:kernel-retrieve-session both retrieves and creates).

CALLBACK with arity 0 (e.g., execute cell now that we're reconnected)"
  (ein:kernel-disconnect kernel)
  (ein:kernel-session-p
   kernel
   (apply-partially
    (lambda (callback* kernel session-p)
      (when (or session-p
                (and (not noninteractive) (y-or-n-p "Session not found.  Restart?")))
        (ein:events-trigger (ein:$kernel-events kernel) 'status_reconnecting.Kernel)
        (ein:kernel-retrieve-session
         kernel 0
         (apply-partially
          (lambda (callback** kernel)
            (ein:events-trigger (ein:$kernel-events kernel)
                                'status_reconnected.Kernel)
            (when callback** (funcall callback** kernel)))
          callback*))))
    callback)))

(defun ein:kernel--ws-url (url-or-port)
  "Assuming URL-OR-PORT already normalized by `ein:url'

See https://github.com/ipython/ipython/pull/3307"
  (let* ((parsed-url (url-generic-parse-url url-or-port))
         (protocol (if (string= (url-type parsed-url) "https") "wss" "ws")))
    (format "%s://%s:%s%s"
            protocol
            (url-host parsed-url)
            (url-port parsed-url)
            (url-filename parsed-url))))

(defun ein:kernel-send-cookie (channel host)
  ;; cookie can be an empty string for IPython server with no password,
  ;; but something must be sent to start channel.
  (let ((cookie (ein:query-get-cookie host "/")))
    (ein:websocket-send channel cookie)))

(defun ein:kernel--handle-websocket-reply (kernel ws frame)
  (ein:and-let* ((packet (websocket-frame-payload frame))
                 (channel (plist-get (ein:json-read-from-string packet) :channel)))
    (cond ((string-equal channel "iopub")
           (ein:kernel--handle-iopub-reply kernel packet))
          ((string-equal channel "shell")
           (ein:kernel--handle-shell-reply kernel packet))
          ((string-equal channel "stdin")
           (ein:kernel--handle-stdin-reply kernel packet))
          (t (ein:log 'warn "Received reply from unforeseen channel %s" channel)))))

(defun ein:start-single-websocket (kernel open-callback)
  "OPEN-CALLBACK (kernel) (e.g., execute cell)"
  (let ((ws-url (concat (ein:$kernel-ws-url kernel)
                         (ein:$kernel-kernel-url kernel)
                         "/channels?session_id="
                         (ein:$kernel-session-id kernel))))
    (ein:log 'verbose "WS start: %s" ws-url)
    (setf (ein:$kernel-websocket kernel)
          (ein:websocket ws-url kernel
                         (apply-partially #'ein:kernel--handle-websocket-reply kernel)
                         (lambda (ws)
                           (let* ((websocket (websocket-client-data ws))
                                  (kernel (ein:$websocket-kernel websocket)))
                             (unless (ein:$websocket-closed-by-client websocket)
                               (ein:log 'verbose "WS closed unexpectedly: %s" (websocket-url ws))
                               (ein:kernel-disconnect kernel))))
                         (apply-partially
                          (lambda (cb ws)
                            (let* ((websocket (websocket-client-data ws))
                                   (kernel (ein:$websocket-kernel websocket)))
                              (when (ein:kernel-live-p kernel)
                                (ein:kernel-run-after-start-hook kernel)
                                (when cb
                                  (funcall cb kernel)))
                              (ein:log 'verbose "WS opened: %s" (websocket-url ws))))
                          open-callback)))))

(defun ein:kernel-start-websocket (kernel callback)
  (cond ((<= (ein:$kernel-api-version kernel) 2)
         (error "Api version %s unsupported" (ein:$kernel-api-version kernel)))
        (t (ein:start-single-websocket kernel callback))))

(defun ein:kernel-on-connect (kernel content -metadata-not-used-)
  (ein:log 'info "Kernel connect_request_reply received."))

(defun ein:kernel-run-after-start-hook (kernel)
  (ein:log 'debug "EIN:KERNEL-RUN-AFTER-START-HOOK")
  (mapc #'ein:funcall-packed
        (ein:$kernel-after-start-hook kernel)))

(defun ein:kernel-disconnect (kernel)
  "Close websocket connection to running kernel, but do not
delete the kernel on the server side"
  (ein:events-trigger (ein:$kernel-events kernel) 'status_disconnected.Kernel)
  (ein:aif (ein:$kernel-websocket kernel)
      (progn (ein:websocket-close it)
             (setf (ein:$kernel-websocket kernel) nil))))

(defun ein:kernel-live-p (kernel)
  (and (ein:$kernel-p kernel)
       (ein:aand (ein:$kernel-websocket kernel) (ein:websocket-open-p it))))

(defun ein:kernel-when-ready (kernel callback)
  "Execute CALLBACK of arity 0 (executing cell) when KERNEL is ready.  Warn user otherwise."
  (if (ein:kernel-live-p kernel)
      (funcall callback kernel)
    (ein:log 'verbose "Kernel %s unavailable" (ein:$kernel-kernel-id kernel))
    (ein:kernel-reconnect-session kernel callback)))


;;; Main public methods

;; NOTE: The argument CALLBACKS for the following functions is almost
;;       same as the JS implementation in IPython.  However, as Emacs
;;       lisp does not support closure, value is "packed" using
;;       `cons': `car' is the actual callback function and `cdr' is
;;       its first argument.  It's like using `cons' instead of
;;       `$.proxy'.

(defun ein:kernel-object-info-request (kernel objname callbacks &optional cursor-pos detail-level)
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
    (if (<= (ein:$kernel-api-version kernel) 2)
        (error "Api version %s unsupported" (ein:$kernel-api-version kernel)))
    (let* ((content (if (< (ein:$kernel-api-version kernel) 5)
                        (list
                         ;; :text ""
                         :oname (format "%s" objname)
                         :cursor_pos (or cursor-pos 0)
                         :detail_level (or detail-level 0))
                      (list
                       :code (format "%s" objname)
                       :cursor_pos (or cursor-pos 0)
                       :detail_level (or detail-level 0))))
           (msg (ein:kernel--get-msg kernel "inspect_request"
                                     (append content (list :detail_level 1))))
           (msg-id (plist-get (plist-get msg :header) :msg_id)))
      (ein:websocket-send-shell-channel kernel msg)
      (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks))))

(defun* ein:kernel-execute (kernel code &optional callbacks
                                   &key
                                   (silent t)
                                   (store-history t)
                                   (user-expressions (make-hash-table))
                                   (allow-stdin t)
                                   (stop-on-error nil))
  "Execute CODE on KERNEL.

When calling this method pass a CALLBACKS structure of the form:

  (:execute_reply  EXECUTE-REPLY-CALLBACK
   :output         OUTPUT-CALLBACK
   :clear_output   CLEAR-OUTPUT-CALLBACK
   :set_next_input SET-NEXT-INPUT)

Right hand sides ending -CALLBACK above must cons a FUNCTION and its
`packed' ARGUMENT which is a sublist of args:

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
  (let* ((content (list
                   :code code
                   :silent (or silent json-false)
                   :store_history (or store-history json-false)
                   :user_expressions user-expressions
                   :allow_stdin allow-stdin
                   :stop_on_error (or stop-on-error json-false)))
         (msg (ein:kernel--get-msg kernel "execute_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:log 'debug "KERNEL-EXECUTE: code=%s msg_id=%s" code msg-id)
    (run-hook-with-args 'ein:pre-kernel-execute-functions msg)
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    (unless silent
      (mapc #'ein:funcall-packed
            (ein:$kernel-after-execute-hook kernel)))
    msg-id))

(defun ein:kernel-complete (kernel line cursor-pos callbacks errback)
  "Complete code at CURSOR-POS in a string LINE on KERNEL.

CURSOR-POS is the position in the string LINE, not in the buffer.

ERRBACK takes a string (error message).

When calling this method pass a CALLBACKS structure of the form:

    (:complete_reply (FUNCTION . ARGUMENT))

Call signature::

  (funcall FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `complete_reply' message.

`complete_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#complete
"
  (condition-case err
      (let* ((content (if (< (ein:$kernel-api-version kernel) 5)
                          (list
                           ;; :text ""
                           :line line
                           :cursor_pos cursor-pos)
                        (list
                         :code line
                         :cursor_pos cursor-pos)))
             (msg (ein:kernel--get-msg kernel "complete_request" content))
             (msg-id (plist-get (plist-get msg :header) :msg_id)))
        (assert (ein:kernel-live-p kernel) nil "kernel not live")
        (ein:websocket-send-shell-channel kernel msg)
        (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
        msg-id)
    (error (if errback (funcall errback (error-message-string err))
             (ein:display-warning (error-message-string err) :error)))))


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
  (ein:log 'debug "EIN:KERNEL-KERNEL-INFO-REQUEST: Sending request.")
  (let* ((msg (ein:kernel--get-msg kernel "kernel_info_request" nil))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein:websocket-send-shell-channel kernel msg)
    (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))

(defun ein:kernel-interrupt (kernel)
  (when (ein:kernel-live-p kernel)
    (ein:log 'info "Interrupting kernel")
    (ein:query-singleton-ajax
     (list 'kernel-interrupt (ein:$kernel-kernel-id kernel))
     (ein:url (ein:$kernel-url-or-port kernel)
              (ein:$kernel-kernel-url kernel)
              "interrupt")
     :type "POST"
     :success (lambda (&rest ignore)
                (ein:log 'info "Sent interruption command.")))))

(defun ein:kernel-delete--from-session-id (url session-id &optional callback)
  "Stop/delete a running kernel from a session id. May also specify a callback function of 0 args to be called once oepration is complete.

We need this to have proper behavior for the 'Stop' command in the ein:notebooklist buffer."
  (ein:query-singleton-ajax
   (list 'kernel-delete-session session-id)
   (ein:url url "api/sessions" session-id)
   :success (apply-partially #'ein:kernel-delete--from-session-complete session-id callback)
   :error (apply-partially #'ein:kernel-delete--from-session-error session-id)
   :type "DELETE"))

(defun ein:kernel-delete--from-session-complete (session-id callback &rest _)
  (ein:log 'info "Deleted session %s and its associated kernel process." session-id)
  (when callback
    (funcall callback)))

(defun ein:kernel-delete--from-session-error (session-id &rest _)
  (ein:log 'info "Error, could not delete session %s." session-id))

(defun ein:kernel-delete-session (kernel &optional callback)
  "Regardless of success or error, we clear all state variables of kernel and funcall CALLBACK (kernel)"
  (ein:and-let* ((session-id (ein:$kernel-session-id kernel)))
    (ein:query-singleton-ajax
     (list 'kernel-delete-session session-id)
     (ein:url (ein:$kernel-url-or-port kernel) "api/sessions" session-id)
     :type "DELETE"
     :complete (apply-partially #'ein:kernel-delete-session--complete kernel session-id callback)
     :error (apply-partially #'ein:kernel-delete-session--error session-id callback)
     :success (apply-partially #'ein:kernel-delete-session--success session-id callback))))

(defun* ein:kernel-delete-session--error (session-id callback
                                             &key response error-thrown
                                             &allow-other-keys)
  (ein:log 'error "ein:kernel-delete-session--error %s: ERROR %s DATA %s"
           session-id (car error-thrown) (cdr error-thrown)))

(defun* ein:kernel-delete-session--success (session-id callback &key data symbol-status response
                                               &allow-other-keys)
  (ein:log 'verbose "ein:kernel-delete-session--success: %s deleted" session-id))

(defun* ein:kernel-delete-session--complete (kernel session-id callback &key data response
                                            &allow-other-keys
                                            &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:kernel-delete-session--complete %s" resp-string)
  (ein:kernel-disconnect kernel)
  (when callback (funcall callback kernel)))


;; Reply handlers.
(defun ein:kernel-get-callbacks-for-msg (kernel msg-id)
  (gethash msg-id (ein:$kernel-msg-callbacks kernel)))

(defun ein:kernel-set-callbacks-for-msg (kernel msg-id callbacks)
  (puthash msg-id callbacks (ein:$kernel-msg-callbacks kernel)))

(defun ein:kernel--handle-stdin-reply (kernel packet)
  (ein:log 'debug "KERNEL--HANDLE-STDIN-REPLY")
  (setf (ein:$kernel-stdin-activep kernel) t)
  (destructuring-bind
      (&key header parent_header metadata content &allow-other-keys)
      (ein:json-read-from-string packet)
    (let ((msg-type (plist-get header :msg_type))
          (msg-id (plist-get header :msg_id))
          (password (plist-get content :password)))
      (ein:log 'debug "KERNEL--HANDLE-STDIN-REPLY: msg_type=%s msg_id=%s"
               msg-type msg-id)
      (cond ((string-equal msg-type "input_request")
             (if (not (eql password :json-false))
                 (let* ((passwd (read-passwd (plist-get content :prompt)))
                        (content (list :value passwd))
                        (msg (ein:kernel--get-msg kernel "input_reply" content)))
                   (ein:websocket-send-stdin-channel kernel msg)
                   (setf (ein:$kernel-stdin-activep kernel) nil))
               (cond ((string-match "ipdb>" (plist-get content :prompt)) (ein:run-ipdb-session kernel "ipdb> "))
                     ((string-match "(Pdb)" (plist-get content :prompt)) (ein:run-ipdb-session kernel "(Pdb) "))
                     (t (let* ((in (read-string (plist-get content :prompt)))
                               (content (list :value in))
                               (msg (ein:kernel--get-msg kernel "input_reply" content)))
                          (ein:websocket-send-stdin-channel kernel msg)
                          (setf (ein:$kernel-stdin-activep kernel) nil))))))))))

(defun ein:kernel--handle-shell-reply (kernel packet)
  (ein:log 'debug "KERNEL--HANDLE-SHELL-REPLY")
  (destructuring-bind
      (&key header content metadata parent_header &allow-other-keys)
      (ein:json-read-from-string packet)
    (let* ((msg-type (plist-get header :msg_type))
           (msg-id (plist-get parent_header :msg_id))
           (callbacks (ein:kernel-get-callbacks-for-msg kernel msg-id))
           (cb (plist-get callbacks (intern (format ":%s" msg-type)))))
      (ein:log 'debug "KERNEL--HANDLE-SHELL-REPLY: msg_type=%s msg_id=%s"
               msg-type msg-id)
      (run-hook-with-args 'ein:on-shell-reply-functions msg-type header content metadata)
      (ein:aif cb (ein:funcall-packed it content metadata))
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
        for text = (or (plist-get p :text)
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
           "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input"
           "set_next_input"))
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
             (msg-id (plist-get parent_header :msg_id))
             (callbacks (ein:kernel-get-callbacks-for-msg kernel msg-id))
             (events (ein:$kernel-events kernel)))
        (ein:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: msg_type=%s msg_id=%s"
                 msg-type msg-id)
        (if (and (not (equal msg-type "status")) (null callbacks))
            (ein:log 'verbose "Not processing msg_type=%s msg_id=%s" msg-type msg-id)
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
                (ein:kernel-disconnect kernel))))
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
                             (ein:aif (plist-get content :text)
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
