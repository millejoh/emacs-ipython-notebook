;;; ein-kernel.el --- Communicate with IPython notebook server

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

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

(require 'ein-log)
(require 'ein-utils)
(require 'ein-websocket)
(require 'url)


(defstruct ein:$kernel
  "Hold kernel variables.

`ein:$kernel-url-or-port'
  URL or port of IPython server.

FIXME: document other slots."
  url-or-port
  kernel-id
  shell-channel
  iopub-channel
  base-url
  kernel-url
  ws-url
  running
  username
  session-id)

(defvar ein:base-kernel-url "/")
;; Currently there is no way to know this setting.  Maybe I should ask
;; IPython developers for an API to get this from notebook server.


(defun ein:kernel-new (url-or-port)
  (make-ein:$kernel
   :url-or-port url-or-port
   :kernel-id nil
   :shell-channel nil
   :iopub-channel nil
   :base-url (concat ein:base-kernel-url "kernels")
   :running nil
   :username "username"
   :session-id (ein:utils-uuid)))


(defun ein:kernel-get-msg (kernel msg-type content)
  (list
   :header (list
            :msg_id (ein:utils-uuid)
            :username (ein:$kernel-username kernel)
            :session (ein:$kernel-session-id kernel)
            :msg_type msg-type)
   :content content
   :parent_header (make-hash-table)))


(defun ein:kernel-start (kernel notebook-id callback &optional cbargs)
  "Start kernel of the notebook whose id is NOTEBOOK-ID.
CALLBACK is called after kernel is started with optional argument CBARGS."
  (unless (ein:$kernel-running kernel)
    (let* ((qs (format "notebook=%s" notebook-id))
           (url (concat (ein:url (ein:$kernel-url-or-port kernel)
                                 (ein:$kernel-base-url kernel))
                        "?" qs))
           (url-request-method "POST"))
      (url-retrieve
       url
       (lambda (status kernel callback cbargs)
         (ein:kernel--handle-start-kernel kernel (ein:json-read) callback
                                          cbargs)
         (kill-buffer (current-buffer)))
       (list kernel callback cbargs)))))


(defun ein:kernel-restart (kernel callback &optional cbargs)
  (ein:log 'info "Restarting kernel")
  (when (ein:$kernel-running kernel)
    (ein:kernel-stop-channels kernel)
    (let ((url (ein:url (ein:$kernel-url-or-port kernel)
                        (ein:$kernel-kernel-url kernel)
                        "restart")))
      (url-retrieve
       url
       (lambda (status kernel callback cbargs)
         (ein:kernel--handle-start-kernel kernel (ein:json-read) callback
                                          cbargs)
         (kill-buffer (current-buffer)))
       (list kernel callback cbargs)))))


(defun ein:kernel--handle-start-kernel (kernel json callback &optional cbargs)
  (setf (ein:$kernel-running kernel) t)
  (setf (ein:$kernel-kernel-id kernel) (plist-get json :kernel_id))
  (setf (ein:$kernel-ws-url kernel) (plist-get json :ws_url))
  (setf (ein:$kernel-kernel-url kernel)
        (concat (ein:$kernel-base-url kernel) "/"
                (ein:$kernel-kernel-id kernel)))
  (ein:kernel-start-channels kernel)
  (apply callback cbargs))


(defun ein:kernel--websocket-closed (kernel ws-url early)
  ;; FIXME: `message' is not good choice for showing multiple line
  ;; message.  I should implement ein-pager.el first and show the
  ;; message using its function.
  (if early
      (ein:log 'warn
       "Websocket connection to %s could not be established. \
You will NOT be able to run code. \
Your websocket.el may not be compatible with the websocket version in \
the server, or if the url does not look right, there could be an \
error in the server's configuration." ws-url)
    (ein:log 'warn "Websocket connection closed unexpectedly. \
The kernel will no longer be responsive.")))


(defun ein:kernel-send-cookie (channel)
  ;; This is required to open channel.  In IPython's kernel.js, it sends
  ;; `document.cookie'.  This is an empty string anyway.
  (ein:websocket-send channel ""))


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


(defun ein:kernel-start-channels (kernel)
    (ein:kernel-stop-channels kernel)
    (let* ((ws-url (concat (ein:$kernel-ws-url kernel)
                           (ein:$kernel-kernel-url kernel)))
           (onclose-arg (list :ws-url ws-url
                              :already-called-onclose nil
                              :early t)))
      (ein:log 'info "Starting WS: %S" ws-url)
      (setf (ein:$kernel-shell-channel kernel)
            (ein:websocket (concat ws-url "/shell")))
      (setf (ein:$kernel-iopub-channel kernel)
            (ein:websocket (concat ws-url "/iopub")))

      (loop for c in (list (ein:$kernel-shell-channel kernel)
                           (ein:$kernel-iopub-channel kernel))
            do (setf (ein:$websocket-onclose-args c) (list kernel onclose-arg))
            do (setf (ein:$websocket-onopen c)
                     (lexical-let ((channel c))
                       (lambda () (ein:kernel-send-cookie channel))))
            do (setf (ein:$websocket-onclose c)
                     #'ein:kernel--ws-closed-callback))

      ;; switch from early-close to late-close message after 1s
      (run-at-time
       1 nil
       (lambda (onclose-arg)
         (plist-put onclose-arg :early nil)
         (ein:log 'debug "(via run-at-time) onclose-arg changed to: %S"
                  onclose-arg))
       onclose-arg)))

;; NOTE: `onclose-arg' can be accessed as:
;; (nth 1 (ein:$websocket-onclose-args (ein:$kernel-shell-channel (ein:$notebook-kernel ein:notebook))))


(defun ein:kernel-stop-channels (kernel)
  (when (ein:$kernel-shell-channel kernel)
    (setf (ein:$websocket-onclose (ein:$kernel-shell-channel kernel)) nil)
    (ein:websocket-close (ein:$kernel-shell-channel kernel))
    (setf (ein:$kernel-shell-channel kernel) nil))
  (when (ein:$kernel-iopub-channel kernel)
    (setf (ein:$websocket-onclose (ein:$kernel-iopub-channel kernel)) nil)
    (ein:websocket-close (ein:$kernel-iopub-channel kernel))
    (setf (ein:$kernel-iopub-channel kernel) nil)))


(defun ein:kernel-ready-p (kernel)
  (and (ein:websocket-open-p (ein:$kernel-shell-channel kernel))
       (ein:websocket-open-p (ein:$kernel-iopub-channel kernel))))


(defmacro ein:kernel-if-ready (kernel &rest body)
  "Execute BODY if KERNEL is ready.  Warn user otherwise."
  (declare (indent 1))
  `(if (ein:kernel-ready-p ,kernel)
       (progn ,@body)
     (ein:log 'warn "Kernel is not ready yet!")))


(defun ein:kernel-object-info-request (kernel objname)
  "Send object info request of OBJNAME to KERNEL and return
`msg-id' or `nil' when OBJNAME is `nil'."
  (assert (ein:kernel-ready-p kernel))
  (when objname
    (let* ((content (list :oname (format "%s" objname)))
           (msg (ein:kernel-get-msg kernel "object_info_request" content)))
      (ein:websocket-send
       (ein:$kernel-shell-channel kernel)
       (json-encode msg))
      (plist-get (plist-get msg :header) :msg_id))))


(defun ein:kernel-execute (kernel code)
  (assert (ein:kernel-ready-p kernel))
  (let* ((content (list
                   :code code
                   :silent json-false
                   :user_variables []
                   :user_expressions (make-hash-table)
                   :allow_stdin json-false))
         (msg (ein:kernel-get-msg kernel "execute_request" content)))
    (ein:websocket-send
     (ein:$kernel-shell-channel kernel)
     (json-encode msg))
    (plist-get (plist-get msg :header) :msg_id)))


(defun ein:kernel-complete (kernel line cursor-pos)
  (assert (ein:kernel-ready-p kernel))
  (let* ((content (list
                   :text ""
                   :line line
                   :cursor_pos cursor-pos))
         (msg (ein:kernel-get-msg kernel "complete_request" content)))
    (ein:websocket-send
     (ein:$kernel-shell-channel kernel)
     (json-encode msg))
    (plist-get (plist-get msg :header) :msg_id)))


(defun ein:kernel-interrupt (kernel)
  (when (ein:$kernel-running kernel)
    (ein:log 'info "Interrupting kernel")
    (let ((url (ein:url (ein:$kernel-url-or-port kernel)
                        (ein:$kernel-kernel-url kernel)
                        "interrupt"))
          (url-request-method "POST"))
    (url-retrieve
     url
     (lambda (s)
       (ein:log 'info "Sent interruption command.")
       (kill-buffer (current-buffer)))))))


(defun ein:kernel-kill (kernel)
  (when (ein:$kernel-running kernel)
    (setf (ein:$kernel-running kernel) nil)
    (let ((url-request-method "DELETE")
          (url (ein:url-no-cache
                (ein:url (ein:$kernel-url-or-port kernel)
                         (ein:$kernel-kernel-url kernel)))))
      (url-retrieve
       url
       (lambda (s)
         (ein:log 'info "Notebook kernel is killed")
         (kill-buffer (current-buffer)))))))


(provide 'ein-kernel)

;;; ein-kernel.el ends here
