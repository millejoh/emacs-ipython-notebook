;;; ein-ipdb.el --- Support ipython debugger (ipdb)

;; Copyright (C) 2015 - John Miller

;; Author: John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-ipdb.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ipdb.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar *ein:ipdb-sessions* (make-hash-table))

(defstruct ein:$ipdb-session
  buffer
  kernel
  current-payload)

(defun ein:find-or-create-ipdb-session (kernel)
  (ein:aif (gethash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*)
      it
    (let ((db-session (make-ein:$ipdb-session
                       :buffer (ein:prepare-ipdb-session (ein:$kernel-kernel-id kernel))
                       :kernel kernel
                       :current-payload "Debugger started.")))
      (setf (gethash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*) db-session)
      db-session)))

(defun ein:stop-ipdb-session (db-session)
  (let ((kernel (ein:$ipdb-session-kernel db-session))
        (buffer (ein:$ipdb-session-buffer db-session)))
    (kill-buffer buffer)
    (setf (ein:$kernel-stdin-activep kernel) nil)
    (remhash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*)))

(defun ein:run-ipdb-session (kernel packet)
  (let ((db-session (ein:find-or-create-ipdb-session kernel)))
    (ein:ipdb-render db-session)
    (let* ((command (read-from-minibuffer "ipdb> "))
           (content (list :value command))
           (msg (ein:kernel--get-msg kernel "input_reply" content)))
      ;(plist-put (plist-get msg :header) :msg_id (plist-get (plist-get packet :header) :msg_id))
      (when (or (string= command "q")
                (string= command "quit"))
        (ein:stop-ipdb-session db-session))
      (ein:websocket-send-stdin-channel kernel msg))))

(defun ein:prepare-ipdb-session (id)
  (let ((buffer (get-buffer-create (format "*ipdb: %s*" id))))
    (switch-to-buffer buffer)
    buffer))

(defun ein:ipdb--handle-iopub-reply (kernel packet)
  (destructuring-bind
      (&key content metadata parent_header header &allow-other-keys)
      (ein:json-read-from-string packet)
    (let ((msg-type (plist-get header :msg_type)))
      (if (string-equal msg-type "stream")
          (let ((session (ein:find-or-create-ipdb-session kernel)))
            (setf (ein:$ipdb-session-current-payload session)
                  (plist-get content :text)))))))

(defun ein:ipdb-render (session)
  (with-current-buffer (ein:$ipdb-session-buffer session)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    ;; Fill the ipdb buffer
    (widget-insert "IPython Debugger\n\n")
    (widget-insert (ein:$ipdb-session-current-payload session))
    (widget-setup)))



(provide 'ein-ipdb)
