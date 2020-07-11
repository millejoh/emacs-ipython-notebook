;;; ein-notification.el --- Notification widget for Notebook    -*- lexical-binding:t -*-

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notification.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notification.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notification.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'eieio)

(require 'ein-core)
(require 'ein-classes)
(require 'ein-events)

(declare-function ein:get-notebook "ein:notebook")
(declare-function ein:notebook-opened-buffer-names "ein:notebook")
(declare-function ein:list-available-kernels "ein:notebook")
(declare-function ein:notebook-switch-kernel "ein:notebook")

(define-obsolete-variable-alias 'ein:@notification 'ein:%notification% "0.1.2")
(ein:deflocal ein:%notification% nil
  "Buffer local variable to hold an instance of `ein:notification'.")

(defvar ein:header-line-format '(:eval (ein:header-line)))
(defvar ein:header-line-switch-kernel-map (make-sparse-keymap))

(cl-defmethod ein:notification-status-set ((ns ein:notification-status) status)
  (let* ((message (cdr (assoc status (slot-value ns 's2m)))))
    (setf (slot-value ns 'status) status)
    (setf (slot-value ns 'message) (substitute-command-keys message))
    (force-mode-line-update t)))

(cl-defmethod ein:notification-bind-events ((notification ein:notification) events)
  "Bind a callback to events of the event handler EVENTS which
just set the status (= event-type):
    (ein:notification-status-set NS EVENT-TYPE)
where NS is `:kernel' or `:notebook' slot of NOTIFICATION."
  (cl-loop for ns in (list (slot-value notification 'kernel)
                        (slot-value notification 'notebook))
        for statuses = (mapcar #'car (slot-value ns 's2m))
        do (cl-loop for st in statuses
                 do (ein:events-on events
                                   st   ; = event-type
                                   #'ein:notification--callback
                                   (cons ns st))))
  (ein:events-on events
                 'notebook_saved.Notebook
                 #'ein:notification--fadeout-callback
                 (list (slot-value notification 'notebook)
                       "Notebook is saved"
                       'notebook_saved.Notebook
                       nil))
  (ein:events-on events
                 'execution_count.Kernel
                 #'ein:notification--set-execution-count
                 notification))

(defun ein:notification--callback (packed _data)
  (let ((ns (car packed))
        (status (cdr packed)))
    (ein:notification-status-set ns status)))

(defun ein:notification--set-execution-count (notification count)
  (setf (oref notification :execution-count) count))

(defun ein:notification--fadeout-callback (packed _data)
  ;; FIXME: I can simplify this.
  ;;        Do not pass around message, for exmaple.
  (cl-destructuring-bind (ns message status &rest) packed
    (setf (oref ns :status) status)
    (setf (oref ns :message) message)
    (apply #'run-at-time
           1 nil
           (lambda (ns _message status next)
             (when (equal (slot-value ns 'status) status)
               (ein:notification-status-set ns next)
               ;; (ein:with-live-buffer (slot-value ns :buffer)
               ;;   (force-mode-line-update))
               ))
           packed)))

(defun ein:notification-setup (buffer events &rest tab-slots)
  "Setup a new notification widget in the BUFFER.
This function saves the new notification widget instance in the
local variable of the BUFFER.

Rest of the arguments are for TABs in `header-line'.

GET-LIST : function
  Return a list of worksheets.

GET-CURRENT : function
  Return the current worksheet.

GET-NAME : function
  Return a name of the worksheet given as its argument.

\(fn buffer events &key get-list get-current)"
  (with-current-buffer buffer
    (setq ein:%notification%
          (make-instance 'ein:notification
                         :buffer buffer))
    (setq header-line-format ein:header-line-format)
    (ein:notification-bind-events ein:%notification% events)
    (setf (oref ein:%notification% :tab)
          (apply #'make-instance 'ein:notification-tab tab-slots))
    ein:%notification%))

(defface ein:notification-tab-normal
  '((t :inherit (header-line) :underline t :height 0.8))
  "Face for headline selected tab."
  :group 'ein)

(define-key ein:header-line-switch-kernel-map
  [header-line mouse-1] 'ein:header-line-switch-kernel)

(defmacro ein:with-destructuring-bind-key-event (key-event &rest body)
  (declare (debug (form &rest form))
           (indent 1))
  ;; See: (info "(elisp) Click Events")
  `(cl-destructuring-bind
       (event-type
        (window pos-or-area (x . y) timestamp
                object text-pos (col . row)
                image (dx . dy) (width . height)))
       ,key-event
     ,@body))

(defun ein:header-line-switch-kernel (_key-event)
  (interactive "e")
  (let* ((notebook (or (ein:get-notebook)
                       (ein:completing-read
                        "Select notebook: "
                        (ein:notebook-opened-buffer-names))))
         (kernel-name (ein:completing-read
                       "Select kernel: "
                       (ein:list-available-kernels (ein:$notebook-url-or-port notebook)))))
    (ein:notebook-switch-kernel notebook kernel-name)))

(defun ein:header-line ()
  (format
   "IP[%s]: %s"
   (slot-value ein:%notification% 'execution-count)
   (ein:join-str
    " | "
    (cl-remove-if-not
     #'identity
     (list (slot-value (slot-value ein:%notification% 'notebook) 'message)
           (slot-value (slot-value ein:%notification% 'kernel) 'message)
           (propertize (aif (aand (ein:get-notebook) (ein:$notebook-kernelspec it))
                           (format "|%s|" (ein:$kernelspec-name it))
                         "|unknown: please click and select a kernel|")
                       'keymap ein:header-line-switch-kernel-map
                       'help-echo "Click (mouse-1) to change the running kernel."
                       'mouse-face 'highlight
                       'face 'ein:notification-tab-normal))))))

(provide 'ein-notification)

;;; ein-notification.el ends here
