;;; ein-notification.el --- Notification widget for Notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

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

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ein-utils)
(require 'ein-events)


;; Class and variable

(ein:deflocal ein:@notification nil
  "Buffer local variable to hold an instance of `ein:notification'.")

(defvar ein:header-line-format '(:eval (ein:header-line)))
;; Note: can't put this below of `ein:notification-setup'...

(defclass ein:notification-status ()
  ((status :initarg :status :initform nil)
   (message :initarg :message :initform nil)
   (s2m :initarg :s2m))
  "Hold status and it's string representation (message).")

(defclass ein:notification ()
  ((buffer :initarg :buffer :type buffer :document "Notebook buffer")
   (notebook
    :initarg :notebook
    :initform
    (ein:notification-status
     "NotebookStatus"
     :s2m
     '((notebook_saving.Notebook       . "Saving Notebook...")
       (notebook_saved.Notebook        . "Notebook is saved")
       (notebook_save_failed.Notebook  . "Failed to save Notebook!")))
    :type ein:notification-status)
   (kernel
    :initarg :kernel
    :initform
    (ein:notification-status
     "KernelStatus"
     :s2m
     '((status_idle.Kernel . nil)
       (status_busy.Kernel . "Kernel is busy...")
       (status_dead.Kernel . "Kernel is dead. Need restart.")))
    :type ein:notification-status))
  "Notification widget for Notebook.")

(defmethod ein:notification-status-set ((ns ein:notification-status) status)
  (let* ((message (cdr (assoc status (oref ns :s2m)))))
    (oset ns :status status)
    (oset ns :message message)))

(defmethod ein:notification-bind-events ((notification ein:notification)
                                         events)
  "Bind a callback to events of the event handler EVENTS which
just set the status \(= event-type):
    \(ein:notification-status-set NS EVENT-TYPE)
where NS is `:kernel' or `:notebook' slot of NOTIFICATION."
  (loop for ns in (list (oref notification :kernel)
                        (oref notification :notebook))
        for statuses = (mapcar #'car (oref ns :s2m))
        do (loop for st in statuses
                 do (ein:events-on events
                                   st   ; = event-type
                                   #'ein:notification--callback
                                   (cons ns st))))
  (ein:events-on events
                 'status_restarting.Kernel
                 #'ein:notification--fadeout-callback
                 (list (oref notification :kernel)
                       "Restarting kernel..."
                       'status_restarting.Kernel
                       'status_idle.Kernel)))

(defun ein:notification--callback (packed data)
  (let ((ns (car packed))
        (status (cdr packed)))
    (ein:notification-status-set ns status)))

(defun ein:notification--fadeout-callback (packed data)
  (let ((ns (nth 0 packed))
        (message (nth 1 packed))
        (status (nth 2 packed))
        (next (nth 3 packed)))
    (oset ns :status status)
    (oset ns :message message)
    (apply #'run-at-time
           1 nil
           (lambda (ns message status next)
             (when (equal (oref ns :status) status)
               (ein:notification-status-set ns next)
               (force-mode-line-update)))
           packed)))

(defun ein:notification-setup (buffer)
  "Setup a new notification widget in the BUFFER.
This function saves the new notification widget instance in the
local variable of the BUFFER"
  (with-current-buffer buffer
    (setq ein:@notification
          (ein:notification "NotificationWidget" :buffer buffer))
    (setq header-line-format ein:header-line-format)
    ein:@notification))


;;; Header line

(defun ein:header-line ()
  (format
   "IP[y]: %s"
   (ein:join-str
    " | "
    (ein:filter
     'identity
     (list (oref (oref ein:@notification :notebook) :message)
           (oref (oref ein:@notification :kernel) :message))))))

(defun ein:header-line-setup-maybe ()
  "Setup `header-line-format' for mumamo.
As `header-line-format' is buffer local variable, it must be set
for each chunk when in
See also `ein:ac-setup-maybe'."
  (and (ein:eval-if-bound 'ein:notebook)
       (ein:eval-if-bound 'mumamo-multi-major-mode)
       (setq header-line-format ein:header-line-format)))
(add-hook 'after-change-major-mode-hook 'ein:header-line-setup-maybe)

(provide 'ein-notification)

;;; ein-notification.el ends here
