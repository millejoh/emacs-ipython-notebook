;;; ein-events.el --- Event module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-events.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-events.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-events.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ein-log)
(require 'ein-utils)
(eval-when-compile (defvar ein:notebook))

(defvar ein:header-line-format '(:eval (ein:header-line)))

(defun ein:header-line ()
  (format
   "IP[y]: %s"
   (ein:join-str
    " | "
    (ein:filter
     'identity
     (list (ein:events-header-message-notebook)
           (ein:events-header-message-kernel))))))

(defun ein:header-line-setup-maybe ()
  "Setup `header-line-format' for mumamo.
As `header-line-format' is buffer local variable, it must be set
for each chunk when in
See also `ein:ac-setup-maybe'."
  (and ein:notebook
       (and (boundp 'mumamo-multi-major-mode)
            (eval 'mumamo-multi-major-mode))
       (setq header-line-format ein:header-line-format)))
(add-hook 'after-change-major-mode-hook 'ein:header-line-setup-maybe)


;;; Events handling class

(ein:deflocal ein:@events nil
  "Buffer local variable to hold an instance of `ein:events'.")

(defclass ein:events ()
  ((buffer :initarg :buffer :type buffer :document "Notebook buffer")
   (status-notebook :initarg :status-notebook :initform nil :type symbol)
   (status-kernel :initarg :status-kernel :initform nil :type symbol))
  "Event handler class.")

(defun ein:events-setup (buffer)
  "Make a new event handler instance and setup local variable in the BUFFER.
The newly created instance is returned by this function.  Event
handler user must *not* use the buffer local variable directly.
Use the variable returned by this function instead."
  (with-current-buffer buffer
    (setq ein:@events (ein:events "Events" :buffer buffer))
    (setq header-line-format ein:header-line-format)
    ein:@events))

(defun ein:events-header-message-notebook ()
  (case (oref ein:@events :status-notebook)
    (notebook_saving.Notebook "Saving Notebook...")
    (notebook_saved.Notebook "Notebook is saved")
    (notebook_save_failed.Notebook "Failed to save Notebook!")))

(defun ein:events-header-message-kernel ()
  (case (oref ein:@events :status-kernel)
    (status_idle.Kernel nil)
    (status_busy.Kernel "Kernel is busy...")
    (status_dead.Kernel "Kernel is dead. Need restart.")))

(defun ein:events-trigger (events event-type)
  (ein:log 'debug "Event: %s" event-type)
  (case event-type
    ((status_busy.Kernel status_idle.Kernel status_dead.Kernel)
     (oset events :status-kernel event-type))
    ((notebook_saving.Notebook
      notebook_saved.Notebook
      notebook_save_failed.Notebook)
     (oset events :status-notebook event-type))
    (t
     (ein:log 'info "Unknown event: %s" event-type))))

(provide 'ein-events)

;;; ein-events.el ends here
