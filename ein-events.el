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

(require 'ein-log)
(require 'ein-utils)

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

(ein:deflocal ein:events-status-notebook nil)
(ein:deflocal ein:events-status-kernel nil)

(defun ein:events-header-message-notebook ()
  (case ein:events-status-notebook
    (notebook_saving.Notebook "Saving Notebook...")
    (notebook_saved.Notebook "Notebook is saved")
    (notebook_save_failed.Notebook "Failed to save Notebook!")))

(defun ein:events-header-message-kernel ()
  (case ein:events-status-kernel
    (status_idle.Kernel nil)
    (status_busy.Kernel "Kernel is busy...")))

(defun ein:events-trigger (event)
  (ein:log 'debug "Event: %s" event)
  (case event
    ((status_busy.Kernel status_idle.Kernel)
     (setq ein:events-status-kernel event))
    ((notebook_saving.Notebook
      notebook_saved.Notebook
      notebook_save_failed.Notebook)
     (setq ein:events-status-notebook event))
    (t
     (ein:log 'info "Unknown event: %s" event))))

(provide 'ein-events)

;;; ein-events.el ends here
