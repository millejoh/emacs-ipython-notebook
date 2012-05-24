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


;; Class and variable

(ein:deflocal ein:@notification nil
  "Buffer local variable to hold an instance of `ein:notification'.")

(defclass ein:notification-status ()
  ((status :initarg :status :initform nil :type symbol)
   (message :initarg :message :initform "" :type string))
  "Hold status and it's string representation (message).")

(defclass ein:notification ()
  ((buffer :initarg :buffer :type buffer :document "Notebook buffer")
   (notebook
    :initarg :notebook
    :initform (ein:notification-status "NotebookStatus")
    :type ein:notification-status)
   (kernel
    :initarg :kernel
    :initform (ein:notification-status "KernelStatus")
    :type ein:notification-status))
  "Notification widget for Notebook.")

(provide 'ein-notification)

;;; ein-notification.el ends here
