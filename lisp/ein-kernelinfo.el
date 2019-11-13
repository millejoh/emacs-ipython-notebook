;;; ein-kernelinfo.el --- Kernel info module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-kernelinfo.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; ein-kernelinfo.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernelinfo.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'ein-kernel)

(defclass ein:kernelinfo ()
  ((kernel
    :initarg :kernel :type ein:$kernel
    :documentation "Kernel instance.")
   (get-buffers
    :initarg :get-buffers
    :documentation "A packed function to get buffers associated
with the kernel.  The buffer local `default-directory' variable
in these buffer will be synced with the kernel's cwd.")
   (hostname
    :initarg :hostname :type string
    :documentation "Host name of the machine where the kernel is running on.")
   (ccwd
    :initarg :ccwd :type string
    :documentation "cached CWD (last time checked CWD)."))
  :documentation "Info related (but unimportant) to kernel")

(defun ein:kernelinfo-new (kernel get-buffers)
  "Make a new `ein:kernelinfo' instance based on KERNEL and GET-BUFFERS."
  (let ((kerinfo (make-instance 'ein:kernelinfo)))
    (setf (slot-value kerinfo 'kernel) kernel)
    (setf (slot-value kerinfo 'get-buffers) get-buffers)
    kerinfo))
(provide 'ein-kernelinfo)

;;; ein-kernelinfo.el ends here
