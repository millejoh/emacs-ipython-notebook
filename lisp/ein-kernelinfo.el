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

(eval-when-compile (require 'cl))
(require 'ein-kernel)
(require 'eieio)

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
    (oset kerinfo :kernel kernel)
    (oset kerinfo :get-buffers get-buffers)
    (ein:kernelinfo-setup-hooks kerinfo)
    kerinfo))

(defun ein:kernelinfo-setup-hooks (kerinfo)
  "Add `ein:kernelinfo-update-*' to `ein:$kernel-after-*-hook'."
  (with-slots (kernel) kerinfo
    (push (cons #'ein:kernelinfo-update-all kerinfo)
          (ein:$kernel-after-start-hook kernel))
    (push (cons #'ein:kernelinfo-update-ccwd kerinfo)
          (ein:$kernel-after-execute-hook kernel))))

(defun ein:kernelinfo-update-all (kerinfo)
  "Update KERINFO slots by triggering all update functions."
  (ein:log 'debug "EIN:KERNELINFO-UPDATE-ALL")
  (ein:log 'debug "(ein:kernel-live-p kernel) = %S"
           (ein:kernel-live-p (oref kerinfo :kernel)))
  (ein:kernelinfo-update-ccwd kerinfo)
  (ein:kernelinfo-update-hostname kerinfo))

(defun ein:kernelinfo-update-ccwd (kerinfo)
  "Update cached current working directory (CCWD) and change
`default-directory' of kernel related buffers."
  (ein:kernel-request-stream
   (oref kerinfo :kernel)
   "__import__('sys').stdout.write(__import__('os').getcwd())"
   (lambda (cwd kerinfo)
     (with-slots (kernel get-buffers) kerinfo
       (setq cwd (ein:kernel-filename-from-python kernel cwd))
       (oset kerinfo :ccwd cwd)
       ;; sync buffer's `default-directory' with CWD
       (when (file-accessible-directory-p cwd)
         (mapc (lambda (buffer)
                 (with-current-buffer buffer
                   (setq default-directory (file-name-as-directory cwd))))
               (ein:filter #'buffer-live-p
                           (ein:funcall-packed get-buffers))))))
   (list kerinfo)))

(defun ein:kernelinfo-update-hostname (kerinfo)
  "Get hostname in which kernel is running and store it in KERINFO."
  (ein:kernel-request-stream
   (oref kerinfo :kernel)
   "__import__('sys').stdout.write(__import__('socket').gethostname())" ; uname() not available in windows
   (lambda (hostname kerinfo)
     (oset kerinfo :hostname hostname))
   (list kerinfo)))


(provide 'ein-kernelinfo)

;;; ein-kernelinfo.el ends here
