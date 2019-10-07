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
   (language
    :initarg :language :type string
    :accessor ein:kernelinfo-language
    :documentation "Language for the running kernel.")
   (ccwd
    :initarg :ccwd :type string
    :documentation "cached CWD (last time checked CWD)."))
  :documentation "Info related (but unimportant) to kernel")

(defun ein:kernelinfo-new (kernel get-buffers kernel-language)
  "Make a new `ein:kernelinfo' instance based on KERNEL and GET-BUFFERS."
  (let ((kerinfo (make-instance 'ein:kernelinfo)))
    (setf (slot-value kerinfo 'kernel) kernel)
    (setf (slot-value kerinfo 'get-buffers) get-buffers)
    (setf (slot-value kerinfo 'language) kernel-language)
    (ein:case-equal kernel-language
      ("python" (ein:kernelinfo-setup-hooks kerinfo)))
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
  (ein:log 'debug "(ein:kernel-live-p kernel) = %S"
           (ein:kernel-live-p (slot-value kerinfo 'kernel)))
  (ein:kernelinfo-update-ccwd kerinfo)
  (ein:kernelinfo-update-hostname kerinfo))

(defun ein:kernelinfo-update-ccwd (kerinfo)
  "Update cached current working directory (CCWD) and change
`default-directory' of kernel related buffers."
  (let ((ccwd-string (ein:case-equal (ein:kernelinfo-language kerinfo)
                       (("python") "__import__('sys').stdout.write(__import__('os').getcwd())")
                       ((t) nil))))
    (when ccwd-string
      (ein:kernel-request-stream
       (slot-value kerinfo 'kernel)
       ccwd-string
       (lambda (cwd kerinfo)
         (with-slots (kernel get-buffers) kerinfo
           (setq cwd (ein:kernel-filename-from-python kernel cwd))
           (oset kerinfo :ccwd cwd)
           ;; sync buffer's `default-directory' with CWD
           (when (file-accessible-directory-p cwd)
             (dolist (buffer (ein:funcall-packed get-buffers))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq default-directory (file-name-as-directory cwd))))))))
       (list kerinfo)))))

(defun ein:kernelinfo-update-hostname (kerinfo)
  "Get hostname in which kernel is running and store it in KERINFO."
  (let ((hostname-string (ein:case-equal (ein:kernelinfo-language kerinfo)
                           (("python") "__import__('sys').stdout.write(__import__('socket').gethostname())")
                           ((t) nil))))
    (when hostname-string
      (ein:kernel-request-stream
       (slot-value kerinfo 'kernel)
       hostname-string ; "__import__('sys').stdout.write(__import__('socket').gethostname())" ; uname() not available in windows
       (lambda (hostname kerinfo)
         (oset kerinfo :hostname hostname))
       (list kerinfo)))))


(provide 'ein-kernelinfo)

;;; ein-kernelinfo.el ends here
