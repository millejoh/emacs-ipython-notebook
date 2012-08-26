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


(defstruct ein:$kernelinfo
  "Info related (but unimportant) to kernel

`ein:$kernelinfo-kernel'
  Kernel instance.

`ein:$kernelinfo-buffer'
  Notebook buffer that the kernel associated with.

`ein:$kernelinfo-hostname'
  Host name of the machine where the kernel is running on.

`ein:$kernelinfo-ccwd'
  cached CWD (last time checked CWD)."
  kernel
  buffer
  hostname
  ccwd)

(defun ein:kernelinfo-setup (kernel buffer)
  (let ((kerinfo (make-ein:$kernelinfo)))
    (setf (ein:$kernelinfo-kernel kerinfo) kernel)
    (ein:kernelinfo-setup-hooks kerinfo)
    (ein:kernelinfo-init kerinfo buffer)
    kerinfo))

(defun ein:kernelinfo-init (kerinfo buffer)
  (setf (ein:$kernelinfo-buffer kerinfo) buffer))

(defun ein:kernelinfo-setup-hooks (kerinfo)
  "Add `ein:kernelinfo-update-*' to `ein:$kernel-after-*-hook'."
  (let ((kernel (ein:$kernelinfo-kernel kerinfo)))
    (push (cons #'ein:kernelinfo-update-all kerinfo)
          (ein:$kernel-after-start-hook kernel))
    (push (cons #'ein:kernelinfo-update-ccwd kerinfo)
          (ein:$kernel-after-execute-hook kernel))))

(defun ein:kernelinfo-update-all (kerinfo)
  (ein:log 'debug "EIN:KERNELINFO-UPDATE-ALL")
  (ein:log 'debug "(ein:kernel-live-p kernel) = %S"
           (ein:kernel-live-p (ein:$kernelinfo-kernel kerinfo)))
  (ein:kernelinfo-update-ccwd kerinfo)
  (ein:kernelinfo-update-hostname kerinfo))

(defun ein:kernelinfo-update-ccwd (kerinfo)
  "Update cached current working directory (CCWD) and change
`default-directory' of `ein:$kernelinfo-buffer'."
  (ein:kernel-request-stream
   (ein:$kernelinfo-kernel kerinfo)
   "__import__('sys').stdout.write(__import__('os').getcwd())"
   (lambda (cwd kernel kerinfo buffer)
     (setq cwd (ein:kernel-filename-from-python kernel cwd))
     (setf (ein:$kernelinfo-ccwd kerinfo) cwd)
     ;; sync buffer's `default-directory' with CWD
     ;; FIXME: Support multiple buffers.
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (file-accessible-directory-p cwd)
           (setq default-directory (file-name-as-directory cwd))))))
   (let ((kernel (ein:$kernelinfo-kernel kerinfo)))
     (list kernel kerinfo (ein:$kernelinfo-buffer kerinfo)))))

(defun ein:kernelinfo-update-hostname (kerinfo)
  "Get hostname in which kernel is running and store it in KERINFO."
  (ein:kernel-request-stream
   (ein:$kernelinfo-kernel kerinfo)
   "__import__('sys').stdout.write(__import__('os').uname()[1])"
   (lambda (hostname kerinfo)
     (setf (ein:$kernelinfo-hostname kerinfo) hostname))
   (list kerinfo)))


(provide 'ein-kernelinfo)

;;; ein-kernelinfo.el ends here
