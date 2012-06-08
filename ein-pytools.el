;;; ein-pytools.el --- Python tools build on top of kernel

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-pytools.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pytools.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pytools.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'ein-kernel)

(eval-when-compile (defvar ein:notebook)
                   (defvar ein:@connect))
(declare-function ein:$notebook-kernel "ein-notebook")
(declare-function ein:notebook-buffer "ein-notebook")
(declare-function ein:connect-get-kernel "ein-connect")
(declare-function ein:connect-get-notebook "ein-connect")
(declare-function ein:connect-to-notebook "ein-connect")

(defcustom ein:propagate-connect t
  "Set to `t' to connect to the notebook after jumping to a buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

(defun ein:pytools-get-kernel ()
  (cond
   (ein:notebook (ein:$notebook-kernel ein:notebook))
   (ein:@connect (ein:connect-get-kernel))))

(defun ein:pytools-get-notebook ()
  (cond
   (ein:notebook ein:notebook)
   (ein:@connect (ein:connect-get-notebook))))

(defun ein:pytools-get-notebook-buffer ()
  (ein:notebook-buffer (ein:pytools-get-notebook)))

(defun ein:pytools-setup-hooks (kernel)
  (push (cons #'ein:pytools-add-sys-path kernel)
        (ein:$kernel-after-start-hook kernel)))

(defun ein:pytools-add-sys-path (kernel)
  (ein:kernel-execute
   kernel
   (format "__import__('sys').path.append('%s')" ein:source-dir)))

(defun ein:pytools-jump-to-source (kernel object &optional
                                          other-window notebook-buffer)
  (ein:log 'info "Jumping to the source of %s..." object)
  (ein:kernel-execute
   kernel
   (format "__import__('ein').find_source('%s')" object)
   (list
    :output
    (cons
     (lambda (packed msg-type content)
       (let ((object (nth 0 packed))
             (other-window (nth 1 packed))
             (notebook-buffer (nth 2 packed)))
         (ein:case-equal msg-type
           (("stream")
            (ein:aif (plist-get content :data)
                (if (or (string-match "^WARNING: .*" it)
                        (string-match
                         "^Traceback (most recent call last):\n" it)
                        (string-match "^.*<ipython-input-[^>\n]+>\n" it))
                    (ein:log 'info
                      "Jumping to the source of %s...Not found" object)
                  (let* ((filename-lineno (split-string it "\n"))
                         (filename (car filename-lineno))
                         (lineno (string-to-number (cadr filename-lineno))))
                    (unless (equal filename "")
                      (funcall (if other-window
                                   #'find-file-other-window
                                 #'find-file)
                               filename)
                      (goto-char (point-min))
                      (forward-line (1- lineno))
                      (when (and notebook-buffer (not ein:@connect))
                        (ein:connect-to-notebook notebook-buffer))
                      (ein:log 'info
                        "Jumping to the source of %s...Done" object))))))
           (("pyerr")
            (ein:log 'info
              "Jumping to the source of %s...Not found" object)))))
     (list object other-window notebook-buffer)))))

(defun ein:pytools-jump-to-source-command (&optional other-window)
  (interactive "P")
  (require 'ein-connect)
  (let ((kernel (ein:pytools-get-kernel))
        (object (ein:object-at-point)))
    (assert (ein:kernel-ready-p kernel) nil "Kernel is not ready.")
    (assert object nil "Object at point not found.")
    (ein:pytools-jump-to-source kernel object other-window
                                (when ein:propagate-connect
                                  (ein:pytools-get-notebook-buffer)))))

(provide 'ein-pytools)

;;; ein-pytools.el ends here
