;;; ein-connect.el --- Connect external buffers to IPython

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-connect.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-connect.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-connect.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: There is a problem when connected notebook is closed.
;;        This can be fixed in some ways:
;; * Turn off ein:connect when the command that uses kernel is invoked
;;   but corresponding notebook was closed already.
;; * Connect directly to ein:kernel and make its destructor to care
;;   about connecting buffers.

;;; Code:

(require 'eieio)
(eval-when-compile (require 'auto-complete nil t))

(require 'ein-notebook)
(require 'ein-shared-output)


(ein:deflocal ein:@connect nil
  "Buffer local variable to store an instance of `ein:$connect'")

(defclass ein:$connect ()
  ((notebook :initarg :notebook :type ein:$notebook)
   (buffer :initarg :buffer :type buffer)))

(defun ein:connect-setup (notebook buffer)
  (with-current-buffer buffer
    (setq ein:@connect
          (ein:$connect "Connect" :notebook notebook :buffer buffer))
    ein:@connect))

(defun ein:connect-to-notebook (buffer-or-name)
  "Connect any buffer to notebook and its kernel."
  (interactive
   (list
    (completing-read
     "Select notebook: "
     (mapcar #'buffer-name (ein:notebook-opened-buffers)))))
  (let* ((notebook (buffer-local-value 'ein:notebook
                                       (get-buffer buffer-or-name)))
         (connection (ein:connect-setup notebook (current-buffer))))
    (when (ein:eval-if-bound 'ac-sources)
      (push 'ac-source-ein-cached ac-sources))
    (ein:connect-mode)
    (message "Connected to %s"
             (ein:$notebook-notebook-name notebook))
    connection))

(defun ein:connect-get-notebook ()
  (oref ein:@connect :notebook))

(defun ein:connect-get-kernel ()
  (ein:$notebook-kernel (ein:connect-get-notebook)))

(defun ein:connect-eval-buffer ()
  (interactive)
  (ein:connect-eval-string-internal (buffer-string))
  (ein:log 'info "Whole buffer is sent to the kernel."))

(defun ein:connect-eval-region (start end)
  (interactive "r")
  (ein:connect-eval-string-internal (buffer-substring start end))
  (ein:log 'info "Selected region is sent to the kernel."))

(defun ein:connect-eval-string (code)
  (interactive "sIP[y]: ")
  (ein:connect-eval-string-internal code)
  (ein:log 'info "Code \"%s\" is sent to the kernel." code))

(defun ein:connect-eval-string-internal (code)
  (let ((cell (ein:shared-output-get-cell))
        (kernel (ein:connect-get-kernel))
        (code (ein:trim-indent code)))
    (ein:cell-execute cell kernel code)))

(defun ein:connect-request-tool-tip-command ()
  (interactive)
  (let ((notebook (ein:connect-get-notebook)))
    (ein:kernel-if-ready (ein:$notebook-kernel notebook)
      (let ((func (ein:object-at-point)))
        ;; Set cell=nil.  In fact, the argument cell is not used.
        ;; FIXME: refactor `ein:notebook-request-tool-tip'
        (ein:notebook-request-tool-tip notebook nil func)))))

(defun ein:connect-request-help-command ()
  (interactive)
  (ein:notebook-request-help (ein:connect-get-notebook)))

(defun ein:connect-request-tool-tip-or-help-command (&optional pager)
  (interactive "P")
  (if pager
      (ein:connect-request-help-command)
    (ein:connect-request-tool-tip-command)))

(defun ein:connect-complete-command ()
  (interactive)
  (ein:notebook-complete-at-point (ein:connect-get-notebook)))

(defun ein:connect-pop-to-notebook ()
  (interactive)
  (pop-to-buffer (ein:notebook-buffer (ein:connect-get-notebook))))

(defvar ein:connect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'ein:connect-eval-buffer)
    (define-key map "\C-c\C-r" 'ein:connect-eval-region)
    (define-key map (kbd "C-:") 'ein:connect-eval-string)
    (define-key map "\C-c\C-f" 'ein:connect-request-tool-tip-or-help-command)
    (define-key map "\C-c\C-i" 'ein:connect-complete-command)
    (define-key map "\C-c\C-z" 'ein:connect-pop-to-notebook)
    map))

(define-minor-mode ein:connect-mode
  "Minor mode for communicating with IPython notebook.

\\{ein:connect-mode-map}"
  :lighter " ein:c"
  :keymap ein:connect-mode-map
  :group 'ein)


(provide 'ein-connect)

;;; ein-connect.el ends here
