;;; ein-jupyter.el --- Manage the jupyter notebook server

;; Copyright (C) 2017- John M. Miller

;; Authors: John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-jupyter.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-jupyter.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-jupyter.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defcustom ein:jupyter-server-buffer-name "*ein:jupyter-server*"
  "The default name of the buffer to run a jupyter notebook
  server session in."
  :group 'ein)

(defvar *ein:jupyter-server-accept-timeout* 60)
(defvar %ein:jupyter-server-session% nil)
(defvar *ein:last-jupyter-command* nil)
(defvar *ein:last-jupyter-directory* nil)

(defun ein:jupyter-server--cmd (path dir)
  (list path
        "notebook"
        (format "--notebook-dir=%s" dir)))

(defun ein:jupyter-server-start  (server-path server-directory)
  (interactive (list
                (read-file-name "Server Command: " default-directory nil nil *ein:last-jupyter-command*)
                (read-directory-name "Notebook Directory: " *ein:last-jupyter-directory*)))
  (assert (and (file-exists-p server-path)
               (file-executable-p server-path))
          t "Command %s is not valid!" server-path)
  (setf *ein:last-jupyter-command* server-path
        *ein:last-jupyter-directory* server-directory)
  (if (buffer-live-p (get-buffer ein:jupyter-server-buffer-name))
      (message "Notebook session is already running, check the contents of %s"
               ein:jupyter-server-buffer-name))
  (message "Starting notebook server in directory: %s" server-directory)
  (let* ((proc (make-process :name "EIN: Jupyter notebook server"
                             :buffer ein:jupyter-server-buffer-name
                             :command (ein:jupyter-server--cmd server-path server-directory))))
    (setq %ein:jupyter-server-session% proc)
    (if (accept-process-output proc *ein:jupyter-server-accept-timeout*)
        (progn
          (sit-for 1.0)
          (ein:jupyter-server-login-and-open)))))

(defun ein:jupyter-server-login-and-open ()
  (interactive)
  (when (buffer-live-p (get-buffer ein:jupyter-server-buffer-name))
    (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
      (ein:notebooklist-login url-or-port token)
      (sit-for 2.0)
      (ein:notebooklist-open url-or-port))))

(defun ein:jupyter-server-stop ()
  (interactive)
  (when %ein:jupyter-server-session%
    (delete-process %ein:jupyter-server-session%)
    (kill-buffer ein:jupyter-server-buffer-name)
    (setq %ein:jupyter-server-session% nil)
    (message "Stopped Jupyter notebook server.")))

(defun ein:jupyter-server-conn-info ()
  (with-current-buffer ein:jupyter-server-buffer-name
    (goto-char (point-min))
    (re-search-forward "\\(https?://.*:[0-9]+\\)/\\?token=\\(.*\\)" nil t)
    (let ((url-or-port (match-string 1))
          (token (match-string 2)))
      (list url-or-port token))))


