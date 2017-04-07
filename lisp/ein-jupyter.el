;;; ein-jupyter.el --- Manage the jupyter notebook server

;; Copyright (C) 2017 John M. Miller

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

(require 'ein-core)
(require 'ein-notebooklist)

(defcustom ein:jupyter-server-buffer-name "*ein:jupyter-server*"
  "The name of the buffer to run a jupyter notebook server
  session in."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-default-server-command nil
  "If you are tired of always being queried for the location of
the jupyter command, you can set it here for future calls to
`ein:jupyter-server-start'"
  :group 'ein
  :type '(file))

(defcustom ein:jupyter-server-args nil
  "Add any additional command line options you wish to include
with the call to the jupyter notebook."
  :group 'ein
  :type '(repeat string))

(defcustom ein:jupyter-default-notebook-directory nil
  "If you are tired of always being queried for the location of
the notebook directory, you can set it here for future calls to
`ein:jupyter-server-start'"
  :group 'ein
  :type '(directory))

(defvar *ein:jupyter-server-accept-timeout* 60)
(defvar %ein:jupyter-server-session% nil)

(defvar *ein:last-jupyter-command* nil)
(defvar *ein:last-jupyter-directory* nil)

(defun ein:jupyter-server--cmd (path dir)
  (append (list path
                "notebook"
                (format "--notebook-dir=%s" (convert-standard-filename dir)))
          ein:jupyter-server-args))


;;;###autoload
(defun ein:jupyter-server-start (server-path server-directory &optional no-login-after-start-p)
  "Start the jupyter notebook server at the given path.

This command opens an asynchronous process running the jupyter
notebook server and then tries to detect the url and token to
generate automatic calls to `ein:notebooklist-login' and
`ein:notebooklist-open'.

On executing the command will prompt the user for the path to the
jupyter executable and the path for the root directory containing
the notebooks the user wants to access.

The buffer named by `ein:jupyter-server-buffer-name' will contain
the log of the running jupyter server."
  (interactive (list
                (read-file-name "Server Command: " default-directory nil nil (or *ein:last-jupyter-command*
                                                                                 ein:jupyter-default-server-command))
                (read-directory-name "Notebook Directory: " (or *ein:last-jupyter-directory*
                                                                ein:jupyter-default-notebook-directory))))
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
    (if (>= ein:log-level 40)
        (switch-to-buffer ein:jupyter-server-buffer-name))
    (if (accept-process-output proc *ein:jupyter-server-accept-timeout*)
        (with-current-buffer (process-buffer proc)
          (goto-char (point-min))
          (loop for x upfrom 0 by 1
                until (or (search-forward "Notebook is running at:" nil t)
                          (> x 100))
                do (progn (sit-for 0.1)
                          (goto-char (point-min))) 
                finally (unless no-login-after-start-p
                          (ein:jupyter-server-login-and-open)))))))

(defun ein:jupyter-server-login-and-open ()
  "Log in and open a notebooklist buffer for a running jupyter notebook server.

Determine if there is a running jupyter server (started via a
call to `ein:jupyter-server-start') and then try to guess if
token authentication is enabled. If a token is found use it to generate a
call to `ein:notebooklist-login' and once authenticated open the notebooklist buffer
via a call to `ein:notebooklist-open'."
  (interactive)
  (when (buffer-live-p (get-buffer ein:jupyter-server-buffer-name))
    (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
      (ein:notebooklist-login url-or-port token)
      (sit-for 1.0) ;; FIXME: Do better!
      (ein:notebooklist-open url-or-port))))

;;;###autoload
(defun ein:jupyter-server-stop ()
  "Stop a running jupyter notebook server.

Use this command to stop a running jupyter notebook server. If
there is no running server then no action will be taken.
"
  (interactive)
  (when (and %ein:jupyter-server-session%
             (y-or-n-p "Kill jupyter server and close all open notebooks?"))
    (let ((unsaved (ein:notebook-opened-notebooks #'ein:notebook-modified-p))
          (check-for-saved (make-hash-table :test #'equal)))
      (when unsaved
        (loop for nb in unsaved
              when (y-or-n-p (format "Save notebook %s before stopping the server?" (ein:$notebook-notebook-name nb)))
              do (progn
                   (setf (gethash (ein:$notebook-notebook-name nb) check-for-saved) t)
                   (ein:notebook-save-notebook nb 0
                                               #'(lambda (name check-hash)
                                                   (remhash name check-hash))
                                               (list (ein:$notebook-notebook-name nb) check-for-saved)))))
      (loop for x upfrom 0 by 1
            until (or (= (hash-table-count check-for-saved) 0)
                      (> x 1000000))
            do (sit-for 0.1)))
    (mapc #'ein:notebook-close (ein:notebook-opened-notebooks))
    (delete-process %ein:jupyter-server-session%)
    (kill-buffer ein:jupyter-server-buffer-name)
    (setq %ein:jupyter-server-session% nil)
    (message "Stopped Jupyter notebook server.")))

(defun ein:jupyter-server-conn-info ()
  "Return the url and port for the currently running jupyter
session, along with the login token."
  (assert (processp %ein:jupyter-server-session%) t "Jupyter server has not started!")
  (with-current-buffer (process-buffer %ein:jupyter-server-session%) ;;ein:jupyter-server-buffer-name
    (goto-char (point-min))
    (re-search-forward "\\(https?://.*:[0-9]+\\)/\\?token=\\(.*\\)" nil t)
    (let ((url-or-port (match-string 1))
          (token (match-string 2)))
      (list url-or-port token))))

(provide 'ein-jupyter)
