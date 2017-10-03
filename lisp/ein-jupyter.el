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

(defcustom ein:jupyter-server-run-timeout 60000
  "Time, in milliseconds, to wait for the jupyter server to start before declaring timeout and cancelling the operation."
  :group 'ein
  :type 'integer)

(defcustom ein:jupyter-default-server-command "jupyter"
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

(defun ein:jupyter-server--run (buf cmd dir &optional args)
  (let ((proc (apply #'start-process
                     "EIN: Jupyter notebook server"
                     buf
                     cmd
                     "notebook"
                     (format "--notebook-dir=%s" (convert-standard-filename dir))
                     (or args ein:jupyter-server-args))))
    (setq %ein:jupyter-server-session% proc)
    (if (>= ein:log-level 40)
        (switch-to-buffer ein:jupyter-server-buffer-name))
    proc))

(defun ein:jupyter-server-conn-info ()
  "Return the url and port for the currently running jupyter
session, along with the login token."
  (assert (processp %ein:jupyter-server-session%) t "Jupyter server has not started!")
  (condition-case err
      (with-current-buffer (process-buffer %ein:jupyter-server-session%) ;;ein:jupyter-server-buffer-name
        (goto-char (point-min))
        (re-search-forward "\\(https?://.*:[0-9]+\\)/\\?token=\\(.*\\)" nil)
        (let ((url-or-port (match-string 1))
              (token (match-string 2)))
          (list url-or-port token)))
    (error (with-current-buffer (process-buffer %ein:jupyter-server-session%)
             (goto-char (point-min))
             (if (re-search-forward "\\(https?://.*:[0-9]+\\)" nil t)
                 (list (match-string 1) nil)
               (list nil nil))))))

;;;###autoload
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
      (if (and url-or-port token)
          (progn
            (ein:notebooklist-login url-or-port token)
            (sit-for 1.0) ;; FIXME: Do better!
            (ein:notebooklist-open url-or-port))
        (if url-or-port
            (ein:notebooklist-open url-or-port)
          (ein:log 'info "Could not determine port nor login info for jupyter server."))))))


;;;###autoload
(defun ein:jupyter-server-start (server-cmd-path notebook-directory &optional no-login-after-start-p)
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
  (assert (and (file-exists-p server-cmd-path)
               (file-executable-p server-cmd-path))
          t "Command %s is not valid!" server-cmd-path)
  (setf *ein:last-jupyter-command* server-cmd-path
        *ein:last-jupyter-directory* notebook-directory)
  (if (buffer-live-p (get-buffer ein:jupyter-server-buffer-name))
      (message "Notebook session is already running, check the contents of %s"
               ein:jupyter-server-buffer-name))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (ein:jupyter-server-stop t)))
  (message "Starting notebook server in directory: %s" notebook-directory)
  (lexical-let ((no-login-after-start-p no-login-after-start-p)
                (proc (ein:jupyter-server--run ein:jupyter-server-buffer-name
                                               *ein:last-jupyter-command*
                                               *ein:last-jupyter-directory*)))
    (accept-process-output proc (/ ein:jupyter-server-run-timeout 2))
    (deferred:$
      (deferred:timeout
        ein:jupyter-server-run-timeout 'ein:jupyter-timeout-sentinel
        (deferred:$
          (deferred:next
            (deferred:lambda ()
              (with-current-buffer (process-buffer proc)
                (goto-char (point-min))
                (if (or (search-forward "Notebook is running at:" nil t)
                        (search-forward "Use Control-C" nil t))
                    no-login-after-start-p
                  (deferred:nextc (deferred:wait (/ ein:jupyter-server-run-timeout 4)) self)))))))
      (deferred:nextc it
        (lambda (no-login-p)
          (if (eql no-login-p 'ein:jupyter-timeout-sentinel)
              (progn
                (warn "[EIN] Jupyter server failed to start, cancelling operation.")
                (ein:jupyter-server-stop t))
            (ein:force-ipython-version-check)
            (unless no-login-p
              (ein:jupyter-server-login-and-open))))))))

;;;###autoload

(defun ein:jupyter-server-stop (&optional force)
  "Stop a running jupyter notebook server.

Use this command to stop a running jupyter notebook server. If
there is no running server then no action will be taken.
"
  (interactive)
  (when (and %ein:jupyter-server-session%
             (or force (y-or-n-p "Kill jupyter server and close all open notebooks?")))
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


(defun ein:jupyter-server-list--cmd (&optional args)
  (append (list "notebook"
                "list")
          args))

(defun ein:jupyter-query-running-notebooks ()
  (with-temp-buffer
    (let ((res (apply #'call-process (or *ein:last-jupyter-command*
                                         ein:jupyter-default-server-command)
                      nil
                      t
                      nil
                      (ein:jupyter-server-list--cmd)))
          (contents (rest (s-lines (buffer-string)))))
      contents)))

(provide 'ein-jupyter)
