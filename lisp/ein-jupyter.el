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
(require 'ein-dev)

(defcustom ein:jupyter-server-buffer-name "*ein:jupyter-server*"
  "The name of the buffer to run a jupyter notebook server
  session in."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-server-run-timeout 60000
  "Time, in milliseconds, to wait for the jupyter server to start before declaring timeout and cancelling the operation."
  :group 'ein
  :type 'integer)

(defcustom ein:jupyter-server-args '("--no-browser" "--debug")
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
(defvar *ein:jupyter-server-process-name* "EIN: Jupyter notebook server")

(defvar *ein:last-jupyter-command* nil)
(defvar *ein:last-jupyter-directory* nil)

(defsubst ein:jupyter-server-process ()
  "Return the emacs process object of our session"
  (get-buffer-process (get-buffer ein:jupyter-server-buffer-name)))

(defun ein:jupyter-server--cmd (path dir)
  (append (list path
                "notebook"
                (format "--notebook-dir=%s" (convert-standard-filename dir)))
          ein:jupyter-server-args))

(defun ein:jupyter-server--run (buf cmd dir &optional args)
  (let ((proc (apply #'start-process
                     *ein:jupyter-server-process-name*
                     buf
                     cmd
                     "notebook"
                     (format "--notebook-dir=%s" (convert-standard-filename dir))
                     (or args ein:jupyter-server-args))))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun ein:jupyter-server-conn-info (&optional buffer)
  "Return the url-or-port and password for BUFFER or the global session."
  (unless buffer
    (setq buffer (get-buffer ein:jupyter-server-buffer-name)))
  (let ((result '(nil nil)))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (re-search-backward (format "Process %s" *ein:jupyter-server-process-name*)
                                nil "") ;; important if we start-stop-start
            (if (and (re-search-forward "otebook [iI]s [rR]unning" nil t)
                     (re-search-forward "\\(https?://[^:]+:[0-9]+\\)\\(?:/\\?token=\\([[:alnum:]]+\\)\\)?" nil t))
                (let ((raw-url (match-string 1))
                      (token (or (match-string 2) "")))
                  (setq result (list (ein:url raw-url) token)))))))
    result))

;;;###autoload
(defun ein:jupyter-server-login-and-open (&optional callback)
  "Log in and open a notebooklist buffer for a running jupyter notebook server.

Determine if there is a running jupyter server (started via a
call to `ein:jupyter-server-start') and then try to guess if
token authentication is enabled. If a token is found use it to generate a
call to `ein:notebooklist-login' and once authenticated open the notebooklist buffer
via a call to `ein:notebooklist-open'."
  (interactive)
  (when (ein:jupyter-server-process)
    (multiple-value-bind (url-or-port password) (ein:jupyter-server-conn-info)
      (ein:notebooklist-login url-or-port callback))))

(defsubst ein:set-process-sentinel (proc url-or-port)
  "Adjust notebooklist corresponding to URL-OR-PORT when the PROC gets signalled.  Would use `add-function' if it didn't produce gv-ref warnings."
  (set-process-sentinel 
   proc
   (apply-partially (lambda (url-or-port* sentinel process event)
                      (ein:aif sentinel (funcall it process event))
                      (funcall #'ein:notebooklist-proc--sentinel url-or-port* process event))
                    url-or-port (process-sentinel proc))))

;;;###autoload
(defun ein:jupyter-server-start (server-cmd-path notebook-directory &optional no-login-p login-callback)
  "Start SERVER-CMD_PATH with `--notebook-dir' NOTEBOOK-DIRECTORY.  Login after connection established unless NO-LOGIN-P is set.  LOGIN-CALLBACK taking single argument, the buffer created by ein:notebooklist-open--finish.

This command opens an asynchronous process running the jupyter
notebook server and then tries to detect the url and password to
generate automatic calls to `ein:notebooklist-login' and
`ein:notebooklist-open'.

With \\[universal-argument] prefix arg, it will prompt the user for the path to
the jupyter executable first. Else, it will try to use the
value of `*ein:last-jupyter-command*' or the value of the
customizable variable `ein:jupyter-default-server-command'.

Then it prompts the user for the path of the root directory
containing the notebooks the user wants to access.

The buffer named by `ein:jupyter-server-buffer-name' will contain
the log of the running jupyter server."
  (interactive
   (let* ((default-command (or *ein:last-jupyter-command*
                               ein:jupyter-default-server-command))
          (server-cmd-path
           (executable-find (if current-prefix-arg
                                (read-file-name "Server command: " default-directory nil nil
                                                default-command)
                              default-command)))
          (notebook-directory 
           (read-directory-name "Notebook directory: "
                                (or *ein:last-jupyter-directory*
                                    ein:jupyter-default-notebook-directory))))
     (list server-cmd-path notebook-directory nil #'pop-to-buffer)))
  (assert (and (file-exists-p server-cmd-path)
               (file-executable-p server-cmd-path))
          t "Command %s is not valid!" server-cmd-path)
  (setf *ein:last-jupyter-command* server-cmd-path
        *ein:last-jupyter-directory* notebook-directory)
  (if (ein:jupyter-server-process)
      (error "Please first M-x ein:jupyter-server-stop"))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (ignore-errors (ein:jupyter-server-stop t))))
  (lexical-let* (done-p
                 (no-login-p no-login-p)
                 (login-callback login-callback)
                 (proc (ein:jupyter-server--run ein:jupyter-server-buffer-name
                                                *ein:last-jupyter-command*
                                                *ein:last-jupyter-directory*))
                 (buf (process-buffer proc)))
    (when (eql system-type 'windows-nt)
      (accept-process-output proc (/ ein:jupyter-server-run-timeout 1000)))
    (if ein:dev-prefer-deferred
        (deferred:$
          (deferred:timeout
            ein:jupyter-server-run-timeout 'timeout
            (deferred:lambda ()
              (ein:aif (car (ein:jupyter-server-conn-info))
                  (progn (ein:set-process-sentinel proc it) no-login-p)
                (deferred:nextc (deferred:wait (/ ein:jupyter-server-run-timeout 5)) self))))
          (deferred:nextc it
            (lambda (no-login-p)
              (if (eq no-login-p 'timeout)
                  (progn
                    (setf done-p 'error)
                    (ein:log 'warn "Jupyter server failed to start, cancelling operation.")
                    (ein:jupyter-server-stop t))
                (setf done-p t)
                (unless no-login-p
                  (ein:jupyter-server-login-and-open login-callback))))))
      (loop repeat 30
            until (car (ein:jupyter-server-conn-info buf))
            do (sleep-for 0 500)
            finally do 
              (ein:aif (car (ein:jupyter-server-conn-info buf))
                  (progn (ein:set-process-sentinel proc it) (setf done-p t))
                (setf done-p "error")
                (ein:log 'warn "Jupyter server failed to start, cancelling operation")
                (ein:jupyter-server-stop t)))
      (if (and (not no-login-p) (ein:jupyter-server-process))
          (ein:jupyter-server-login-and-open login-callback)))))

;;;###autoload
(defalias 'ein:run 'ein:jupyter-server-start)

;;;###autoload
(defalias 'ein:stop 'ein:jupyter-server-stop)

;;;###autoload
(defun ein:jupyter-server-stop (&optional force log)
  "Stop a running jupyter notebook server.

Use this command to stop a running jupyter notebook server. If
there is no running server then no action will be taken.
"
  (interactive)
  (when (and (ein:jupyter-server-process)
             (or force (y-or-n-p "Kill jupyter server and close all open notebooks?")))
    (let ((unsaved (ein:notebook-opened-notebooks #'ein:notebook-modified-p))
          (check-for-saved (make-hash-table :test #'equal)))
      (when unsaved
        (loop for nb in unsaved
              when (y-or-n-p (format "Save notebook %s before stopping the server?" (ein:$notebook-notebook-name nb)))
              do (progn
                   (setf (gethash (ein:$notebook-notebook-name nb) check-for-saved) t)
                   (ein:notebook-save-notebook nb
                                               #'(lambda (name check-hash)
                                                   (remhash name check-hash))
                                               (list (ein:$notebook-notebook-name nb) check-for-saved)))))
      (loop for x upfrom 0 by 1
            until (or (= (hash-table-count check-for-saved) 0)
                      (> x 1000000))
            do (sit-for 0.1)))

    (mapc #'ein:notebook-close (ein:notebook-opened-notebooks))

    ;; Both (quit-process) and (delete-process) leaked child kernels, so signal
    (ein:aif (ein:jupyter-server-process)
        (progn
          (if (eql system-type 'windows-nt)
              (delete-process it)
            (let ((pid (process-id it)))
              (ein:log 'verbose "Signaled %s with pid %s" it pid)
              (signal-process pid 15)))
          (ein:log 'info "Stopped Jupyter notebook server.")))
    (when log
      (with-current-buffer ein:jupyter-server-buffer-name
        (write-region (point-min) (point-max) log)))))

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
