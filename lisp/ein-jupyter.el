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

(defun ein:jupyter-server--run (buf cmd dir &optional args)
  (let* ((vargs (append (if dir
                            `("notebook" ,(format "--notebook-dir=%s"
                                                  (convert-standard-filename dir))))
                        (or args ein:jupyter-server-args)))
         (proc (apply #'start-process
                     *ein:jupyter-server-process-name*
                     buf
                     cmd
                     vargs)))
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
            (when (re-search-forward "\\([[:alnum:]]+\\) is\\( now\\)? running" nil t)
              (let ((hub-p (search "jupyterhub" (downcase (match-string 1)))))
                (when (re-search-forward "\\(https?://[^:]*:[0-9]+\\)\\(?:/\\?token=\\([[:alnum:]]+\\)\\)?" nil t)
                  (let ((raw-url (match-string 1))
                        (token (or (match-string 2) (and (not hub-p) ""))))
                    (setq result (list (ein:url raw-url) token)))))))))
    result))

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
  "URL-OR-PORT might get redirected from (ein:jupyter-server-conn-info).
This is currently only the case for jupyterhub.
Once login handshake provides the new URL-OR-PORT, we set various state as pertains
our singleton jupyter server process here."

  ;; Would have used `add-function' if it didn't produce gv-ref warnings.
  (set-process-sentinel 
   proc
   (apply-partially (lambda (url-or-port* sentinel proc* event)
                      (ein:aif sentinel (funcall it proc* event))
                      (funcall #'ein:notebooklist-sentinel url-or-port* proc* event))
                    url-or-port (process-sentinel proc))))

;;;###autoload
(defun ein:jupyter-server-start (server-cmd-path notebook-directory &optional no-login-p login-callback)
  "Start SERVER-CMD_PATH with `--notebook-dir' NOTEBOOK-DIRECTORY.  Login after connection established unless NO-LOGIN-P is set.  LOGIN-CALLBACK takes two arguments, the buffer created by ein:notebooklist-open--finish, and the url-or-port argument of ein:notebooklist-open*.

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
     (list server-cmd-path notebook-directory nil (lambda (buffer url-or-port)
                                                    (pop-to-buffer buffer)))))
  (assert (and (file-exists-p server-cmd-path)
               (file-executable-p server-cmd-path))
          t "Command %s is not valid!" server-cmd-path)
  (setf *ein:last-jupyter-command* server-cmd-path
        *ein:last-jupyter-directory* notebook-directory)
  (if (ein:jupyter-server-process)
      (error "Please first M-x ein:jupyter-server-stop"))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (ignore-errors (ein:jupyter-server-stop t))))
  (let ((proc (ein:jupyter-server--run ein:jupyter-server-buffer-name
                                       *ein:last-jupyter-command*
                                       *ein:last-jupyter-directory*)))
    (when (eql system-type 'windows-nt)
      (accept-process-output proc (/ ein:jupyter-server-run-timeout 1000)))
    (loop repeat 30
          until (car (ein:jupyter-server-conn-info ein:jupyter-server-buffer-name))
          do (sleep-for 0 500)
          finally do
          (unless (car (ein:jupyter-server-conn-info ein:jupyter-server-buffer-name))
            (ein:log 'warn "Jupyter server failed to start, cancelling operation")
            (ein:jupyter-server-stop t)))
    (when (and (not no-login-p) (ein:jupyter-server-process))
      (unless login-callback
        (setq login-callback #'ignore))
      (add-function :after login-callback
                    (apply-partially (lambda (proc* buffer url-or-port)
                                       (ein:set-process-sentinel proc* url-or-port))
                                     proc))
      (ein:jupyter-server-login-and-open login-callback))))

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
            until (or (zerop (hash-table-count check-for-saved))
                      (> x 20))
            do (sleep-for 0 500)))

    (mapc #'ein:notebook-close (ein:notebook-opened-notebooks))

    (loop repeat 10
          do (ein:query-gc-running-process-table)
          when (zerop (hash-table-count ein:query-running-process-table))
          return t
          do (sleep-for 0 500))

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

(provide 'ein-jupyter)
