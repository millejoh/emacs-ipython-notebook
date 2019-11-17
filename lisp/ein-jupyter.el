;;; ein-jupyter.el --- Manage the jupyter notebook server   -*- lexical-binding: t -*-

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
  "The name of the buffer for the jupyter notebook server
session."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-server-run-timeout 60000
  "Time, in milliseconds, to wait for the jupyter server to start before declaring timeout and cancelling the operation."
  :group 'ein
  :type 'integer)

(defcustom ein:jupyter-server-args '("--no-browser")
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

(defcustom ein:jupyter-default-server-command "jupyter"
  "The default command to start a jupyter notebook server.

Changing this to `jupyter-notebook' requires customizing `ein:jupyter-server-use-subcommand' to nil.
"
  :group 'ein
  :type '(file)
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq *ein:last-jupyter-command* nil)))

(defcustom ein:jupyter-server-use-subcommand "notebook"
  "Users of \"jupyter-notebook\" (as opposed to \"jupyter notebook\") need to `Omit'."
  :group 'ein
  :type '(choice (string :tag "Subcommand" "notebook")
                 (const :tag "Omit" nil)))

(defcustom ein:jupyter-default-kernel 'first-alphabetically
  "With which of ${XDG_DATA_HOME}/jupyter/kernels to create new notebooks."
  :group 'ein
  :type (append
         '(choice (other :tag "First alphabetically" first-alphabetically))
         (condition-case err
             (mapcar
              (lambda (x) `(const :tag ,(cdr x) ,(car x)))
              (cl-loop
                for (k . spec) in
                  (alist-get
                   'kernelspecs
                   (let ((json-object-type 'alist))
                     (json-read-from-string
                      (shell-command-to-string
                       (format "%s kernelspec list --json"
                               ein:jupyter-default-server-command)))))
                collect `(,k . ,(alist-get 'display_name (alist-get 'spec spec)))))
           (error (ein:log 'warn "ein:jupyter-default-kernel: %s" err)
                  '((string :tag "Ask"))))))

(defsubst ein:jupyter-server-process ()
  "Return the emacs process object of our session"
  (get-buffer-process (get-buffer ein:jupyter-server-buffer-name)))

(defun ein:jupyter-server--run (buf cmd dir &optional args)
  (when ein:debug
    (add-to-list 'ein:jupyter-server-args "--debug"))
  (unless (stringp dir)
    (error "ein:jupyter-server--run: notebook directory required"))
  (let* ((vargs (append (ein:aif ein:jupyter-server-use-subcommand (list it))
                        (list (format "--notebook-dir=%s" (convert-standard-filename dir)))
                        args
                        ein:jupyter-server-args))
         (proc (apply #'start-process
                      *ein:jupyter-server-process-name* buf cmd vargs)))
    (ein:log 'info "ein:jupyter-server--run: %s %s" cmd (ein:join-str " " vargs))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun ein:jupyter-server-conn-info (&optional buffer-name)
  "Return the url-or-port and password for BUFFER or the global session."
  (unless buffer-name
    (setq buffer-name ein:jupyter-server-buffer-name))
  (let ((buffer (get-buffer buffer-name))
        (result '(nil nil)))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (re-search-backward (format "Process %s" *ein:jupyter-server-process-name*)
                                nil "") ;; important if we start-stop-start
            (when (re-search-forward "\\([[:alnum:]]+\\) is\\( now\\)? running" nil t)
              (let ((hub-p (cl-search "jupyterhub" (downcase (match-string 1)))))
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
    (cl-multiple-value-bind (url-or-port _password) (ein:jupyter-server-conn-info)
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
(defun ein:jupyter-server-start (server-cmd-path notebook-directory
                                 &optional no-login-p login-callback port)
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
     (list server-cmd-path notebook-directory nil #'(lambda (buffer _url-or-port)
                                                      (pop-to-buffer buffer)))))
  (unless (and (stringp server-cmd-path)
               (file-exists-p server-cmd-path)
               (file-executable-p server-cmd-path))
    (error "Command %s not found or not executable"
           (or *ein:last-jupyter-command*
               ein:jupyter-default-server-command)))
  (setf *ein:last-jupyter-command* server-cmd-path
        *ein:last-jupyter-directory* notebook-directory)
  (if (ein:jupyter-server-process)
      (error "Please first M-x ein:stop"))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (ignore-errors (ein:jupyter-server-stop t))))
  (let ((proc (ein:jupyter-server--run ein:jupyter-server-buffer-name
                                       *ein:last-jupyter-command*
                                       *ein:last-jupyter-directory*
                                       (if (numberp port)
                                           `("--port" ,(format "%s" port)
                                             "--port-retries" "0")))))
    (when (eql system-type 'windows-nt)
      (accept-process-output proc (/ ein:jupyter-server-run-timeout 1000)))
    (cl-loop repeat 30
      until (car (ein:jupyter-server-conn-info ein:jupyter-server-buffer-name))
      do (sleep-for 0 500)
      finally do
        (unless (car (ein:jupyter-server-conn-info ein:jupyter-server-buffer-name))
          (ein:log 'warn "Jupyter server failed to start, cancelling operation")
          (ein:jupyter-server-stop t)))
    (when (and (not no-login-p) (ein:jupyter-server-process))
      (unless login-callback
        (setq login-callback #'ignore))
      (add-function :after (var login-callback)
                    (apply-partially (lambda (proc* _buffer url-or-port)
                                       (ein:set-process-sentinel proc* url-or-port))
                                     proc))
      (ein:jupyter-server-login-and-open login-callback))))

;;;###autoload
(defalias 'ein:run 'ein:jupyter-server-start)

;;;###autoload
(defalias 'ein:stop 'ein:jupyter-server-stop)

(defun ein:undocumented-shutdown (url-or-port)
  (ein:query-singleton-ajax
   (list 'shutdown-server url-or-port)
   (ein:url url-or-port "api/shutdown")
   :type "POST"
   :timeout 3 ;; content-query-timeout and query-timeout default nil
   :sync t))

;;;###autoload
(defun ein:jupyter-server-stop (&optional force log)
  (interactive)
  (ein:and-let* ((url-or-port (car (ein:jupyter-server-conn-info)))
                 (ok (or force (y-or-n-p "Stop server and close notebooks?"))))
    (ein:notebook-close-notebooks t)
    (cl-loop repeat 10
      do (ein:query-running-process-table)
      until (zerop (hash-table-count ein:query-running-process-table))
      do (sleep-for 0 500))
    (if (eq system-type 'windows-nt)
        (progn
          (ein:undocumented-shutdown url-or-port)
          (ein:aif (ein:jupyter-server-process)
              (delete-process it)))
      (let* ((proc (ein:jupyter-server-process))
             (pid (process-id proc)))
        (ein:log 'info "Signaled %s with pid %s" proc pid)
        (signal-process pid 15)
        (run-at-time 2 nil
                     (lambda ()
                       (ein:log 'info "Resignaled %s with pid %s" proc pid)
                       (signal-process pid 15)))))

    ;; `ein:notebooklist-sentinel' frequently does not trigger
    (ein:notebooklist-list-remove url-or-port)
    (kill-buffer (ein:notebooklist-get-buffer url-or-port))
    (when log
      (with-current-buffer ein:jupyter-server-buffer-name
        (write-region (point-min) (point-max) log)))))

(provide 'ein-jupyter)
