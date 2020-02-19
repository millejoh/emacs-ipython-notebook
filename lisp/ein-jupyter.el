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

(defcustom ein:jupyter-use-containers nil
  "Take EIN in a different direcsh."
  :group 'ein
  :type 'boolean)

(defcustom ein:jupyter-docker-image "jupyter/datascience-notebook"
  "Docker pull whichever jupyter image you prefer.  This defaults to
the 'jupyter docker stacks' on hub.docker.com.

Optionally append ':tag', e.g., ':latest' in the customary way."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-docker-mount-point "/home/jovyan/ipynb"
  "Directory in docker image where to mount `ein:jupyter-default-notebook-directory'."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-docker-additional-switches "-e JUPYTER_ENABLE_LAB=no --rm"
  "Additional options to the 'docker run' call.

Note some options like '-v' and '-network' are imposed by EIN."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-server-command "jupyter"
  "The default command to start a jupyter notebook server.

Changing this to `jupyter-notebook' requires customizing `ein:jupyter-server-use-subcommand' to nil."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-default-server-command ein:jupyter-server-command
  "Obsolete alias for `ein:jupyter-server-command'"
  :group 'ein
  :type 'string
  :set (lambda (_symbol value)
         (setq ein:jupyter-server-command value)))

(defcustom ein:jupyter-server-use-subcommand "notebook"
  "Users of \"jupyter-notebook\" (as opposed to \"jupyter notebook\") need to Omit."
  :group 'ein
  :type '(choice (string :tag "Subcommand" "notebook")
                 (const :tag "Omit" nil)))

(defcustom ein:jupyter-server-args '("--no-browser")
  "Add any additional command line options you wish to include
with the call to the jupyter notebook."
  :group 'ein
  :type '(repeat string))

(defcustom ein:jupyter-default-notebook-directory nil
  "Default location of ipynb files."
  :group 'ein
  :type 'directory)

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
                            ein:jupyter-server-command)))))
               collect `(,k . ,(alist-get 'display_name (alist-get 'spec spec)))))
           (error (ein:log 'warn "ein:jupyter-default-kernel: %s" err)
                  '((string :tag "Ask"))))))

(defvar *ein:jupyter-server-process-name* "ein server")
(defvar *ein:jupyter-server-buffer-name*
  (format "*%s*" *ein:jupyter-server-process-name*))

(defun ein:jupyter-get-default-kernel (kernels)
  (cond (ein:%notebooklist-new-kernel%
         (ein:$kernelspec-name ein:%notebooklist-new-kernel%))
        ((eq ein:jupyter-default-kernel 'first-alphabetically)
         (car (car kernels)))
        ((stringp ein:jupyter-default-kernel)
         ein:jupyter-default-kernel)
        (t
         (symbol-name ein:jupyter-default-kernel))))

(defun ein:jupyter-process-lines (url-or-port command &rest args)
  "If URL-OR-PORT registered as a k8s url, preface COMMAND ARGS with `kubectl exec'."
  (condition-case err
      (apply #'process-lines command args)
    (error (ein:log 'info "ein:jupyter-process-lines: %s" (error-message-string err))
           nil)))

(defsubst ein:jupyter-server-process ()
  "Return the emacs process object of our session."
  (get-buffer-process (get-buffer *ein:jupyter-server-buffer-name*)))

(defun ein:jupyter-server--run (buf user-cmd dir &optional args)
  (let* ((cmd (if ein:jupyter-use-containers "docker" user-cmd))
         (vargs (cond (ein:jupyter-use-containers
                       (split-string
                        (format "run --network host -v %s:%s %s %s"
                                dir
                                ein:jupyter-docker-mount-point
                                ein:jupyter-docker-additional-switches
                                ein:jupyter-docker-image)))
                      (t
                       (append (aif ein:jupyter-server-use-subcommand (list it))
                               (list (format "--notebook-dir=%s"
                                             (convert-standard-filename dir)))
                               args
                               (let ((copy ein:jupyter-server-args))
                                 (when ein:debug
                                   (add-to-list 'copy "--debug"))
                                 copy)))))
         (proc (apply #'start-process
                      *ein:jupyter-server-process-name* buf cmd vargs)))
    (ein:log 'info "ein:jupyter-server--run: %s %s" cmd (ein:join-str " " vargs))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun ein:jupyter-server-conn-info (&optional buffer-name)
  "Return the url-or-port and password for BUFFER or the global session."
  (unless buffer-name
    (setq buffer-name *ein:jupyter-server-buffer-name*))
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
                      (aif sentinel (funcall it proc* event))
                      (funcall #'ein:notebooklist-sentinel url-or-port* proc* event))
                    url-or-port (process-sentinel proc))))

;;;###autoload
(defun ein:jupyter-crib-token (url-or-port)
  "Shell out to jupyter for its credentials knowledge.  Return list of (PASSWORD TOKEN)."
  (aif (cl-loop for line in
                (apply #'ein:jupyter-process-lines url-or-port
                       ein:jupyter-server-command
                       (split-string
                        (format "%s%s %s"
                                (aif ein:jupyter-server-use-subcommand
                                    (concat it " ") "")
                                "list" "--json")))
                with token0
                with password0
                when (destructuring-bind
                         (&key password url token &allow-other-keys)
                         (ein:json-read-from-string line)
                       (prog1 (or (equal (ein:url url) url-or-port)
                                  (equal (url-host (url-generic-parse-url url))
                                         "0.0.0.0"))
                         (setq password0 password) ;; t or :json-false
                         (setq token0 token)))
                return (list password0 token0))
      it (list nil nil)))

;;;###autoload
(defun ein:jupyter-crib-running-servers ()
  "Shell out to jupyter for running servers."
  (cl-loop for line in
           (apply #'ein:jupyter-process-lines nil
                  ein:jupyter-server-command
                  (split-string
                   (format "%s%s %s"
                           (aif ein:jupyter-server-use-subcommand
                               (concat it " ") "")
                           "list" "--json")))
           collecting (destructuring-bind
                          (&key url &allow-other-keys)
                          (ein:json-read-from-string line)
                        (ein:url url))))

;;;###autoload
(defun ein:jupyter-server-start (server-command
                                 notebook-directory
                                 &optional no-login-p login-callback port)
  "Start SERVER-COMMAND with `--notebook-dir' NOTEBOOK-DIRECTORY.

Login after connection established unless NO-LOGIN-P is set.
LOGIN-CALLBACK takes two arguments, the buffer created by
`ein:notebooklist-open--finish', and the url-or-port argument
of `ein:notebooklist-open*'.

With \\[universal-argument] prefix arg, prompt the user for the
server command."
  (interactive
   (list (let ((default-command (executable-find ein:jupyter-server-command)))
           (if (and (not ein:jupyter-use-containers)
                    (or current-prefix-arg (not default-command)))
               (let (command result)
                 (while (not (setq
                              result
                              (executable-find
                               (setq
                                command
                                (read-string
                                 (format
                                  "%sServer command: "
                                  (if command
                                      (format "[%s not executable] " command)
                                    ""))
                                 nil nil ein:jupyter-server-command))))))
                 result)
             default-command))
         (let (result
               (default-dir ein:jupyter-default-notebook-directory))
           (while (or (not result) (not (file-directory-p result)))
             (setq result (read-directory-name
                           (format "%sNotebook directory: "
                                   (if result
                                       (format "[%s not a directory]" result)
                                     ""))
                           default-dir default-dir t)))
           result)
         nil
         (lambda (buffer url-or-port)
           (pop-to-buffer buffer))
         nil))
  (if (ein:jupyter-server-process)
      (error "ein:jupyter-server-start: please first M-x ein:stop"))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (ignore-errors (ein:jupyter-server-stop t))))
  (let ((proc (ein:jupyter-server--run *ein:jupyter-server-buffer-name*
                                       server-command
                                       notebook-directory
                                       (if (numberp port)
                                           `("--port" ,(format "%s" port)
                                             "--port-retries" "0")))))
    (cl-loop repeat 30
          until (car (ein:jupyter-server-conn-info *ein:jupyter-server-buffer-name*))
          do (sleep-for 0 500)
          finally do
          (unless (car (ein:jupyter-server-conn-info *ein:jupyter-server-buffer-name*))
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
  (interactive)
  (ein:and-let* ((url-or-port (first (ein:jupyter-server-conn-info)))
                 (_ok (or force (y-or-n-p "Stop server and close notebooks?"))))
    (ein:notebook-close-notebooks t)
    (cl-loop repeat 10
             until (not (seq-some (lambda (proc)
                                    (cl-search "request curl"
                                               (process-name proc)))
                                  (process-list)))
             do (sleep-for 0 500))
    (lexical-let* ((proc (ein:jupyter-server-process))
                   (pid (process-id proc)))
      (if (eq system-type 'windows-nt)
          (ein:query-singleton-ajax
           (ein:url url-or-port "api/shutdown")
           :type "POST")
        (ein:log 'info "Signaled %s with pid %s" proc pid)
        (signal-process pid 15))
      (run-at-time 2 nil
                   (lambda ()
                     (ein:log 'info "Resignaled %s with pid %s" proc pid)
                     (signal-process pid (if (eq system-type 'windows-nt) 9 15)))))

    ;; `ein:notebooklist-sentinel' frequently does not trigger
    (ein:notebooklist-list-remove url-or-port)
    (kill-buffer (ein:notebooklist-get-buffer url-or-port))
    (when log
      (with-current-buffer *ein:jupyter-server-buffer-name*
        (write-region (point-min) (point-max) log)))))

(provide 'ein-jupyter)
