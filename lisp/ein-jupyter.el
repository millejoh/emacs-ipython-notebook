;;; ein-jupyter.el --- Manage the jupyter notebook server    -*- lexical-binding:t -*-

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
(require 'exec-path-from-shell nil t)
(autoload 'ein:gat-chain "ein-gat")
(autoload 'ein:gat-project "ein-gat")
(autoload 'ein:gat-region "ein-gat")
(autoload 'ein:gat-zone "ein-gat")

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
  "Where in docker image to mount `ein:jupyter-default-notebook-directory'."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-docker-additional-switches "-e JUPYTER_ENABLE_LAB=no --rm"
  "Additional options to the 'docker run' call.

Note some options like '-v' and '-network' are imposed by EIN."
  :group 'ein
  :type 'string)

(defcustom ein:jupyter-cannot-find-jupyter nil
  "Use purcell's `exec-path-from-shell'"
  :group 'ein
  :type 'boolean)

(defcustom ein:jupyter-server-command "jupyter"
  "The default command to start a jupyter notebook server.
Changing this to `jupyter-notebook' requires customizing
`ein:jupyter-server-use-subcommand' to nil."
  :group 'ein
  :type 'string
  :set-after '(ein:jupyter-cannot-find-jupyter)
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (when (and (featurep 'exec-path-from-shell)
                    ein:jupyter-cannot-find-jupyter
		    (memq window-system '(mac ns x)))
           (eval `(let (,@(when (boundp 'exec-path-from-shell-check-startup-files)
                            (list 'exec-path-from-shell-check-startup-files)))
                    (exec-path-from-shell-initialize))))))

(defcustom ein:jupyter-default-server-command ein:jupyter-server-command
  "Obsolete alias for `ein:jupyter-server-command'"
  :group 'ein
  :type 'string
  :set-after '(ein:jupyter-server-command)
  :set (lambda (symbol value)
	 (set-default symbol value)
         (set-default 'ein:jupyter-server-command value)))

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
                  (json-read-from-string ;; intentionally not ein:json-read-from-string
                   (shell-command-to-string
                    (format "2>/dev/null %s kernelspec list --json"
                            ein:jupyter-server-command)))))
               collect `(,k . ,(alist-get 'display_name (alist-get 'spec spec)))))
           (error (ein:log 'warn "ein:jupyter-default-kernel: %s" err)
                  '((string :tag "Ask"))))))

(defconst *ein:jupyter-server-process-name* "ein server")
(defconst *ein:jupyter-server-buffer-name*
  (format "*%s*" *ein:jupyter-server-process-name*))
(defvar-local ein:jupyter-server-notebook-directory nil
  "Keep track of prevailing --notebook-dir argument.")

(defun ein:jupyter-running-notebook-directory ()
  (when (ein:jupyter-server-process)
    (buffer-local-value 'ein:jupyter-server-notebook-directory
                        (get-buffer *ein:jupyter-server-buffer-name*))))

(defun ein:jupyter-get-default-kernel (kernels)
  (cond (ein:%notebooklist-new-kernel%
         (ein:$kernelspec-name ein:%notebooklist-new-kernel%))
        ((eq ein:jupyter-default-kernel 'first-alphabetically)
         (car (car kernels)))
        ((stringp ein:jupyter-default-kernel)
         ein:jupyter-default-kernel)
        (t
         (symbol-name ein:jupyter-default-kernel))))

(defun ein:jupyter-process-lines (_url-or-port command &rest args)
  "If URL-OR-PORT registered as a k8s url, preface COMMAND ARGS with `kubectl exec'."
  (condition-case err
      (apply #'process-lines command args)
    (error (ein:log 'info "ein:jupyter-process-lines: %s" (error-message-string err))
           nil)))

(defsubst ein:jupyter-server-process ()
  "Return the emacs process object of our session."
  (get-buffer-process (get-buffer *ein:jupyter-server-buffer-name*)))

(defun ein:jupyter-server--run (buf user-cmd dir &optional args)
  (ein:with-read-only-buffer (get-buffer-create buf))
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
                               (when dir
                                 (list (format "--notebook-dir=%s"
                                               (convert-standard-filename dir))))
                               args
                               (let ((copy (cl-copy-list ein:jupyter-server-args)))
                                 (when (ein:debug-p)
                                   (cl-pushnew "--debug" copy))
                                 copy)))))
         (proc (apply #'start-process
                      *ein:jupyter-server-process-name* buf cmd vargs)))
    (ein:log 'info "ein:jupyter-server--run: %s %s" cmd (ein:join-str " " vargs))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun ein:jupyter-server-conn-info ()
  "Return the url-or-port and password for global session."
  (let ((result '(nil nil)))
    (when (ein:jupyter-server-process)
      (with-current-buffer *ein:jupyter-server-buffer-name*
        (save-excursion
          (goto-char (point-max))
          (re-search-backward (format "Process %s" *ein:jupyter-server-process-name*)
                              nil "") ;; important if we start-stop-start
          (-when-let* ((running-p
                        (and (re-search-forward
                              "\\([[:alnum:]]+\\) is\\( now\\)? running"
                              nil t)
                             (re-search-forward
                              "\\(https?://[^:]*:[0-9]+\\)\\(?:/\\?token=\\([[:alnum:]]+\\)\\)?"
                              nil t)))
                       (raw-url (match-string 1))
                       (token (or (match-string 2) "")))
            (setq result (list (ein:url raw-url) token))))))
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
      (if-let ((token (ein:notebooklist-token-or-password url-or-port)))
	  (ein:notebooklist-login url-or-port callback nil nil token)
	(ein:log 'error "`(ein:notebooklist-token-or-password %s)` must return non-nil"
		 url-or-port)))))

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
                        (format "2>/dev/null %s%s %s"
                                (aif ein:jupyter-server-use-subcommand
                                    (concat it " ") "")
                                "list" "--json")))
                with token0
                with password0
                when (cl-destructuring-bind
                         (&key password url token &allow-other-keys)
                         (ein:json-read-from-string line)
                       (prog1 (equal (ein:url url) url-or-port)
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
           collecting (cl-destructuring-bind
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
         (let ((default-dir ein:jupyter-default-notebook-directory)
               result)
           (while (or (not result) (not (file-directory-p result)))
             (setq result (read-directory-name
                           (format "%sNotebook directory: "
                                   (if result
                                       (format "[%s not a directory]" result)
                                     ""))
                           default-dir default-dir t)))
           result)
         nil
         (lambda (buffer _url-or-port)
           (pop-to-buffer buffer))
         nil))
  (when (ein:jupyter-server-process)
    (error "ein:jupyter-server-start: First `M-x ein:stop'"))
  (let ((proc (ein:jupyter-server--run *ein:jupyter-server-buffer-name*
                                       server-command
                                       notebook-directory
                                       (when (numberp port)
                                         `("--port" ,(format "%s" port)
                                           "--port-retries" "0")))))
    (cl-loop repeat 30
             until (car (ein:jupyter-server-conn-info))
             do (sleep-for 0 500)
             finally do
             (-if-let* ((buffer (get-buffer *ein:jupyter-server-buffer-name*))
                        (url-or-port (car (ein:jupyter-server-conn-info))))
                 (with-current-buffer buffer
                   (setq ein:jupyter-server-notebook-directory
                         (convert-standard-filename notebook-directory))
                   (add-hook 'kill-buffer-query-functions
                             (lambda () (or (not (ein:jupyter-server-process))
                                            (ein:jupyter-server-stop t url-or-port)))
                             nil t))
               (ein:log 'warn "Jupyter server failed to start, cancelling operation")))
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

(defvar ein:gat-urls)
(defvar ein:gat-aws-region)
;;;###autoload
(defun ein:jupyter-server-stop (&optional ask-p url-or-port)
  (interactive
   (list t (awhen (ein:get-notebook)
             (ein:$notebook-url-or-port it))))
  (let ((my-url-or-port (car (ein:jupyter-server-conn-info)))
        (all-p t))
    (dolist (url-or-port
             (if url-or-port (list url-or-port) (ein:notebooklist-keys))
             (prog1 all-p
               (when (and (null (ein:notebooklist-keys))
                          (ein:shared-output-healthy-p))
                 (kill-buffer (ein:shared-output-buffer)))))
      (let* ((gat-dir (alist-get (intern url-or-port)
				 (awhen (bound-and-true-p ein:gat-urls) it)))
             (my-p (string= url-or-port my-url-or-port))
             (close-p (or (not ask-p)
                          (prog1 (y-or-n-p (format "Close %s?" url-or-port))
                            (message "")))))
        (if (not close-p)
            (setq all-p nil)
          (ein:notebook-close-notebooks
           (lambda (notebook)
             (string= url-or-port (ein:$notebook-url-or-port notebook)))
           t)
          (cl-loop repeat 10
                   until (null (seq-some (lambda (proc)
                                           (cl-search "request curl"
                                                      (process-name proc)))
                                         (process-list)))
                   do (sleep-for 0 500))
          (cond (my-p
                 (-when-let* ((proc (ein:jupyter-server-process))
                              (pid (process-id proc)))
                   (run-at-time 2 nil
                                (lambda ()
                                  (signal-process pid (if (eq system-type 'windows-nt) 9 15))))
                   ;; NotebookPasswordApp::shutdown_server() also ignores req response.
                   (ein:query-singleton-ajax (ein:url url-or-port "api/shutdown")
                                             :type "POST")))
                (gat-dir
                 (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
                   (-when-let* ((gat-chain-args `("gat" nil
                                                  "--project" ,(ein:gat-project)
                                                  "--region" ,(ein:gat-region)
                                                  "--zone" ,(ein:gat-zone)))
                                (now (truncate (float-time)))
                                (gat-log-exec (append gat-chain-args
                                                      (list "log" "--after" (format "%s" now)
							    "--vendor" (aif (bound-and-true-p ein:gat-vendor) it "aws")
                                                            "--nextunit" "shutdown.service")))
                                (magit-process-popup-time 0))
                     (ein:gat-chain (current-buffer) nil gat-log-exec :notebook-dir gat-dir)
                     ;; NotebookPasswordApp::shutdown_server() also ignores req response.
                     (ein:query-singleton-ajax (ein:url url-or-port "api/shutdown")
                                               :type "POST")))))
          ;; `ein:notebooklist-sentinel' frequently does not trigger
          (ein:notebooklist-list-remove url-or-port)
          (maphash (lambda (k _v) (when (equal (car k) url-or-port)
                                    (remhash k *ein:notebook--pending-query*)))
                   *ein:notebook--pending-query*)
          (kill-buffer (ein:notebooklist-get-buffer url-or-port)))))))

(provide 'ein-jupyter)
