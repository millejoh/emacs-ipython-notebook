;;; ein-gat.el --- hooks to gat -*- lexical-binding: t; -*-

;; Copyright (C) 2019 The Authors

;; Authors: dickmao <github id: dickmao>

;; This file is NOT part of GNU Emacs.

;; ein-gat.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-gat.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-gat.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'compile)
(require 'magit-process nil t)
(declare-function ein:jupyter-running-notebook-directory "ein-jupyter")

;; (declare-function magit--process-coding-system "magit-process")
;; (declare-function magit-call-process "magit-process")
;; (declare-function magit-start-process "magit-process")
;; (declare-function magit-process-sentinel "magit-process")

(defconst ein:gat-status-cd 7 "gat exits 7 if requiring a change directory.")

(defcustom ein:gat-python-command (if (equal system-type 'windows-nt)
                                      (or (executable-find "py")
                                          (executable-find "pythonw")
                                          "python")
                                    "python")
  "Python executable name."
  :type (append '(choice)
                (let (result)
                  (dolist (py '("python" "python3" "pythonw" "py")
                              result)
                    (setq result (append result `((const :tag ,py ,py))))))
                '((string :tag "Other")))
  :group 'ein)

(defsubst ein:gat-shell-command (command)
  (string-trim (shell-command-to-string (concat "2>/dev/null " command))))

(defcustom ein:gat-gce-zone (ein:gat-shell-command "gcloud config get-value compute/zone")
  "gcloud project zone."
  :type 'string
  :group 'ein)

(defcustom ein:gat-gce-region (ein:gat-shell-command "gcloud config get-value compute/region")
  "gcloud project region."
  :type 'string
  :group 'ein)

(defcustom ein:gat-aws-region (ein:gat-shell-command "aws configure get region")
  "gcloud project region."
  :type 'string
  :group 'ein)

(defcustom ein:gat-gce-project (ein:gat-shell-command "gcloud config get-value core/project")
  "gcloud project id."
  :type 'string
  :group 'ein)

(defcustom ein:gat-aws-machine-types (split-string (ein:gat-shell-command "aws ec2 describe-instance-type-offerings --location-type=region --page-size=1000 --filter Name=location,Values=us-east-2 --query 'sort_by(InstanceTypeOfferings, &InstanceType)[].InstanceType' --output text"))
  "gcloud machine types."
  :type '(repeat string)
  :group 'ein)

(defcustom ein:gat-gce-machine-types (split-string (ein:gat-shell-command (format "gcloud compute machine-types list --filter=\"zone:%s\" --format=\"value[terminator=' '](name)\"" ein:gat-gce-zone)))
  "gcloud machine types."
  :type '(repeat string)
  :group 'ein)

(defcustom ein:gat-gpu-types (split-string "nvidia-tesla-t4")
  "https://accounts.google.com/o/oauth2/auth?client_id=[client-id]&redirect_uri=urn:ietf:wg:oauth:2.0:oob&scope=https://www.googleapis.com/auth/compute&response_type=code
curl -d code=[page-code] -d client_id=[client-id] -d client_secret=[client-secret] -d redirect_uri=urn:ietf:wg:oauth:2.0:oob -d grant_type=authorization_code https://accounts.google.com/o/oauth2/token
curl -sLk -H \"Authorization: Bearer [access-token]\" https://compute.googleapis.com/compute/v1/projects/[project-id]/zones/[zone-id]/acceleratorTypes | jq -r -c '.items[].selfLink'"
  :type '(repeat string)
  :group 'ein)

(defcustom ein:gat-base-images '("dickmao/tensorflow-gpu"
                                 "dickmao/scipy-gpu"
                                 "dickmao/pytorch-gpu")
  "Known https://hub.docker.com/u/jupyter images."
  :type '(repeat (string :tag "FROM-appropriate docker image"))
  :group 'ein)

(defvar ein:gat-previous-worktree nil)

(defvar ein:gat-urls nil)

(defconst ein:gat-master-worktree "master")

(defvar ein:gat-current-worktree ein:gat-master-worktree)

(defvar-local ein:gat-disksizegb-history '("default")
  "Hopefully notebook-specific history of user entered disk size.")

(defvar-local ein:gat-gpus-history '("0")
  "Hopefully notebook-specific history of user entered gpu count.")

(defvar ein:gat-machine-history nil
  "Hopefully notebook-specific history of user entered machine type.")

(defun ein:gat-where-am-i (&optional print-message)
  (interactive "p")
  (cond ((and (string= major-mode "magit-process-mode")
              (string-prefix-p "ein-gat:" (buffer-name)))
         (aprog1 default-directory
           (when print-message
             (message it))))
        ((string= major-mode "ein:ipynb-mode")
         (aprog1 (directory-file-name (file-name-directory (buffer-file-name)))
           (when print-message
             (message it))))
        (t
         (if-let ((notebook-dir (ein:jupyter-running-notebook-directory))
                  (notebook (ein:get-notebook))
                  (where (directory-file-name
                          (concat (file-name-as-directory notebook-dir)
                                  (file-name-directory (ein:$notebook-notebook-path notebook))))))
             (aprog1 where
               (when print-message
                 (message it)))
           (prog1 nil
             (when print-message
	       (message "nowhere")))))))

(cl-defun ein:gat-jupyter-login (ipynb-name notebook-dir callback &rest args &key public-ip-address)
  (let ((url-or-port (ein:url (format "http://%s:8888" public-ip-address))))
    (setf (alist-get (intern url-or-port) ein:gat-urls) notebook-dir)
    (ein:login url-or-port
               (lambda (buffer url-or-port)
                 (pop-to-buffer buffer)
                 (ein:notebook-open url-or-port ipynb-name nil callback)))))

(defun ein:gat-process-filter (proc string)
  "Copied `magit-process-filter' with added wrinkle of `ansi-color'.
Advising `insert' in `magit-process-filter' is a little gross,
and moreover, how would I avoid messing `magit-process-filter' of other processes?"
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (when-let ((ret-pos (cl-position ?\r string :from-end t)))
        (cl-callf substring string (1+ ret-pos))
        (delete-region (line-beginning-position) (point)))
      (insert (propertize (ansi-color-filter-apply string) 'magit-section
                          (process-get proc 'section)))
      (set-marker (process-mark proc) (point)))))

;; (defvar magit-process-popup-time)
;; (defvar inhibit-magit-refresh)
;; (defvar magit-process-raise-error)
;; (defvar magit-process-display-mode-line-error)
(cl-defun ein:gat-chain (buffer callback exec &rest args &key public-ip-address notebook-dir &allow-other-keys)
  (declare (indent 0))
  (let* ((default-directory (or notebook-dir (ein:gat-where-am-i)))
         (default-process-coding-system (magit--process-coding-system))
	 (inhibit-magit-refresh t)
	 (_ (awhen (getenv "GAT_APPLICATION_CREDENTIALS")
              (push (concat "GOOGLE_APPLICATION_CREDENTIALS=" it) process-environment)))
         (activate-with-editor-mode
          (when (string= (car exec) with-editor-emacsclient-executable)
            (lambda () (when (string= (buffer-name) (car (last exec)))
                         (with-editor-mode 1)))))
         (process (let ((magit-buffer-name-format "%xein-gat%v: %t%x"))
                    (apply #'magit-start-process exec))))
    (when activate-with-editor-mode
      (add-hook 'find-file-hook activate-with-editor-mode))
    ;; (with-current-buffer (process-buffer process)
    ;;   (special-mode))
    (with-editor-set-process-filter process #'ein:gat-process-filter)
    (set-process-sentinel
     process
     (lambda (proc event)
       (let* ((gat-status (process-exit-status proc))
              (process-buf (process-buffer proc))
              (section (process-get proc 'section))
              (gat-status-cd-p (= gat-status ein:gat-status-cd))
              worktree-dir new-public-ip-address)
         (when activate-with-editor-mode
           (remove-hook 'find-file-hook activate-with-editor-mode))
	 (let ((magit-process-display-mode-line-error
		(if gat-status-cd-p nil magit-process-display-mode-line-error))
	       (magit-process-raise-error
		(if gat-status-cd-p nil magit-process-raise-error))
               (short-circuit (lambda (&rest _args) (when gat-status-cd-p 0))))
           (add-function :before-until (symbol-function 'process-exit-status)
                         short-circuit)
           (unwind-protect
               (magit-process-sentinel proc event)
             (remove-function (symbol-function 'process-exit-status) short-circuit)))
	 (cond
          ((or (zerop gat-status) gat-status-cd-p)
           (alet (and (bufferp process-buf)
                      (with-current-buffer process-buf
                        (buffer-substring-no-properties (oref section content)
                                                        (oref section end))))
             (when it
               (when gat-status-cd-p
                 (setq worktree-dir (when (string-match "^cd\\s-+\\(\\S-+\\)" it)
                                          (string-trim (match-string 1 it)))))
               (when-let ((last-line (car (last (split-string (string-trim it) "\n")))))
                 (setq new-public-ip-address
                       (when (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)\\s-+\\S-+$" last-line)
                         (string-trim (match-string 1 last-line))))))
             (when callback
               (with-current-buffer buffer
                 (let ((magit-process-popup-time 0))
                   (apply callback
                          (append
                           (when worktree-dir
                             `(:worktree-dir ,worktree-dir))
                           (when-let ((address (or new-public-ip-address
                                                   public-ip-address)))
                             `(:public-ip-address ,address)))))))))
          (t
           (ein:log 'error "ein:gat-chain: %s exited %s"
		     (car exec) (process-exit-status proc)))))))
    process))

(defun ein:gat--path (archepath worktree-dir)
  "Form new relative path from ARCHEPATH root, WORKTREE-DIR subroot, and ARCHEPATH leaf.

With WORKTREE-DIR of 3/4/1/2/.gat/fantab,
1/2/eager.ipynb -> 1/2/.gat/fantab/eager.ipynb
1/2/.gat/fubar/subdir/eager.ipynb -> 1/2/.gat/fantab/subdir/eager.ipynb

With WORKTREE-DIR of /home/dick/gat/test-repo2
.gat/getout/eager.ipynb -> eager.ipynb
"
  (when-let ((root (directory-file-name (or (awhen (cl-search ".gat/" archepath :from-end)
                                              (cl-subseq archepath 0 it))
                                            (file-name-directory archepath)
                                            ""))))
    (if (zerop (length root))
        (concat (replace-regexp-in-string
                 "^\\./" ""
                 (file-name-as-directory
                  (cl-subseq worktree-dir
                             (or (cl-search ".gat/" worktree-dir :from-end)
                                 (length worktree-dir)))))
                (file-name-nondirectory archepath))
      (concat (file-name-as-directory
               (cl-subseq worktree-dir
                          (cl-search root worktree-dir :from-end)))
              (or (awhen (string-match "\\(\\.gat/[^/]+/\\)" archepath)
                    (cl-subseq archepath (+ it (length (match-string 1 archepath)))))
                  (file-name-nondirectory archepath))))))

(defun ein:gat-edit (&optional _refresh)
  (interactive "P")
  (ein:gat-install-gat
   (if-let ((default-directory (ein:gat-where-am-i))
            (notebook (ein:get-notebook))
            (gat-chain-args `("gat" nil "--project" "-"
                              "--region" ,ein:gat-aws-region "--zone" "-")))
       (if (special-variable-p 'magit-process-popup-time)
           (let ((magit-process-popup-time -1))
             (ein:gat-chain
               (current-buffer)
               (cl-function
                (lambda (&rest args &key worktree-dir &allow-other-keys)
                  (ein:notebook-open
                   (ein:$notebook-url-or-port notebook)
                   (ein:gat--path (ein:$notebook-notebook-path notebook)
                                  worktree-dir)
                   (ein:$notebook-kernelspec notebook))))
               (append gat-chain-args
                       (list "edit"
                             (alet (ein:gat-elicit-worktree t)
                               (setq ein:gat-previous-worktree ein:gat-current-worktree)
                               (setq ein:gat-current-worktree it))))))
         (error "ein:gat-create: magit not installed"))
     (message "ein:gat-edit: not a notebook buffer"))))

(defun ein:gat-create (&optional _refresh)
  (interactive "P")
  (ein:gat-install-gat
   (if-let ((default-directory (ein:gat-where-am-i))
            (notebook (ein:get-notebook))
            (gat-chain-args `("gat" nil "--project" "-"
                              "--region" ,ein:gat-aws-region "--zone" " -")))
       (if (special-variable-p 'magit-process-popup-time)
           (let ((magit-process-popup-time 0))
             (ein:gat-chain
               (current-buffer)
               (cl-function
                (lambda (&rest args &key worktree-dir &allow-other-keys)
                  (ein:notebook-open
                   (ein:$notebook-url-or-port notebook)
                   (ein:gat--path (ein:$notebook-notebook-path notebook)
                                  worktree-dir)
                   (ein:$notebook-kernelspec notebook))))
               (append gat-chain-args
                       (list "create"
                             (alet (ein:gat-elicit-worktree nil)
                               (setq ein:gat-previous-worktree ein:gat-current-worktree)
                               (setq ein:gat-current-worktree it))))))
         (error "ein:gat-create: magit not installed"))
     (message "ein:gat-create: not a notebook buffer"))))

(defmacro ein:gat-install-gat (&rest body)
  `(if (executable-find "gat")
       (progn ,@body)
     (if (or (not (executable-find "aws"))
             (zerop (length ein:gat-aws-region)))
         (ein:log 'error "ein:gat-install-gat: `aws configure region` must be valid")
       (ein:log 'info "ein:gat-install-gat: Installing gat...")
       (let* ((orig-buf (current-buffer))
              (bufname "*gat-install*")
              (dir (make-temp-file "gat-install" t))
              (commands `(,(format "cd %s" dir)
                          "git clone --depth=1 --single-branch --branch=dev https://github.com/dickmao/gat.git"
                          "make -C gat install"))
              (compile (format "bash -ex -c '%s'" (mapconcat #'identity commands "; ")))
              (callback (lambda (_buf msg)
                          (when (cl-search "finished" msg)
                            (with-current-buffer orig-buf
                              ,@body)))))
         (let ((compilation-scroll-output t))
           (compilation-start compile nil (lambda (&rest _args) bufname)))
         (with-current-buffer bufname
           (add-hook 'compilation-finish-functions callback nil t))))))

;;;###autoload
(defun ein:gat-run-local-batch (&optional refresh)
  (interactive "P")
  (ein:gat--run nil t refresh))

;;;###autoload
(defun ein:gat-run-local (&optional refresh)
  (interactive "P")
  (ein:gat--run nil nil refresh))

;;;###autoload
(defun ein:gat-run-remote-batch (&optional refresh)
  (interactive "P")
  (ein:gat--run t t refresh))

;;;###autoload
(defun ein:gat-run-remote (&optional refresh)
  (interactive "P")
  (ein:gat--run t nil refresh))

(defun ein:gat-hash-password (raw-password)
  (let ((gat-hash-password-python
         (format "%s - <<EOF
from notebook.auth import passwd
print(passwd('%s', 'sha1'))
EOF
" ein:gat-python-command raw-password)))
    (ein:gat-shell-command gat-hash-password-python)))

(defun ein:gat-crib-password ()
  (let* ((gat-crib-password-python
          (format "%s - <<EOF
from traitlets.config.application import Application
from traitlets import Unicode
class NotebookApp(Application):
  password = Unicode(u'', config=True,)

app = NotebookApp()
app.load_config_file('jupyter_notebook_config.py', '~/.jupyter')
print(app.password)
EOF
" ein:gat-python-command))
         (config-dir
          (elt (assoc-default
                'config
                (ein:json-read-from-string (ein:gat-shell-command "jupyter --paths --json")))
               0))
         (config-json (expand-file-name "jupyter_notebook_config.json" config-dir))
         (config-py (expand-file-name "jupyter_notebook_config.py" config-dir))
         password)
    (when (file-exists-p config-py)
      (setq password
            (awhen (ein:gat-shell-command gat-crib-password-python)
              (unless (zerop (length it)) it))))
    (unless (stringp password)
      (when (file-exists-p config-json)
        (-let* (((&alist 'NotebookApp (&alist 'password))
                 (json-read-file config-json)))
          password)))
    password))

(defun ein:gat-kaggle-env (var json-key)
  (when-let ((val (or (getenv var)
                      (let ((json (expand-file-name "kaggle.json" "~/.kaggle")))
                        (when (file-exists-p json)
                          (assoc-default json-key (json-read-file json)))))))
    (format "--env %s=%s" var val)))

(defun ein:gat--run (remote-p batch-p refresh)
  (unless with-editor-emacsclient-executable
    (error "Could not determine emacsclient"))
  (ein:gat-install-gat
   (-if-let* ((ipynb-name
               (if (string= major-mode "ein:ipynb-mode")
                   (file-name-nondirectory (buffer-file-name))
                 (awhen (aand (ein:get-notebook) (ein:$notebook-notebook-name it)) it)))
              (callback
               (if (string= major-mode "ein:ipynb-mode")
                   (apply-partially (lambda (buffer*
                                             _notebook _created
                                             &rest _args)
                                      (when (buffer-live-p buffer*)
                                        (kill-buffer-if-not-modified buffer*)))
                                    (current-buffer))
                 #'ignore))
              (default-directory (ein:gat-where-am-i))
              (password (if (or batch-p (not remote-p))
                            ""
                          (or (ein:gat-crib-password)
                              (let ((new-password
                                     (read-passwd "Enter new password for remote server [none]: " t)))
                                (if (zerop (length new-password))
                                    new-password
                                  (let ((hashed (ein:gat-hash-password new-password)))
                                    (if (cl-search ":" hashed)
                                        hashed
                                      (prog1 nil
                                        (ein:log 'error "ein:gat--run: %s %s"
                                                 "Could not hash" new-password)))))))))
              (gat-chain-args `("gat" nil
                                "--project" "-"
                                "--region" ,ein:gat-aws-region
                                "--zone" "-"))
              (common-options (append `("--user" "root")
                                      `("--env" "GRANT_SUDO=1")
                                      (awhen (ein:gat-kaggle-env "KAGGLE_USERNAME" 'username)
                                        (split-string it))
                                      (awhen (ein:gat-kaggle-env "KAGGLE_KEY" 'key)
                                        (split-string it))
                                      (awhen (ein:gat-kaggle-env "KAGGLE_NULL" 'null)
                                        (split-string it))))
              (gat-chain-run (if remote-p
                                 (append '("run-remote")
                                         common-options
                                         `("--machine" ,(ein:gat-elicit-machine))
                                         `(,@(aif (ein:gat-elicit-disksizegb)
                                                 (list "--disksizegb"
                                                       (number-to-string it))))
                                         `(,@(-when-let* ((gpus (ein:gat-elicit-gpus))
                                                          (nonzero (not (zerop gpus))))
                                               (list "--gpus"
                                                     (number-to-string gpus)))))
                               (append '("run-local") common-options)))
              (now (truncate (float-time)))
              (gat-log-exec (append gat-chain-args
                                    (list "log" "--after" (format "%s" now)
                                          "--until" "is running at:"
                                          "--nextunit" "shutdown.service")))
              (command (cond (batch-p
                              (format "start.sh jupyter nbconvert --ExecutePreprocessor.timeout=21600 --to notebook --execute %s" ipynb-name))
                             ((zerop (length password))
                              (format "start-notebook.sh --NotebookApp.token=''"))
                             (t
                              (format "start-notebook.sh --NotebookApp.password='%s'" password)))))
       (cl-destructuring-bind (pre-docker . post-docker) (ein:gat-dockerfiles-state)
         (if (or refresh (null pre-docker))
             (if (fboundp 'magit-with-editor)
                 (magit-with-editor
                   (let* ((dockerfile (format "Dockerfile.%s" (file-name-sans-extension ipynb-name)))
                          (base-image (ein:gat-elicit-base-image))
                          (_ (with-temp-file dockerfile
                               (insert (format "FROM %s\nCOPY --chown=jovyan:users ./%s .\n"
                                               base-image ipynb-name))))
                          (my-editor (when (and (boundp 'server-name)
                                                (server-running-p server-name))
                                       `("-s" ,server-name))))
                     (ein:gat-chain
                       (current-buffer)
                       (apply-partially
                        #'ein:gat-chain
                        (current-buffer)
                        (when remote-p
                          (apply-partially
                           #'ein:gat-chain
                           (current-buffer)
                           (unless batch-p
                             (apply-partially #'ein:gat-jupyter-login ipynb-name default-directory callback))
                           gat-log-exec))
                        (append gat-chain-args gat-chain-run (list "--dockerfile" dockerfile "--command" command)))
                       `(,with-editor-emacsclient-executable nil ,@my-editor ,dockerfile))))
               (error "ein:gat--run: magit not installed"))
           (if (special-variable-p 'magit-process-popup-time)
               (let ((magit-process-popup-time 0))
                 (ein:gat-chain
                   (current-buffer)
                   (when remote-p
                     (apply-partially
                      #'ein:gat-chain
                      (current-buffer)
                      (unless batch-p
                        (apply-partially #'ein:gat-jupyter-login ipynb-name default-directory callback))
                      gat-log-exec))
                   (append gat-chain-args gat-chain-run (list "--dockerfile" pre-docker "--command" command))))
             (error "ein:gat--run: magit not installed"))))
     (message "ein:gat--run: aborting"))))

(defun ein:gat-elicit-base-image ()
  "Using a defcustom as HIST is suspect but pithy."
  (ein:completing-read
   "FROM image: " ein:gat-base-images nil 'confirm
   nil 'ein:gat-base-images (car ein:gat-base-images)))

(defun ein:gat-elicit-machine ()
  (interactive)
  (require 'seq)
  (ein:completing-read
   "Machine Type: "
   (append (seq-uniq ein:gat-machine-history)
           (seq-remove (lambda (x) (member x ein:gat-machine-history))
                       (cl-copy-list ein:gat-aws-machine-types)))
   nil t nil 'ein:gat-machine-history
   (car (or ein:gat-machine-history ein:gat-aws-machine-types))))

(defun ein:gat-elicit-gpus (&rest _args)
  (interactive)
  (cl-loop for answer =
	   (string-to-number (ein:completing-read
			      "Number GPUs: " '("0") nil nil nil
			      'ein:gat-gpus-history (car ein:gat-gpus-history)))
	   until (>= answer 0)
	   finally return answer))
(add-function :override (symbol-function 'ein:gat-elicit-gpus) #'ignore)

(defun ein:gat-elicit-worktree (extant)
  (let ((already (split-string
                  (ein:gat-shell-command
                   (format "gat --project %s --region %s --zone - list"
                           "-" ein:gat-aws-region)))))
    (if extant
        (ein:completing-read
         "Experiment: " already nil t nil nil
         ein:gat-previous-worktree)
      (read-string "New experiment: "))))

(defun ein:gat-elicit-disksizegb ()
  "Return nil for default [currently max(8, 6 + image size)]."
  (interactive)
  (cl-loop with answer
	   do (setq answer (ein:completing-read
			    "Disk GiB: " '("default") nil nil nil
			    'ein:gat-disksizegb-history (car ein:gat-disksizegb-history)))
	   if (string= answer "default")
	   do (setq answer nil)
	   else
	   do (setq answer (string-to-number answer))
	   end
	   until (or (null answer) (> answer 0))
	   finally return answer))

(defun ein:gat-dockerfiles-state ()
  "Return cons of (pre-Dockerfile . post-Dockerfile).
Pre-Dockerfile is Dockerfile.<notebook> if extant, else Dockerfile."
  (-if-let* ((default-directory (ein:gat-where-am-i))
	     (notebook-name (cond ((string= major-mode "ein:ipynb-mode")
                                   (file-name-nondirectory (buffer-file-name)))
                                  (t
                                   (aand (ein:get-notebook) (ein:$notebook-notebook-name it)))))
	     (dockers (directory-files (file-name-as-directory default-directory)
				       nil "^Dockerfile")))
      (let* ((pre-docker-p (lambda (f) (or (string= f (format "Dockerfile.%s" (file-name-sans-extension notebook-name)))
					   (string= f "Dockerfile"))))
	     (pre-docker (seq-find pre-docker-p (sort (cl-copy-list dockers) #'string>)))
	     (post-docker-p (lambda (f) (string= f (format "%s.gat" pre-docker))))
	     (post-docker (and (stringp pre-docker) (seq-find post-docker-p (sort (cl-copy-list dockers) #'string>)))))
	`(,pre-docker . ,post-docker))
    '(nil)))

(provide 'ein-gat)
