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

(require 'magit-process nil t)
(declare-function ein:jupyter-running-notebook-directory "ein-jupyter")

;; (declare-function magit--process-coding-system "magit-process")
;; (declare-function magit-call-process "magit-process")
;; (declare-function magit-start-process "magit-process")
;; (declare-function magit-process-sentinel "magit-process")

(defconst ein:gat-status-cd 7 "gat exits 7 if requiring a change directory.")
(defcustom ein:gat-zone (string-trim (shell-command-to-string
				      "gcloud config get-value compute/zone"))
  "gcloud project zone."
  :type 'string
  :group 'ein)

(defcustom ein:gat-region (string-trim (shell-command-to-string
					"gcloud config get-value compute/region"))
  "gcloud project region."
  :type 'string
  :group 'ein)

(defcustom ein:gat-project (string-trim (shell-command-to-string
					 "gcloud config get-value core/project"))
  "gcloud project id."
  :type 'string
  :group 'ein)

(defcustom ein:gat-machine-types (split-string (string-trim (shell-command-to-string (format "gcloud compute machine-types list --filter=\"zone:%s\" --format=\"value[terminator=' '](name)\"" ein:gat-zone))))
  "gcloud machine types."
  :type '(repeat string)
  :group 'ein)

(defcustom ein:gat-gpu-types (split-string "nvidia-tesla-t4")
  "https://accounts.google.com/o/oauth2/auth?client_id=[client-id]&redirect_uri=urn:ietf:wg:oauth:2.0:oob&scope=https://www.googleapis.com/auth/compute&response_type=code
curl -d code=[page-code] -d client_id=[client-id] -d client_secret=[client-secret] -d redirect_uri=urn:ietf:wg:oauth:2.0:oob -d grant_type=authorization_code https://accounts.google.com/o/oauth2/token
curl -sLk -H \"Authorization: Bearer [access-token]\" https://compute.googleapis.com/compute/v1/projects/[project-id]/zones/[zone-id]/acceleratorTypes | jq -r -c '.items[].selfLink'"
  :type '(repeat string)
  :group 'ein)

(defcustom ein:gat-base-images '("jupyter/scipy-notebook"
                                 "jupyter/tensorflow-notebook"
				 "jupyter/datascience-notebook"
				 "jupyter/r-notebook"
				 "jupyter/minimal-notebook"
				 "jupyter/base-notebook"
				 "jupyter/pyspark-notebook"
                                 "jupyter/all-spark-notebook")
  "Known https://hub.docker.com/u/jupyter images."
  :type '(repeat (string :tag "FROM-appropriate docker image"))
  :group 'ein)

(defvar ein:gat-previous-worktree nil)

(defconst ein:gat-master-worktree "master")

(defvar ein:gat-current-worktree ein:gat-master-worktree)

(defvar-local ein:gat-disksizegb-history '("default")
  "Hopefully notebook-specific history of user entered disk size.")

(defvar-local ein:gat-gpus-history '("0")
  "Hopefully notebook-specific history of user entered gpu count.")

(defvar-local ein:gat-machine-history '("e2-standard-2")
  "Hopefully notebook-specific history of user entered machine type.")

(defun ein:gat-where-am-i (&optional print-message)
  (interactive "p")
  (if-let ((notebook-dir (ein:jupyter-running-notebook-directory))
           (notebook (ein:get-notebook))
           (where (directory-file-name
                   (concat (file-name-as-directory notebook-dir)
                           (file-name-directory (ein:$notebook-notebook-path notebook))))))
      (prog1 where
        (when print-message
          (message where)))
    (prog1 nil
      (when print-message
	(message "nowhere")))))

;; (defvar magit-process-popup-time)
;; (defvar inhibit-magit-refresh)
;; (defvar magit-process-raise-error)
;; (defvar magit-process-display-mode-line-error)
(defun ein:gat-chain (buffer callback &rest args)
  (declare (indent 0))
  (let* ((default-directory (ein:gat-where-am-i))
         (default-process-coding-system (magit--process-coding-system))
	 (inhibit-magit-refresh t)
	 (process-environment (cons (concat "GOOGLE_APPLICATION_CREDENTIALS="
					    (or (getenv "GAT_APPLICATION_CREDENTIALS")
                                                (error "GAT_APPLICATION_CREDENTIALS undefined")))
				    process-environment))
	 (process (apply #'magit-start-process args)))
    (set-process-sentinel
     process
     (lambda (proc event)
       (let* ((gat-status (process-exit-status proc))
              (process-buf (process-buffer proc))
              (section (process-get proc 'section))
              (gat-status-cd-p (= gat-status ein:gat-status-cd))
              new-worktree-dir)
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
           (when (and gat-status-cd-p (buffer-live-p process-buf))
             (alet (with-current-buffer process-buf
                     (buffer-substring-no-properties (oref section content)
                                                     (oref section end)))
               (setq new-worktree-dir (progn (string-match "^cd\\s-+\\(\\S-+\\)" it)
                                             (string-trim (match-string 1 it))))))
           (when callback
             (with-current-buffer buffer
               (let ((magit-process-popup-time 0))
                 (apply callback (when new-worktree-dir
                                   `(:worktree-dir ,new-worktree-dir)))))))
          (t
           (ein:log 'error "ein:gat-chain: %s exited %s"
		     (car args) (process-exit-status proc)))))))
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
  (if-let ((default-directory (ein:gat-where-am-i))
           (notebook (ein:get-notebook))
           (gat-chain-args `("gat" nil "--project" ,ein:gat-project
                             "--region" ,ein:gat-region "--zone"
                             ,ein:gat-zone)))
      (if (special-variable-p 'magit-process-popup-time)
          (let ((magit-process-popup-time -1))
            (apply #'ein:gat-chain (current-buffer)
                   (cl-function
                    (lambda (&rest args &key worktree-dir)
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
    (message "ein:gat-edit: not a notebook buffer")))

(defun ein:gat-create (&optional _refresh)
  (interactive "P")
  (if-let ((default-directory (ein:gat-where-am-i))
           (notebook (ein:get-notebook))
           (gat-chain-args `("gat" nil "--project" ,ein:gat-project
                             "--region" ,ein:gat-region "--zone"
                             ,ein:gat-zone)))
      (if (special-variable-p 'magit-process-popup-time)
          (let ((magit-process-popup-time 0))
            (apply #'ein:gat-chain (current-buffer)
                   (cl-function
                    (lambda (&rest args &key worktree-dir)
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
    (message "ein:gat-create: not a notebook buffer")))

(defsubst ein:gat-run-local (&optional refresh)
  (interactive "P")
  (ein:gat--run-local-or-remote nil refresh))

(defsubst ein:gat-run-remote (&optional refresh)
  (interactive "P")
  (ein:gat--run-local-or-remote t refresh))

(defun ein:gat--run-local-or-remote (remote-p refresh)
  (unless with-editor-emacsclient-executable
    (error "Could not determine emacsclient"))
  (if-let ((default-directory (ein:gat-where-am-i))
           (notebook (aand (ein:get-notebook)
                           (ein:$notebook-notebook-name it)))
           (gat-chain-args `("gat" nil "--project" ,ein:gat-project
                             "--region" ,ein:gat-region "--zone"
                             ,ein:gat-zone))
           (gat-chain-run (if remote-p
                              (append '("run-remote")
                                      `("--machine"
                                        ,(ein:gat-elicit-machine))
                                      `(,@(aif (ein:gat-elicit-disksizegb)
                                              (list "--disksizegb"
                                                    (number-to-string it))))
                                      `(,@(-when-let* ((gpus (ein:gat-elicit-gpus))
                                                       (nonzero (not (zerop gpus))))
                                            (list "--gpus"
                                                  (number-to-string gpus)))))
                            (list "run-local"))))
      (cl-destructuring-bind (pre-docker . post-docker) (ein:gat-dockerfiles-state)
        (if (or refresh (null pre-docker) (null post-docker))
            (if (fboundp 'magit-with-editor)
                (magit-with-editor
                  (let* ((dockerfile (format "Dockerfile.%s" (file-name-sans-extension notebook)))
                         (base-image (ein:gat-elicit-base-image))
                         (_ (with-temp-file dockerfile (insert (format "FROM %s\nCOPY ./%s .\nCMD [ \"start.sh\", \"jupyter\", \"nbconvert\", \"--ExecutePreprocessor.timeout=21600\", \"--to\", \"notebook\", \"--execute\", \"%s\" ]" base-image notebook notebook))))
                         (my-editor (when (and (boundp 'server-name)
                                               (server-running-p server-name))
                                      `("-s" ,server-name))))
                    (apply #'ein:gat-chain
                           (current-buffer)
                           (apply #'apply-partially
                                  #'ein:gat-chain
                                  (current-buffer)
                                  (apply #'apply-partially
                                         #'ein:gat-chain
                                         (current-buffer)
                                         (when remote-p
                                           (apply #'apply-partially #'ein:gat-chain (current-buffer) nil
                                                  (append gat-chain-args (list "log" "-f"))))
                                         (append gat-chain-args gat-chain-run (list "--dockerfile" dockerfile)))
                                  (append gat-chain-args (list "dockerfile" dockerfile)))
                           `(,with-editor-emacsclient-executable nil ,@my-editor ,dockerfile))))
              (error "ein:gat--run-local-or-remote: magit not installed"))
          (if (special-variable-p 'magit-process-popup-time)
              (let ((magit-process-popup-time 0))
                (apply #'ein:gat-chain (current-buffer)
                       (when remote-p
                         (apply #'apply-partially #'ein:gat-chain (current-buffer) nil
                                (append gat-chain-args (list "log" "-f"))))
                       (append gat-chain-args gat-chain-run (list "--dockerfile" pre-docker))))
            (error "ein:gat--run-local-or-remote: magit not installed"))))
    (message "ein:gat--run-local-or-remote: not a notebook buffer")))

(defun ein:gat-elicit-base-image ()
  "Using a defcustom as HIST is suspect but pithy."
  (ein:completing-read
   "FROM image: " ein:gat-base-images nil 'confirm
   nil 'ein:gat-base-images (car ein:gat-base-images)))

(defun ein:gat-elicit-machine ()
  (interactive)
  (ein:completing-read
   "Machine Type: " ein:gat-machine-types nil t nil
   'ein:gat-machine-history (car ein:gat-machine-history)))

(defun ein:gat-elicit-gpus ()
  (interactive)
  (cl-loop for answer =
	   (string-to-number (ein:completing-read
			      "Number GPUs: " '("0") nil nil nil
			      'ein:gat-gpus-history (car ein:gat-gpus-history)))
	   until (>= answer 0)
	   finally return answer))

(defun ein:gat-elicit-worktree (extant)
  (let ((already (split-string
                  (string-trim
                   (shell-command-to-string
                    (format "gat --project %s --region %s --zone %s list"
                            ein:gat-project ein:gat-region ein:gat-zone))))))
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
             (notebook (ein:get-notebook))
	     (notebook-name (ein:$notebook-notebook-name notebook))
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
