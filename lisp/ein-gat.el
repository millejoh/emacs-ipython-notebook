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

(require 'with-editor)

(declare-function magit--process-coding-system "magit-process")
(declare-function magit-call-process "magit-process")
(declare-function magit-start-process "magit-process")
(declare-function magit-process-sentinel "magit-process")
(declare-function magit-with-editor "magit-git")

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

(defun ein:gat-where-am-i (&optional print-message)
  (interactive "p")
  (if (ein:get-notebook)
      (let ((where (file-name-as-directory default-directory)))
	(prog1 where
	  (when print-message
	    (message where))))
    (prog1 nil
      (when print-message
	(message "nowhere")))))

(defun ein:gat-call-gat (&rest args)
  "Return exit status returned by `call-process'."
  (interactive (split-string (read-string "gat ")))
  (if-let ((default-directory (ein:gat-where-am-i)))
      (let ((default-process-coding-system (magit--process-coding-system))
	    (args `("--project" ,ein:gat-project
		    "--region" ,ein:gat-region
		    "--zone" ,ein:gat-zone
		    ,@args))
	    (process-environment (cons (concat "GOOGLE_APPLICATION_CREDENTIALS=" (getenv "GAT_APPLICATION_CREDENTIALS"))
				       process-environment)))
	(apply #'magit-call-process "gat" args))
    (ein:log 'error "ein:gat-call-gat: cannot ascertain cwd")
    -1))

(defvar magit-process-popup-time)
(defvar inhibit-magit-refresh)
(defun ein:gat-chain (buffer callback &rest args)
  (declare (indent 0))
  (let* ((default-process-coding-system (magit--process-coding-system))
	 (inhibit-magit-refresh t)
	 (process-environment (cons (concat "GOOGLE_APPLICATION_CREDENTIALS="
					    (getenv "GAT_APPLICATION_CREDENTIALS"))
				    process-environment))
	 (process (apply #'magit-start-process args)))
    (set-process-sentinel process
			  (lambda (proc event)
			    (magit-process-sentinel proc event)
			    (if (zerop (process-exit-status proc))
				(when callback
				  (with-current-buffer buffer
				    (let ((magit-process-popup-time 0))
				      (funcall callback))))
			      (ein:log 'error "ein:gat-chain: %s exited %s"
				       (car args) (process-exit-status proc)))))
    process))

(defsubst ein:gat-run-local (&optional refresh)
  (interactive "P")
  (ein:gat--run-local-or-remote nil refresh))

(defsubst ein:gat-run-remote (&optional refresh)
  (interactive "P")
  (ein:gat--run-local-or-remote t refresh))

(defun ein:gat--run-local-or-remote (remote-p refresh)
  (when-let ((notebook (aand (ein:get-notebook)
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
						      (number-to-string it)))))
			      (list "run-local"))))
    (cl-destructuring-bind (pre-docker . post-docker) (ein:gat-dockerfiles-state)
      (if (or refresh (null pre-docker) (null post-docker))
	  (magit-with-editor
	    (let* ((dockerfile (format "Dockerfile.%s" (file-name-sans-extension notebook)))
		   (base-image (ein:gat-elicit-base-image))
		   (_ (with-temp-file dockerfile (insert (format "FROM %s\nCOPY ./%s .\nCMD [ \"start.sh\", \"jupyter\", \"nbconvert\", \"--ExecutePreprocessor.timeout=21600\", \"--to\", \"notebook\", \"--execute\", \"%s\" ]" base-image notebook notebook)))))
	      (ein:gat-chain
		(current-buffer)
		(apply #'apply-partially
		       #'ein:gat-chain
		       (current-buffer)
		       (apply #'apply-partially
			      #'ein:gat-chain
			      (current-buffer)
			      (apply #'apply-partially #'ein:gat-chain (current-buffer) nil
				     (append gat-chain-args (list "log" "-f")))
			      (append gat-chain-args gat-chain-run (list "--dockerfile" dockerfile)))
		       (append gat-chain-args (list "dockerfile" dockerfile)))
		with-editor-emacsclient-executable nil dockerfile)))
	(let ((magit-process-popup-time 0))
	  (apply #'ein:gat-chain (current-buffer)
		 (when remote-p
		   (apply #'apply-partially #'ein:gat-chain (current-buffer) nil
			  (append gat-chain-args (list "log" "-f"))))
		 (append gat-chain-args gat-chain-run (list "--dockerfile" pre-docker))))))))

(defcustom ein:gat-base-images '("jupyter/all-spark-notebook"
				 "jupyter/base-notebook"
				 "jupyter/datascience-notebook"
				 "jupyter/minimal-notebook"
				 "jupyter/pyspark-notebook"
				 "jupyter/r-notebook"
				 "jupyter/scipy-notebook"
				 "jupyter/tensorflow-notebook")
  "Known https://hub.docker.com/u/jupyter images."
  :type '(repeat (string :tag "FROM-appropriate docker image"))
  :group 'ein)

(defun ein:gat-elicit-base-image ()
  "Using a defcustom as HIST is suspect but pithy."
  (ein:completing-read
   "FROM image: " ein:gat-base-images nil 'confirm
   nil 'ein:gat-base-images (car ein:gat-base-images)))

(defvar-local ein:gat-disksizegb-history '("default")
  "Hopefully notebook-specific history of user entered disk size.")

(defvar-local ein:gat-machine-history '("e2-standard-2")
  "Hopefully notebook-specific history of user entered machine type.")

(defun ein:gat-elicit-machine ()
  (interactive)
  (ein:completing-read
   "Machine Type: " ein:gat-machine-types nil t nil
   'ein:gat-machine-history (car ein:gat-machine-history)))

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
  (-if-let* ((notebook (ein:get-notebook))
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
