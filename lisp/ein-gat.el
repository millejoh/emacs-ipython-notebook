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
	    (gat-executable "gat")
	    (args `("--project" ,ein:gat-project
		    "--region" ,ein:gat-region
		    "--zone" ,ein:gat-zone
		    ,@args))
	    (process-environment (cons (concat "GOOGLE_APPLICATION_CREDENTIALS=" (getenv "GAT_APPLICATION_CREDENTIALS"))
				       process-environment)))
	(apply #'magit-call-process gat-executable args))
    (ein:log 'error "ein:gat-call-gat: cannot ascertain cwd")
    -1))

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
				  (save-excursion
				    (with-current-buffer buffer
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
	     (gat-executable "gat")
	     (gat-chain-args (list (current-buffer) nil
				   gat-executable nil "--project" ein:gat-project
				   "--region" ein:gat-region "--zone" ein:gat-zone
				   (if remote-p "run-remote" "run-local"))))
    (cl-destructuring-bind (pre-docker . post-docker) (ein:gat-dockerfiles-state)
      (if (or refresh (null pre-docker) (null post-docker))
	  (magit-with-editor
	    (let* ((dockerfile (format "Dockerfile.%s" (file-name-sans-extension notebook)))
		   (base-image (ein:gat-elicit-base-image))
		   (_ (with-temp-file dockerfile (insert (format "FROM %s\nCOPY ./%s .\nCMD [ \"start.sh\", \"jupyter\", \"nbconvert\", \"--to\", \"notebook\", \"--execute\", \"%s\" ]" base-image notebook notebook)))))
	      (ein:gat-chain
		(current-buffer)
		(apply-partially
		 #'ein:gat-chain
		 (current-buffer)
		 (apply #'apply-partially #'ein:gat-chain
			(append gat-chain-args (list "--dockerfile" dockerfile)))
		 gat-executable nil "--project" ein:gat-project
		 "--region" ein:gat-region "--zone" ein:gat-zone
		 "dockerfile" dockerfile)
		with-editor-emacsclient-executable nil dockerfile)))
	(let ((magit-process-popup-time 0))
	  (apply #'ein:gat-chain (append gat-chain-args
					 (list "--dockerfile" pre-docker))))))))

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
