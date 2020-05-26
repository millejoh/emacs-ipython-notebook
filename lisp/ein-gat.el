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

(defsubst ein:gat-where-am-i ()
  (when (ein:get-notebook)
    (file-name-as-directory default-directory)))

(defun ein:gat-call-gat (&rest args)
  (interactive (split-string (read-string "gat ")))
  (if-let ((default-directory (ein:gat-where-am-i)))
      (let ((default-process-coding-system (magit--process-coding-system))
	    (gat-executable "gat")
	    (args `("--project" ,ein:gat-project
		    "--region" ,ein:gat-region
		    "--zone" ,ein:gat-zone
		    ,@args)))
	(apply #'magit-call-process gat-executable args))
    (error "ein:gat-call-gat: cannot ascertain cwd")))

;; logic to handle the bash
;; bespoke elisp wrappers for each gat chunk
;; magit generally does not halt a sequence of `magit-call-git' calls for errors

(defun ein:gat-run-local ()
  (interactive)
  ;; run-local calls build calls dockerfile
  (ein:gat-call-gat "dockerfile"))

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

(defun ein:gat-dockerfile-p ()
  "Is there a Dockerfile sitting in `ein:gat-where-am-i'?"
  (directory-files (ein:gat-where-am-i) nil "^Dockerfile"))

(provide 'ein-gat)
