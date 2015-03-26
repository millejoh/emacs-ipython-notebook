#! /bin/sh
":"; exec ${EMACS:-emacs} --no-init -Q -l "$0" "$@" # -*-emacs-lisp-*-
;;; zeroein.el --- Zero setup Emacs IPython Notebook client

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; zeroein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; zeroein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with zeroein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))


;;; Utilities

(defvar zeroein:lisp-dir
  (or (if load-file-name (file-name-directory load-file-name))
      default-directory))

(defvar zeroein:root-dir
  (file-name-as-directory
   (expand-file-name ".." (file-name-as-directory zeroein:lisp-dir))))

(defun zeroein:path (p &rest ps)
  (if ps
      (apply #'zeroein:path
           (concat (file-name-as-directory p) (car ps)) (cdr ps))
    (concat zeroein:root-dir p)))

(defvar zeroein:dependencies
  '("ein-mumamo" "nxhtml" "markdown-mode" "websocket" "request"
    "auto-complete" "popup" "fuzzy" "pos-tip" "smartrep"))

;; Loading the new python.el fails in Emacs 23.
(when (>= emacs-major-version 24)
  (add-to-list 'zeroein:dependencies "python"))


;;; Install dependencies

(call-process "git" nil (get-buffer "*Messages*") nil
              "submodule" "update" "--init")



;;; `load-path' configurations

(add-to-list 'load-path (zeroein:path "lisp"))
(add-to-list 'load-path (zeroein:path "lib" "nxhtml" "util"))
(mapc (lambda (path) (add-to-list 'load-path (zeroein:path "lib" path)))
      zeroein:dependencies)


;;; Configurations
(require 'ein-loaddefs)
(eval-when-compile (require 'ein-notebooklist))
(require 'ein)

;; auto-complete
(setq ein:use-auto-complete-superpack t)
;; (setq ein:use-smartrep t)

(require 'auto-complete-config nil t)
(declare-function global-auto-complete-mode "auto-complete.el")
(when (featurep 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories
               (zeroein:path "lib" "auto-complete" "dict"))
  (global-auto-complete-mode t))

;; MuMaMo
(custom-set-faces
   '(mumamo-background-chunk-major
     ((((class color) (min-colors 88) (background dark)) nil)))
   ;; '(mumamo-background-chunk-submode1
   ;;   ((((class color) (min-colors 88) (background dark)) nil)))
   )


;;; Workaround

;; Suppress this warning when using mumamo:
;; Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;     use `syntax-propertize-function' instead.
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 1))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords)))
;; See: http://stackoverflow.com/a/5470584/727827


;;; Finally, open notebook list
(if noninteractive
    (progn
      ;; When called in batch mode, print system info.
      (require 'ein-dev)
      (ein:dev-print-sys-info))
  ;; To make EIN configurable by --eval, use idle timer:
  (run-with-idle-timer 0 nil 'call-interactively 'ein:notebooklist-open))

;;; zeroein.el ends here
