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


;;; load-path configurations
(defvar zeroein:root-dir
  (or (if load-file-name (file-name-directory load-file-name))
      default-directory))

(defun zeroein:path (p &rest ps)
  (if ps
      (apply #'zeroein:path
           (concat (file-name-as-directory p) (car ps)) (cdr ps))
    (concat zeroein:root-dir p)))

(mapc (lambda (path) (add-to-list 'load-path (zeroein:path path)))
      '("ein/lisp" "markdown-mode" "websocket" "python" "auto-complete"
        "popup" "fuzzy" "pos-tip" "smartrep"))

(load (zeroein:path "nxhtml" "autostart.el"))


;;; Configurations
(eval-when-compile (require 'ein-notebooklist))
(require 'ein)

;; auto-complete
(setq ein:use-auto-complete-superpack t)
;; (setq ein:use-smartrep t)

(require 'auto-complete-config nil t)
(when (featurep 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories
               (zeroein:path "auto-complete" "dict"))
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

;;; zeroein.el ends here
