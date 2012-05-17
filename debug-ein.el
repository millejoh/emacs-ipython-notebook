;;; debug-ein.el --- Debug ein.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; debug-ein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; debug-ein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with debug-ein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emacs -Q -L path/to/nxhtml/util/ -l debug-ein.el

;;; Code:

(add-to-list 'load-path (file-name-directory load-file-name))
(eval-when-compile (require 'ein-notebooklist))
(require 'ein)
(require 'ein-dev)
(setq debug-on-error t)
(setq websocket-debug t)
(ein:notebooklist-open)
(ein:log-set-level 'debug)
(ein:log-set-message-level 'verbose)

(require 'markdown-mode nil t)
(require 'rst nil t)

(declare-function ein:ac-config "ein-ac")
(declare-function global-auto-complete-mode "auto-complete")
(when (featurep 'auto-complete)
  (global-auto-complete-mode t)
  (require 'ein-ac)
  (ein:ac-config t))

(declare-function ein:smartrep-config "ein-smartrep")
(when (featurep 'smartrep)
  (require 'ein-smartrep)
  (ein:smartrep-config))


;; Suppress this warning when using mumamo:
;; Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;     use `syntax-propertize-function' instead.
;; See: http://stackoverflow.com/a/5470584/727827
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 1))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords)))


(defadvice backtrace (around eintest-short-backtrace activate)
  "A hack for shorten backtrace.

As code cells hold base64-encoded image data, backtrace tends to
be VERY long.  So I am setting `print-level' to *1*.  Note that
setting it globally via `setq' does not work because the value
for debugger is hard-coded.  See `debugger-setup-buffer'."
  (let ((print-level 1))
    ad-do-it))

(defun eintest-pop-to-debug-shell ()
  "Open shell challen websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-shell-channel (ein:@notebook kernel))))))

(defun eintest-pop-to-debug-iopub ()
  "Open iopub challen websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-iopub-channel (ein:@notebook kernel))))))

(defun eintest-notebook-plain-mode ()
  "Use `ein:notebook-plain-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-plain-mode)))

(defun eintest-notebook-mumamo-mode ()
  "Use `ein:notebook-mumamo-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-plain-mode)))

;;; debug-ein.el ends here
