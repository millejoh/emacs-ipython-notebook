;;; ein-dev.el --- Development tools

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-dev.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-dev.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-dev.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'rst)
(require 'ein-notebook)
(require 'ein-subpackages)

(defun ein:dev-insert-mode-map (map-string)
  "Insert mode-map into rst document.  For README.rst."
  (save-excursion
    (insert "\n\n::\n\n")
    (let ((beg (point)))
      (search-forward ".. // KEYS END //")
      (move-beginning-of-line nil)
      (delete-region beg (point))
      (insert "\n")
      (goto-char beg)
      (insert (substitute-command-keys map-string))
      (rst-shift-region beg (point) 1))))

(defun ein:load-files (&optional regex dir)
  (let* ((dir (or dir ein:source-dir))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (mapc #'load files)))

(defun ein:dev-reload ()
  "Reload ein-*.el modules."
  (interactive)
  (ein:notebook-kill-all-buffers)
  (makunbound 'ein:notebook-mode-map)   ; so defvar works.
  (load "ein-notebook")  ; ... but make sure it will be defined first.
  (ein:load-files "^ein-.*\\.el$")
  (ein:subpackages-reload))

(defadvice backtrace (around ein:dev-short-backtrace)
  "A hack to shorten backtrace.

As code cells hold base64-encoded image data, backtrace tends to
be VERY long.  So I am setting `print-level' to *1*.  Note that
setting it globally via `setq' does not work because the value
for debugger is hard-coded.  See `debugger-setup-buffer'."
  (let ((print-level 1))
    ad-do-it))

(defun ein:dev-patch-backtrace ()
  "Monkey patch `backtrace' function to make it shorter."
  (interactive)
  (ad-enable-advice 'backtrace 'around 'ein:dev-short-backtrace)
  (ad-activate 'backtrace))

(defun ein:dev-depatch-backtrace ()
  "Undo `ein:dev-patch-backtrace'."
  (interactive)
  (ad-deactivate 'backtrace)
  (ad-disable-advice 'backtrace 'around 'ein:dev-short-backtrace)
  ;; In case it has other advices.
  (ad-activate 'backtrace))

(defun ein:dev-start-debug ()
  (interactive)
  (setq debug-on-error t)
  (setq websocket-debug t)
  (setq ein:debug t)
  (ein:log-set-level 'debug)
  (ein:log-set-message-level 'verbose)
  (ein:dev-patch-backtrace))

(defun ein:dev-stop-debug ()
  (interactive)
  (setq debug-on-error nil)
  (setq websocket-debug nil)
  (setq ein:debug nil)
  (ein:log-set-level 'verbose)
  (ein:log-set-message-level 'info)
  (ein:dev-depatch-backtrace))

(defun ein:dev-pop-to-debug-shell ()
  "Open shell channel websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-shell-channel
                        (ein:$notebook-kernel ein:notebook))))))

(defun ein:dev-pop-to-debug-iopub ()
  "Open iopub channel websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-iopub-channel
                        (ein:$notebook-kernel ein:notebook))))))

(defun ein:dev-notebook-plain-mode ()
  "Use `ein:notebook-plain-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-plain-mode)))

(defun ein:dev-notebook-python-mode ()
  "Use `ein:notebook-python-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-python-mode)))

(defun ein:dev-notebook-mumamo-mode ()
  "Use `ein:notebook-mumamo-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-mumamo-mode)))

(provide 'ein-dev)

;;; ein-dev.el ends here
