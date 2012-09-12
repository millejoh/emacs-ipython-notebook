;;; ein-dev.el --- Development tools

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

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

(eval-when-compile (require 'cl))
(declare-function rst-shift-region "rst")

(require 'ein-notebook)
(require 'ein-subpackages)

;;;###autoload
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

(defun ein:load-files (&optional regex dir ignore-compiled)
  (let* ((dir (or dir ein:source-dir))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (unless ignore-compiled
      (setq files (mapcar #'file-name-sans-extension files)))
    (mapc #'load files)))

(defun ein:dev-reload ()
  "Reload ein-*.el modules."
  (interactive)
  (ein:notebook-kill-all-buffers)
  (makunbound 'ein:notebook-mode-map)   ; so defvar works.
  (load "ein-notebook")  ; ... but make sure it will be defined first.
  (ein:load-files "^ein-.*\\.el$")
  (ein:subpackages-reload))

(defun* ein:dev-require-all (&key (ignore-p #'ignore))
  (loop for f in (directory-files ein:source-dir nil "^ein-.*\\.el$")
        unless (or (equal f "ein-pkg.el")
                   (funcall ignore-p f))
        do (require (intern (file-name-sans-extension f)) nil t))
  ;; For `widget-button-press':
  (require 'wid-edit nil t))

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

(defun ein:dev-show-debug-setting ()
  "Show variables related to EIN debugging."
  (interactive)
  (message (concat "debug-on-error=%s websocket-debug=%s "
                   "websocket-callback-debug-on-error=%s "
                   "ein:debug=%s ein:log-level=%s ein:log-message-level=%s")
           debug-on-error websocket-debug websocket-callback-debug-on-error
           ein:debug
           (ein:log-level-int-to-name ein:log-level)
           (ein:log-level-int-to-name ein:log-message-level)))

;;;###autoload
(defun ein:dev-start-debug (&optional ws-callback)
  "Enable EIN debugging support.
When the prefix argument is given, debugging support for websocket
callback (`websocket-callback-debug-on-error') is enabled."
  (interactive "P")
  (setq debug-on-error t)
  (setq websocket-debug t)
  (when ws-callback
    (setq websocket-callback-debug-on-error t))
  (setq ein:debug t)
  (ein:log-set-level 'debug)
  (ein:log-set-message-level 'verbose)
  (ein:dev-patch-backtrace)
  (ein:dev-show-debug-setting))

;;;###autoload
(defun ein:dev-stop-debug ()
  "Disable debugging support enabled by `ein:dev-start-debug'."
  (interactive)
  (setq debug-on-error nil)
  (setq websocket-debug nil)
  (setq websocket-callback-debug-on-error nil)
  (setq ein:debug nil)
  (ein:log-set-level 'verbose)
  (ein:log-set-message-level 'info)
  (ein:dev-depatch-backtrace)
  (ein:dev-show-debug-setting))

(defun ein:dev-pop-to-debug-shell ()
  "Open shell channel websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-shell-channel
                        (ein:$notebook-kernel ein:%notebook%))))))

(defun ein:dev-pop-to-debug-iopub ()
  "Open iopub channel websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-iopub-channel
                        (ein:$notebook-kernel ein:%notebook%))))))

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

(defun ein:dev-notebook-org-src-mode ()
  "Use `ein:notebook-org-src-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-org-src-mode)))

(defun ein:dev-sys-info--lib (name)
  (list :name name
        :path (ein:aand (locate-library name) (abbreviate-file-name it))))

(defun ein:dev-sys-info ()
  (list
   "EIN system info"
   :emacs-version (emacs-version)
   :image-types image-types
   :lib (mapcar #'ein:dev-sys-info--lib
                '("websocket" "auto-complete" "mumamo"
                  "auto-complete" "popup" "fuzzy" "pos-tip"
                  "python" "python-mode" "markdown-mode"
                  "smartrep" "anything" "helm"))))

(defun ein:dev-show-sys-info (&optional show-in-buffer)
  "Show Emacs and library information."
  (interactive (list t))
  (let ((info (ein:dev-sys-info)))
    (if show-in-buffer
        (let ((buffer (get-buffer-create "*ein:sys-info*")))
          (with-current-buffer buffer
            (erase-buffer)
            (pp info buffer)
            (pop-to-buffer buffer)))
      (message "EIN INFO:\n%s" (pp-to-string info)))))

(provide 'ein-dev)

;;; ein-dev.el ends here
