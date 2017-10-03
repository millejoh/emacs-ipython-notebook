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

(require 'ein-loaddefs)
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
         (files (-remove #'(lambda (x)
                             (string-match-p "ein-pkg\\.el" x))
                         (and
                          (file-accessible-directory-p dir)
                          (directory-files dir 'full regex)))))
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
  (let ((print-level 1)
        (print-length 1)
        (print-circle t))
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

(defun ein:dev-pop-to-debug-channels ()
  "Open notebok communication channels websocket log buffer."
  (interactive)
  (pop-to-buffer
   (websocket-get-debug-buffer-create
    (ein:$websocket-ws (ein:$kernel-channels
                        (ein:$notebook-kernel ein:%notebook%))))))

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

(defun ein:dev-notebook-multilang-mode ()
  "Use `ein:notebook-multilang-mode'."
  (interactive)
  (setq ein:notebook-modes '(ein:notebook-multilang-mode)))

(defun ein:dev-sys-info--lib (name)
  (let* ((libsym (intern-soft name))
         (version-var (loop for fmt in '("%s-version" "%s:version")
                            if (intern-soft (format fmt name))
                            return it))
         (version (symbol-value version-var)))
    (list :name name
          :path (ein:aand (locate-library name) (abbreviate-file-name it))
          :featurep (featurep libsym)
          :version-var version-var
          :version version)))

(defun ein:dev-dump-vars (names)
  (loop for var in names
        collect (intern (format ":%s" var))
        collect (symbol-value (intern (format "ein:%s" var)))))

(defun ein:dev-stdout-program (command args)
  "Safely call COMMAND with ARGS and return its stdout."
  (ein:aand (executable-find command)
            (with-temp-buffer
              (erase-buffer)
              (apply #'call-process it nil t nil args)
              (buffer-string))))

(defun ein:dev-sys-info ()
  (list
   "EIN system info"
   :emacs-version (emacs-version)
   :emacs-bzr-version (ein:eval-if-bound 'emacs-bzr-version)
   :window-system window-system
   ;; Emacs variant detection
   ;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
   :emacs-variant
   (cond ((featurep 'meadow) 'meadow)
         ((featurep 'carbon-emacs-package) 'carbon))
   :os (list
        :uname (ein:dev-stdout-program "uname" '("-a"))
        :lsb-release (ein:dev-stdout-program "lsb_release" '("-a")))
   :image-types (ein:eval-if-bound 'image-types)
   :image-types-available (ein:filter #'image-type-available-p
                                      (ein:eval-if-bound 'image-types))
   :request (list :backend request-backend)
   :ein (append (list :version (ein:version))
                (ein:dev-dump-vars '("source-dir")))
   :lib (ein:filter (lambda (info) (plist-get info :path))
                    (mapcar #'ein:dev-sys-info--lib
                            '("websocket" "request" "auto-complete" "mumamo"
                              "auto-complete" "popup" "fuzzy" "pos-tip"
                              "python" "python-mode" "markdown-mode"
                              "smartrep" "anything" "helm")))))

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

;;;###autoload
(defun ein:dev-bug-report-template ()
  "Open a buffer with bug report template."
  (interactive)
  (let ((buffer (generate-new-buffer "*ein:bug-report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "<!-- Use this template to help write bug report.
You may skip some sections, but at the very least include the
\"System info\" section, though do edit it to make sure no
personal information is included!

After finish writing it, please post it here:
https://github.com/millejoh/emacs-ipython-notebook/issues/new
-->

## Check list

- [ ] Read the \"Avoiding Common Emacs Traps\" section in
  https://github.com/millejoh/emacs-ipython-notebook/blob/master/CONTRIBUTING.md
- [ ] Does IPython works from the web browser?
- [ ] Confirm the problem was not due to badly compiled
  files by removing all the `*.elc` files from source directory of EIN and
  its dependencies.
- [ ] Confirm that the libraries are loaded as expected and are at the required version.
  (You can check the location in the \"System info\" section below)

## Description of the problem you have


## Steps to reproduce the problem

1.
2.
3.

## Expected output


## Your EIN configuration (in .emacs.d/init.el or somewhere else)


## Your IPython configuration

1. What is your IPython version? (run `ipython --version`):

2. How do you start IPython? (e.g., `ipython notebook --port 9999`):

3. What is your IPython notebook port number or URL?:


## Additional information (if any)


")
      (insert "## System info:\n\n```cl\n")
      (condition-case err
          (ein:dev-print-sys-info buffer)
        (error (insert (format "`ein:dev-sys-info' produce: %S" err))))
      (insert "```\n")
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (pop-to-buffer buffer))))

(defun ein:dev-print-sys-info (&optional stream)
  (princ (ein:dev--pp-to-string (ein:dev-sys-info))
         (or stream standard-output)))

(defun ein:dev--pp-to-string (object)
  "`pp-to-string' with additional prettifier."
  (with-temp-buffer
    (erase-buffer)
    (let ((pp-escape-newlines nil))
      (pp object (current-buffer)))
    (goto-char (point-min))
    (let ((emacs-lisp-mode-hook nil))
      (emacs-lisp-mode))
    (ein:dev--prettify-sexp)
    (buffer-string)))

(defun ein:dev--prettify-sexp ()
  "Prettify s-exp at point recursively.
Use this function in addition to `pp' (see `ein:dev--pp-to-string')."
  (down-list)
  (condition-case nil
      (while t
        (forward-sexp)
        ;; Prettify nested s-exp.
        (when (looking-back ")" (1- (point)))
          (save-excursion
            (backward-sexp)
            (ein:dev--prettify-sexp)))
        ;; Add newline before keyword symbol.
        (when (looking-at-p " :")
          (newline-and-indent))
        ;; Add newline before long string literal.
        (when (and (looking-at-p " \"")
                   (let ((end (save-excursion
                                (forward-sexp)
                                (point))))
                     (> (- end (point)) 80)))
          (newline-and-indent)))
    (scan-error)))

(defun ein:debug-notebook-to-json-buffer ()
  "Create a new buffer with the json representation of the current notebook."
  (interactive)
  (let ((content-data (ein:notebook-to-json ein:%notebook%))
        (bufname (format "*notebook-json:%s" (ein:$notebook-notebook-name ein:%notebook%))))
    (with-current-buffer (get-buffer-create bufname)
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
        (insert (json-encode content-data))
        (json-pretty-print (point-min) (point-max))))))

(provide 'ein-dev)

;;; ein-dev.el ends here
