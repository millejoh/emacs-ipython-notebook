;;; ein-dev.el --- Development tools   -*- lexical-binding: t -*-

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
         (files (-remove #'(lambda (x)
                             (or (string-match-p "ein-pkg\\.el" x)
                                 (string-match-p "ein-smartrep\\.el" x)))
                         (and
                          (file-accessible-directory-p dir)
                          (directory-files dir 'full regex)))))
    (unless ignore-compiled
      (setq files (mapcar #'file-name-sans-extension files)))
    (mapc #'load files)))

(defun ein:dev-reload ()
  "Reload ein-*.el modules."
  (interactive)
  (makunbound 'ein:notebook-mode-map)   ; so defvar works.
  (load "ein-notebook")  ; ... but make sure it will be defined first.
  (ein:load-files "^ein-.*\\.el$"))

(cl-defun ein:dev-require-all (&key (ignore-p #'ignore))
  (cl-loop for f in (directory-files ein:source-dir nil "^ein-.*\\.el$")
    unless (or (equal f "ein-pkg.el")
               (equal f "ein-autoloads.el")
               (equal f "ein-smartrep.el")
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
(defun ein:dev-start-debug ()
  "Enable EIN debugging support.
When the prefix argument is given, debugging support for websocket
callback (`websocket-callback-debug-on-error') is enabled."
  (interactive)
  (setq debug-on-error t)
;; only use these with deferred:sync!  they cause strange failures otherwise!
;;  (setq deferred:debug-on-signal t)
;;  (setq deferred:debug t)
  (setq request-log-level (quote debug))
  (let ((curl-trace (concat temporary-file-directory "curl-trace")))
    (nconc request-curl-options `("--trace-ascii" ,curl-trace))
    (add-function :after
                  (symbol-function 'request--curl-callback)
                  (lambda (&rest _args)
                    (if (file-readable-p curl-trace)
                        (with-temp-buffer
                          (insert-file-contents curl-trace)
                          (request-log 'debug (buffer-string)))
                      (request-log 'debug "%s unreadable" curl-trace)))))
  (setq request-message-level (quote verbose))
  (setq websocket-debug t)
  (setq websocket-callback-debug-on-error t)
  (setq ein:debug t)
  (ein:log-set-level 'debug)
  (ein:log-set-message-level 'verbose)
  (ein:dev-patch-backtrace)
  (ein:dev-show-debug-setting))

;;;###autoload
(defun ein:dev-stop-debug ()
  "Inverse of `ein:dev-start-debug'.  Hard to maintain because it needs to match start"
  (interactive)
  (setq debug-on-error nil)
  (setq websocket-debug nil)
  (setq deferred:debug-on-signal nil)
  (setq deferred:debug nil)
  (setq request-log-level -1)
  (setq request-message-level 'warn)
  (setq websocket-callback-debug-on-error nil)
  (setq ein:debug nil)
  (ein:log-set-level 'verbose)
  (ein:log-set-message-level 'info)
  (ein:dev-depatch-backtrace)
  (ein:dev-show-debug-setting))

(defun ein:dev-pop-to-debug-channels ()
  "Open notebook communication channels websocket log buffer."
  (interactive)
  (-when-let* ((kernel (ein:get-kernel--notebook))
               (websocket (ein:$kernel-websocket kernel)))
    (pop-to-buffer
     (websocket-get-debug-buffer-create
      (ein:$websocket-ws websocket)))))

(defun ein:dev-pop-to-debug-shell ()
  "Legacy diagnostic for shell channel that got folded into ein:$kernel-websocket."
  (interactive)
  (-when-let* ((kernel (ein:get-kernel--notebook))
               (channel (ein:$kernel-shell-channel kernel)))
    (pop-to-buffer
     (websocket-get-debug-buffer-create
      (ein:$websocket-ws channel)))))

(defun ein:dev-pop-to-debug-iopub ()
  "Legacy diagnostic for iopub channel that got folded into ein:$kernel-websocket."
  (interactive)
  (-when-let* ((kernel (ein:get-kernel--notebook))
               (channel (ein:$kernel-shell-channel kernel)))
    (pop-to-buffer
     (websocket-get-debug-buffer-create
      (ein:$websocket-ws channel)))))

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
         (version-var (cl-loop for fmt in '("%s-version" "%s:version")
                        if (intern-soft (format fmt name))
                        return it))
         (version (symbol-value version-var)))
    (list :name name
          :path (ein:aand (locate-library name) (abbreviate-file-name it))
          :featurep (featurep libsym)
          :version-var version-var
          :version version)))

(defun ein:dev-dump-vars (names)
  (cl-loop for var in names
    collect (intern (format ":%s" var))
    collect (symbol-value (intern (format "ein:%s" var)))))

(defun ein:dev-stdout-program (command args)
  "Safely call COMMAND with ARGS and return its stdout."
  (ein:aand (executable-find command)
            (with-temp-buffer
              (erase-buffer)
              (apply #'call-process it nil t nil args)
              (buffer-string))))

(defsubst ein:dev-packages ()
  (let (result)
    (cl-letf (((symbol-function 'define-package)
               (lambda (&rest args)
                 (setq result (mapcar (lambda (x) (symbol-name (car x))) (nth 3 args))))))
      (load "ein-pkg")
      result)))

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
         ((featurep 'core-spacemacs) 'spacemacs)
         ((featurep 'carbon-emacs-package) 'carbon))
   :os (list
        :uname (ein:dev-stdout-program "uname" '("-a"))
        :lsb-release (ein:dev-stdout-program "lsb_release" '("-a")))
   :notebook (ein:dev-stdout-program "pip" '("show" "notebook"))
   :ipython (ein:dev-stdout-program "ipython" '("--version"))
   :image-types (ein:eval-if-bound 'image-types)
   :image-types-available (seq-filter #'image-type-available-p
                                      (ein:eval-if-bound 'image-types))
   :request (list :backend request-backend)
   :ein (append (list :version (ein:version))
                (ein:dev-dump-vars '("source-dir")))
   :lib (seq-filter (lambda (info) (plist-get info :path))
                    (mapcar #'ein:dev-sys-info--lib
                            (ein:dev-packages)))))

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
      (insert "## Problem description\n\n## Steps to reproduce the problem\n\n")
      (insert "<!-- Ensure no information sensitive to your institution is included!!! -->\n")
      (insert "## System info:\n\n```cl\n")
      (condition-case err
          (ein:dev-print-sys-info buffer)
        (error (insert (format "`ein:dev-sys-info' produce: %S" err))))
      (insert "```\n")
      (goto-char (point-min))
      (markdown-mode)
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
  (when-let ((notebook (ein:get-notebook)))
    (let ((content-data (ein:notebook-to-json notebook))
          (bufname (format "*notebook-json:%s" (ein:$notebook-notebook-name notebook))))
      (with-current-buffer (get-buffer-create bufname)
        (barf-if-buffer-read-only)
        (erase-buffer)
        (save-excursion
          (insert (json-encode content-data))
          (json-pretty-print (point-min) (point-max))))
      (pop-to-buffer bufname))))

(provide 'ein-dev)

;;; ein-dev.el ends here
