;;; ein-dev.el --- Development tools    -*- lexical-binding:t -*-

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

(require 'ein-notebook)

(defvar ein:dev-trace-curl nil "Turn on to really go after it.")

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

;;;###autoload
(defun ein:dev-start-debug ()
  "Start logging a bunch of stuff."
  (interactive)
  (setq debug-on-error t)
  (setq request-log-level (quote debug))
  (let ((curl-trace (concat temporary-file-directory "curl-trace")))
    (setq request-curl-options
          (append request-curl-options `("--trace-ascii" ,curl-trace)))
    (add-function :after
                  (symbol-function 'request--curl-callback)
                  (lambda (&rest _args)
                    (when ein:dev-trace-curl
                      (if (file-readable-p curl-trace)
                          (with-temp-buffer
                            (insert-file-contents curl-trace)
                            (request-log 'debug (buffer-string)))
                        (request-log 'debug "%s unreadable" curl-trace))))))
  (setq request-message-level (quote verbose))
  (setq websocket-debug t)
  (setq websocket-callback-debug-on-error t)
  (ein:log-set-level 'debug)
  (ein:log-set-message-level 'verbose)
  (ein:dev-patch-backtrace))

;;;###autoload
(defun ein:dev-stop-debug ()
  "Inverse of `ein:dev-start-debug'.
Impossible to maintain because it needs to match start."
  (interactive)
  (setq debug-on-error nil)
  (setq websocket-debug nil)
  (setq request-log-level -1)
  (setq request-message-level 'warn)
  (setq websocket-callback-debug-on-error nil)
  (ein:log-set-level 'verbose)
  (ein:log-set-message-level 'info)
  (ein:dev-depatch-backtrace)
  (let ((curl-trace (concat temporary-file-directory "curl-trace")))
    (setq request-curl-options
          (cl-remove-if (lambda (x) (member x `("--trace-ascii" ,curl-trace)))
                        request-curl-options))))

(defun ein:dev-stdout-program (command args)
  "Safely call COMMAND with ARGS and return its stdout."
  (aand (executable-find command)
        (with-temp-buffer
          (erase-buffer)
          (apply #'call-process it nil t nil args)
          (buffer-string))))

(defun ein:dev-packages ()
  (let (result)
    (cl-labels ((extract
                 (lst)
                 (mapcar (lambda (x) (symbol-name (cl-first x))) lst))
                (define-package
                  (args)
                  (setq result (extract (nth 3 args)))))
      (condition-case err
          (load "ein-pkg")
        (error
         (with-temp-buffer
           (ein:log 'warn "ein:dev-packages: %s" (error-message-string err))
           (insert-file-contents (locate-library "ein-pkg"))
           (setq result (extract (eval (nth 4 (car (read-from-string (buffer-string))))))))))
      result)))

(defun ein:dev-sys-info ()
  "Returns a list."
  (cl-flet ((lib-info
             (name)
             (let* ((libsym (intern-soft name))
                    (version-var (cl-loop for fmt in '("%s-version" "%s:version")
                                          if (intern-soft (format fmt name))
                                          return it))
                    (version (symbol-value version-var)))
               (list :name name
                     :path (aand (locate-library name) (abbreviate-file-name it))
                     :featurep (featurep libsym)
                     :version-var version-var
                     :version version)))
            (dump-vars
             (names)
             (cl-loop for var in names
                      collect (intern (format ":%s" var))
                      collect (symbol-value (intern (format "ein:%s" var))))))
    (list
     "EIN system info"
     :emacs-version (emacs-version)
     :window-system window-system
     :emacs-variant
     (cond ((boundp 'spacemacs-version) (concat "spacemacs" spacemacs-version))
           ((boundp 'doom-version) (concat "doom-" doom-version)))
     :build system-configuration-options
     :os (list
          :uname (ein:dev-stdout-program "uname" '("-a"))
          :lsb-release (ein:dev-stdout-program "lsb_release" '("-a")))
     :jupyter (ein:dev-stdout-program "jupyter" '("--version"))
     :image-types (ein:eval-if-bound 'image-types)
     :image-types-available (seq-filter #'image-type-available-p
                                        (ein:eval-if-bound 'image-types))
     :request-backend request-backend
     :ein (append (list :version (ein:version))
                  (dump-vars '("source-dir")))
     :lib (seq-filter (lambda (info) (plist-get info :path))
                      (mapcar #'lib-info (ein:dev-packages))))))

;;;###autoload
(defun ein:dev-bug-report-template ()
  "Open a buffer with bug report template."
  (interactive)
  (let ((buffer (generate-new-buffer "*ein:bug-report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "## Problem description\n\n"
              "## Steps to reproduce the problem\n\n"
              "<!-- Ensure no information sensitive to your institution below!!! -->\n"
              "## System info:\n\n"
              "```cl\n")
      (condition-case err
          (ein:dev-pp-sys-info buffer)
        (error (insert (format "ein:dev-sys-info erred: %s" (error-message-string err)))))
      (insert "```\n"
              "## Logs:\n")
      (ein:dev-dump-logs buffer)
      (goto-char (point-min))
      (pop-to-buffer buffer))))

(defvar *ein:jupyter-server-buffer-name*)
(defun ein:dev-dump-logs (&optional stream)
  (interactive)
  (dolist (notebook (ein:notebook-opened-notebooks))
    (-when-let* ((kernel (ein:$notebook-kernel notebook))
                 (websocket (ein:$kernel-websocket kernel))
                 (ws (ein:$websocket-ws websocket))
                 (ws-buf (websocket-get-debug-buffer-create ws)))
      (let (dump)
        (with-current-buffer ws-buf
          (setq dump (buffer-substring-no-properties
                      (point-min) (point-max))))
        (if (zerop (length dump))
            (kill-buffer ws-buf)
          (mapc (lambda (s)
                  (princ (format "%s\n" s) (or stream standard-output)))
                (list
                 (format "#### `%s`:" (ein:url (ein:$kernel-url-or-port kernel)
                                             (ein:$kernel-path kernel)))
                 "```"
                 (string-trim dump)
                 "```"))))))
  (cl-macrolet ((dump
                 (name)
                 `(awhen (get-buffer ,name)
                    (with-current-buffer it
                      (mapc (lambda (s)
                              (princ (format "%s\n" s)
                                     (or stream standard-output)))
                            (list
                             (format "#### %s:" ,name)
                             "```"
                             (string-trim (buffer-substring-no-properties
                                           (point-min) (point-max)))
                             "```"))))))
    (dump request-log-buffer-name)
    (dump ein:log-all-buffer-name)
    (dump *ein:jupyter-server-buffer-name*)))

(defun ein:dev-pp-sys-info (&optional stream)
  (interactive)
  (princ (ein:dev-obj-to-string (ein:dev-sys-info))
         (or stream standard-output)))

(defvar pp-escape-newlines)
(defun ein:dev-obj-to-string (object)
  (with-temp-buffer
    (erase-buffer)
    (let ((pp-escape-newlines nil))
      (pp object (current-buffer)))
    (goto-char (point-min))
    (let ((emacs-lisp-mode-hook nil))
      (emacs-lisp-mode))
    (ein:dev-pp-sexp)
    (buffer-string)))

(defun ein:dev-pp-sexp ()
  "Prettify s-exp at point recursively.
Use this function in addition to `pp' (see `ein:dev-obj-to-string')."
  (down-list)
  (condition-case nil
      (while t
        (forward-sexp)
        ;; Prettify nested s-exp.
        (when (looking-back ")" (1- (point)))
          (save-excursion
            (backward-sexp)
            (ein:dev-pp-sexp)))
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

(provide 'ein-dev)

;;; ein-dev.el ends here
