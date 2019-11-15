;;; ein-testing.el --- Tools for testing

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-testing.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-testing.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-testing.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-log)
(require 'request)

(defmacro ein:setq-if-not (sym val)
  `(unless ,sym (setq ,sym ,val)))

(defvar ein:testing-dump-file-log nil
  "File to save buffer specified by `ein:log-all-buffer-name'.")

(defvar ein:testing-dump-file-messages nil
  "File to save the ``*Messages*`` buffer.")

(defvar ein:testing-dump-file-server nil
  "File to save `ein:jupyter-server-buffer-name`.")

(defvar ein:testing-dump-file-request nil
  "File to save `request-log-buffer-name`.")

(defun ein:testing-save-buffer (buffer-or-name file-name)
  (when (and buffer-or-name (get-buffer buffer-or-name) file-name)
    (let ((dir (file-name-directory file-name)))
      (make-directory dir t)
      (with-current-buffer buffer-or-name
        (let ((coding-system-for-write 'raw-text))
          (write-region (point-min) (point-max) file-name))))))

(defun ein:testing-dump-logs ()
  (ein:testing-save-buffer "*Messages*" ein:testing-dump-file-messages)
  (ein:testing-save-buffer "*ein:jupyter-server*" ein:testing-dump-file-server)
  (mapc (lambda (b)
          (ein:and-let* ((bname (buffer-name b))
                         (prefix "kernels/")
                         (is-websocket (search "*websocket" bname))
                         (kernel-start (search prefix bname))
                         (sofar (subseq bname (+ kernel-start (length prefix))))
                         (kernel-end (search "/" sofar)))
            (ein:testing-save-buffer
             bname
             (concat ein:testing-dump-file-websocket "."
                     (seq-take sofar kernel-end)))))
        (buffer-list))
  (ein:testing-save-buffer ein:log-all-buffer-name ein:testing-dump-file-log)
  (ein:testing-save-buffer request-log-buffer-name ein:testing-dump-file-request))

(defun ein:testing-flush-queries (&optional ms interval)
  "I need the smoke to clear, but just waiting for zero running processes doesn't work
if I call this between links in a deferred chain.  Adding a flush-queue."
  (deferred:flush-queue!)
  (ein:testing-wait-until (lambda ()
                            (ein:query-running-process-table)
                            (zerop (hash-table-count ein:query-running-process-table)))
                          nil ms interval t))

(defun ein:testing-make-directory-level (parent current-depth width depth)
  (let ((write-region-inhibit-sync nil))
    (f-touch (concat (file-name-as-directory parent) "foo.txt"))
    (f-touch (concat (file-name-as-directory parent) "bar.ipynb"))
    (f-write-text "{
 \"cells\": [],
 \"metadata\": {},
 \"nbformat\": 4,
 \"nbformat_minor\": 2
}
" 'utf-8 (concat (file-name-as-directory parent) "bar.ipynb")))
  (if (< current-depth depth)
      (loop for w from 1 to width
            for dir = (concat (file-name-as-directory parent) (number-to-string w))
            do (f-mkdir dir)
               (ein:testing-make-directory-level dir (1+ current-depth) width depth))))

(defun ein:testing-wait-until (predicate &optional predargs ms interval continue)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait.  INTERVAL is polling interval in milliseconds."
  (let* ((int (ein:aif interval it (ein:aif ms (max 300 (/ ms 10)) 300)))
         (count (max 1 (if ms (truncate (/ ms int)) 25))))
    (unless (or (loop repeat count
                       when (apply predicate predargs)
                       return t
                       do (sleep-for 0 int))
                continue)
      (error "Timeout: %s" predicate))))

(defun ein:testing-new-notebook (url-or-port ks &optional retry)
  (lexical-let (notebook)
    (condition-case err
        (progn
          (ein:testing-wait-until (lambda ()
                                    (ein:notebooklist-list-get url-or-port))
                                  nil 10000 1000)
          (ein:notebooklist-new-notebook url-or-port ks
                                         (lambda (nb created)
                                           (setq notebook nb)))
          (ein:testing-wait-until (lambda ()
                                    (and notebook
                                         (ein:aand (ein:$notebook-kernel notebook)
                                                   (ein:kernel-live-p it))))
                                  nil 20000 1000)
          notebook)
      (error (let ((notice (format "ein:testing-new-notebook: [%s] %s"
                                   url-or-port (error-message-string err))))
               (if retry
                   (progn (ein:log 'error notice) nil)
                 (ein:log 'info notice)
                 (sleep-for 0 1500)
                 (ein:testing-new-notebook url-or-port ks t)))))))

(defadvice ert-run-tests-batch (after ein:testing-dump-logs-hook activate)
  "Hook `ein:testing-dump-logs-hook' because `kill-emacs-hook'
is not run in batch mode before Emacs 24.1."
  (ein:testing-dump-logs))

(add-hook 'kill-emacs-hook #'ein:testing-dump-logs)

(with-eval-after-load "ein-notebook"
  ;; if y-or-n-p isn't specially overridden, make it always "no"
  (lexical-let ((original-y-or-n-p (symbol-function 'y-or-n-p)))
    (add-function :around (symbol-function 'ein:notebook-ask-save)
                  (lambda (f &rest args)
                    (if (not (eq (symbol-function 'y-or-n-p) original-y-or-n-p))
                        (apply f args)
                      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest args) nil)))
                        (apply f args)))))))

(provide 'ein-testing)

;;; ein-testing.el ends here
