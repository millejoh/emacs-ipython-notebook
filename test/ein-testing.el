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
    (with-current-buffer buffer-or-name
      (write-region (point-min) (point-max) file-name))))

(defun ein:testing-dump-logs ()
  (ein:testing-save-buffer "*Messages*" ein:testing-dump-file-messages)
  (ein:testing-save-buffer "*ein:jupyter-server*" ein:testing-dump-file-server)
  (ein:testing-save-buffer ein:log-all-buffer-name ein:testing-dump-file-log)
  (ein:testing-save-buffer request-log-buffer-name ein:testing-dump-file-request))

(defun ein:testing-flush-queries (&optional ms interval)
  "Forget all the deferred:flush-queue! and deferred:sync! and all the semaphore
callbacks.  This is what I need."
  (deferred:flush-queue!)
  (ein:testing-wait-until (lambda ()
                            (ein:query-gc-running-process-table)
                            (zerop (hash-table-count ein:query-running-process-table)))
                          nil ms interval t))

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

(defadvice ert-run-tests-batch (after ein:testing-dump-logs-hook activate)
  "Hook `ein:testing-dump-logs-hook' because `kill-emacs-hook'
is not run in batch mode before Emacs 24.1."
  (ein:testing-dump-logs))

(add-hook 'kill-emacs-hook #'ein:testing-dump-logs)

(provide 'ein-testing)

;;; ein-testing.el ends here
