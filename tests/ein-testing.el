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

(defmacro ein:setq-if-not (sym val)
  `(unless ,sym (setq ,sym ,val)))

(defvar ein:testing-dump-file-log nil
  "File to save buffer specified by `ein:log-all-buffer-name'.")

(defvar ein:testing-dump-file-messages nil
  "File to save the ``*Messages*`` buffer.")

(defvar ein:testing-dump-file-debug nil)

(defun ein:testing-save-buffer (buffer-or-name file-name)
  (when (and buffer-or-name file-name)
    (with-current-buffer (get-buffer buffer-or-name)
      (write-region (point-min) (point-max) file-name))))

(defun ein:testing-dump-logs ()
  (ein:testing-save-buffer "*Messages*" ein:testing-dump-file-messages)
  (ein:testing-save-buffer ein:log-all-buffer-name ein:testing-dump-file-log))

(defvar ein:testing-dump-logs--saved nil)

(defun ein:testing-dump-logs-noerror ()
  (if ein:testing-dump-logs--saved
      (message "EIN:TESTING-DUMP-LOGS-NOERROR called but already saved.")
    (condition-case err
        (progn (ein:testing-dump-logs)
               (setq ein:testing-dump-logs--saved t))
      (error
       (message "Error while executing EIN:TESTING-DUMP-LOGS. err = %S"
                err)
       (when ein:testing-dump-file-debug
         (signal (car err) (cdr err)))))))

(defadvice ert-run-tests-batch (after ein:testing-dump-logs-hook activate)
  "Hook `ein:testing-dump-logs-noerror' because `kill-emacs-hook'
is not run in batch mode before Emacs 24.1."
  (ein:testing-dump-logs-noerror))

(add-hook 'kill-emacs-hook #'ein:testing-dump-logs-noerror)

(provide 'ein-testing)

;;; ein-testing.el ends here
