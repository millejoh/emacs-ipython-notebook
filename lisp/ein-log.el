;;; ein-log.el --- Logging module for ein.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-log.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-log.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-log.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ein-core)


(defvar ein:log-all-buffer-name "*ein:log-all*")

(defvar ein:log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")
;; Some names are stolen from supervisord (http://supervisord.org/logging.html)

(defvar ein:log-level 30)
(defvar ein:log-message-level 20)

(defvar ein:log-print-length 10 "`print-length' for `ein:log'")
(defvar ein:log-print-level 1 "`print-level' for `ein:log'")
(defvar ein:log-max-string 1000)


(defun ein:log-set-level (level)
  (setq ein:log-level (ein:log-level-name-to-int level)))

(defun ein:log-set-message-level (level)
  (setq ein:log-message-level (ein:log-level-name-to-int level)))

(defun ein:log-level-int-to-name (int)
  (loop for (n . i) in ein:log-level-def
        when (>= int i)
        return n
        finally 'error))

(defun ein:log-level-name-to-int (name)
  (cdr (assq name ein:log-level-def)))

(defsubst ein:log-strip-timestamp (msg)
  (replace-regexp-in-string "^[0-9: ]+" "" msg))

(defun ein:log-wrapper (level func)
  (setq level (ein:log-level-name-to-int level))
  (when (<= level ein:log-level)
    (let* ((levname (ein:log-level-int-to-name level))
           (print-level ein:log-print-level)
           (print-length ein:log-print-length)
           (msg (format "%s: [%s] %s" (format-time-string "%H:%M:%S:%3N") levname (funcall func)))
           (orig-buffer (current-buffer)))
      (if (and ein:log-max-string
               (> (length msg) ein:log-max-string))
          (setq msg (substring msg 0 ein:log-max-string)))
      (ein:with-read-only-buffer (get-buffer-create ein:log-all-buffer-name)
        (goto-char (point-max))
        (insert msg (format " @%S" orig-buffer) "\n"))
      (when (<= level ein:log-message-level)
        (message "ein: %s" (ein:log-strip-timestamp msg))))))

(defmacro ein:log (level string &rest args)
  (declare (indent 1))
  `(ein:log-wrapper ,level (lambda () (format ,string ,@args))))

;; FIXME: this variable must go to somewhere more central
(defvar ein:debug nil
  "Set to non-`nil' to raise errors instead of suppressing it.
Change the behavior of `ein:log-ignore-errors'.")

(defmacro ein:log-ignore-errors (&rest body)
  "Execute BODY; if an error occurs, log the error and return nil.
Otherwise, return result of last form in BODY."
  (declare (debug t) (indent 0))
  `(if ein:debug
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error
        (ein:log 'debug "Error: %S" err)
        (ein:log 'error (error-message-string err))
        nil))))

(defun ein:log-pop-to-all-buffer ()
  (interactive)
  (pop-to-buffer (get-buffer-create ein:log-all-buffer-name)))

(provide 'ein-log)

;;; ein-log.el ends here
