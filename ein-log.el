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
(require 'ein-utils)

(ein:deflocal ein:log-buffer nil
  "Buffer local variable to store a name of buffer to output log
of activities occurred in this buffer.")


(defvar ein:log-buffer-name-template " *ein:log %s*")
(defvar ein:log-all-buffer-name " *ein:log-all*")

(defvar ein:log-level-def
  '((debug . 40) (verbose . 30) (info . 20) (warn . 10) (error . 0)))

(defvar ein:log-level 30)
(defvar ein:log-message-level 20)

(defvar ein:log-print-level 1 "`print-level' for `ein:log'")
(defvar ein:log-print-length 10 "`print-length' for `ein:log'")
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

(defun ein:log-wrapper (level func)
  (setq level (ein:log-level-name-to-int level))
  (when (<= level ein:log-level)
    (let* ((levname (ein:log-level-int-to-name level))
           (print-level ein:log-print-level)
           (print-length ein:log-print-length)
           (msg (format "[%s] %s"  levname (funcall func)))
           (orig-buffer (current-buffer)))
      (if (and ein:log-max-string
               (> (length msg) ein:log-max-string))
          (setq msg (substring msg 0 ein:log-max-string)))
      (when ein:log-buffer
        (ein:with-read-only-buffer (get-buffer-create ein:log-buffer)
          (goto-char (point-max))
          (insert msg "\n")))
      (ein:with-read-only-buffer (get-buffer-create ein:log-all-buffer-name)
        (goto-char (point-max))
        (insert msg (format " @%S" orig-buffer) "\n"))
      (when (<= level ein:log-message-level)
        (message "ein: %s" msg)))))

(defmacro ein:log (level string &rest args)
  (declare (indent 1))
  `(ein:log-wrapper ,level (lambda () (format ,string ,@args))))

(defun ein:log-setup (name)
  (setq ein:log-buffer (format ein:log-buffer-name-template name))
  (ein:log 'verbose "Start logging."))

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

(defun ein:log-del ()
  "Kill buffer `ein:log-buffer'."
  ;; FIXME: Maybe add `ein:debug' option for not killing buffer?
  (when ein:log-buffer
    (when (get-buffer ein:log-buffer)
      (kill-buffer ein:log-buffer))
    (setq ein:log-buffer nil)))

(defun ein:log-pop-to-buffer ()
  (interactive)
  (if ein:log-buffer
      (pop-to-buffer ein:log-buffer)
    (message "ein: log buffer for current buffer is not set.")))

(defun ein:log-pop-to-all-buffer ()
  (interactive)
  (pop-to-buffer (get-buffer-create ein:log-all-buffer-name)))

(provide 'ein-log)

;;; ein-log.el ends here
