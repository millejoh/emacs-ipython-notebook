;;; ein-log.el --- Logging module for ein.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

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

(defvar ein:log-buffer)
(make-variable-buffer-local 'ein:log-buffer)
(put 'ein:log-buffer 'permanent-local t)

(defvar ein:log-buffer-name-template " *ein:log %s*")
(defvar ein:log-all-buffer-name " *ein:log-all*")

(defvar ein:log-level-def
  '((debug . 40) (verbose . 30) (info . 20) (warn . 10) (error . 0)))

(defvar ein:log-level 30)
(defvar ein:log-message-level 20)

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
           (msg (format "[%s] %s"  levname (funcall func)))
           (orig-buffer (current-buffer)))
      (if (and ein:log-max-string
               (> (length msg) ein:log-max-string))
          (setq msg (substring msg 0 ein:log-max-string)))
      (when ein:log-buffer
        (with-current-buffer ein:log-buffer
          (save-excursion
            (goto-char (point-max))
            (insert msg "\n"))))
      (with-current-buffer (get-buffer-create ein:log-all-buffer-name)
        (save-excursion
          (goto-char (point-max))
          (insert msg (format " @%S" orig-buffer) "\n")))
      (when (<= level ein:log-message-level)
        (message "ein: %s" msg)))))

(defmacro ein:log (level string &rest args)
  (declare (indent 1))
  `(ein:log-wrapper ,level (lambda () (format ,string ,@args))))

(defun ein:log-setup (name)
  (setq ein:log-buffer
        (get-buffer-create (format ein:log-buffer-name-template name)))
  (ein:log 'verbose "Start logging."))

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
