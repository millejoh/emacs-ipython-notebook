;;; ein-iexec.el --- Instant execution mode for notebook

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-iexec.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-iexec.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-iexec.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-worksheet)

(defcustom ein:iexec-delay 0.3
  "Delay before executing cell after change in second."
  :type 'number
  :group 'ein)

(defvar ein:iexec-timer nil)

(defun ein:iexec-execute-cell (cell)
  "Call `ein:notebook-execute-cell' after `ein:iexec-delay' second.
If the previous execution timer is not fired yet, cancel the timer."
  (when ein:iexec-timer
    (cancel-timer ein:iexec-timer))
  (setq ein:iexec-timer
        (run-with-idle-timer ein:iexec-delay nil
                             #'ein:worksheet-execute-cell
                             ein:%worksheet% cell)))

(defun ein:iexec-should-execute-p (cell beg end)
  "Return non-`nil' if CELL should be executed by the change within
BEG and END."
  (and (ein:codecell-p cell)
       this-command
       (ein:aif (ein:cell-input-pos-min cell) (<= it beg))
       (ein:aif (ein:cell-input-pos-max cell) (>= it end))))

(defun ein:iexec-after-change (beg end -ignore-len-)
  "Called via `after-change-functions' hook."
  (let ((cell (ein:worksheet-get-current-cell :pos beg)))
    (when (ein:iexec-should-execute-p cell beg end)
      (ein:iexec-execute-cell cell))))

;;;###autoload
(define-minor-mode ein:iexec-mode
  "Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area."
  :lighter " ein:i"
  :group 'ein
  (if ein:iexec-mode
      (add-hook 'after-change-functions 'ein:iexec-after-change nil t)
    (remove-hook 'after-change-functions 'ein:iexec-after-change t)))

;; To avoid MuMaMo to discard `ein:iexec-after-change', make it
;; permanent local.
(put 'ein:iexec-after-change 'permanent-local-hook t)
(put 'ein:iexec-mode 'permanent-local t)

(provide 'ein-iexec)

;;; ein-iexec.el ends here
