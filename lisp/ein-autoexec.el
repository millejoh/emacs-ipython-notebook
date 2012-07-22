;;; ein-autoexec.el --- Automatic cell execution

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-autoexec.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-autoexec.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-autoexec.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-notebook)

(defun ein:autoexec-after-change (beg end -ignore-len-)
  "Called via `after-change-functions' hook."
  (let ((cell (ein:notebook-get-current-cell beg)))
    (when (and (ein:codecell-p cell)
               this-command
               (<= (ein:cell-input-pos-min cell) beg)
               (>= (ein:cell-input-pos-max cell) end))
      (ein:notebook-execute-cell ein:notebook cell))))

(define-minor-mode ein:autoexec-mode
  "Automatic cell execution minor mode."
  :lighter " ein:au"
  :group 'ein
  (if ein:autoexec-mode
      (add-hook 'after-change-functions 'ein:autoexec-after-change nil t)
    (remove-hook 'after-change-functions 'ein:autoexec-after-change t)))

;; To avoid MuMaMo to discard `ein:autoexec-after-change', make it
;; permanent local.
(put 'ein:autoexec-after-change 'permanent-local-hook t)
(put 'ein:autoexec-mode 'permanent-local t)

(provide 'ein-autoexec)

;;; ein-autoexec.el ends here
