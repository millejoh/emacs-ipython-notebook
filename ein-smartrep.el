;;; ein-smartrep.el --- smartrep integration

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-smartrep.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-smartrep.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-smartrep.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'smartrep nil t)
(require 'ein-notebook)

(defun ein:smartrep-config ()
  (interactive)
  (smartrep-define-key
      ein:notebook-mode-map
      "C-c"
    '(("C-t" . ein:notebook-toggle-cell-type)
      ("C-l" . ein:notebook-clear-output-command)
      ("C-k" . ein:notebook-kill-cell-command)
      ("C-y" . ein:notebook-yank-cell-command)
      ("C-a" . ein:notebook-insert-cell-above-command)
      ("C-b" . ein:notebook-insert-cell-below-command)
      ("C-n" . ein:notebook-goto-next-input-command)
      ("C-p" . ein:notebook-goto-prev-input-command)
      ("C-m" . ein:notebook-merge-cell-command)
      ("<up>" . ein:notebook-move-cell-up-command)
      ("<down>" . ein:notebook-move-cell-down-command)
      )))


(defvar ein:smartrep-config-once-called nil)

(defun ein:smartrep-config-once ()
  (unless ein:smartrep-config-once-called
    (setq ein:smartrep-config-once-called t)
    (ein:smartrep-config)))

(provide 'ein-smartrep)

;;; ein-smartrep.el ends here
