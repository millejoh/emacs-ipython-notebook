;;; ein-ac.el --- Auto-complete extension

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-ac.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ac.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-ac.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'auto-complete)

(defvar ein:ac-sources (default-value 'ac-sources)
  "Extra `ac-sources' used in notebook.")

(defvar ein:ac-dotty-syntax-table
  (let ((table (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Adapted from `python-dotty-syntax-table'.")

(defvar ein:ac-cache-matches nil)

(defvar ein:ac-direct-matches nil
  "Variable to store completion candidates for `auto-completion'.")
;; FIXME: Maybe this should be buffer-local?

(ac-define-source ein-direct
  '((candidates . ein:ac-direct-matches)
    (symbol . "s")))

(ac-define-source ein-cached
  '((candidates . ein:ac-cache-matches)
    (symbol . "c")))

(defun ein:completer-finish-completing-ac (matched-text matches)
  "Invoke completion using `auto-complete'.
Only the argument MATCHES is used.  MATCHED-TEXT is for
compatibility with `ein:completer-finish-completing-default'."
  ;; I don't need to check if the point is at right position, as in
  ;; `ein:completer-finish-completing-default' because `auto-complete'
  ;; checks it anyway.
  (setq ein:ac-direct-matches matches)  ; let-binding won't work
  (setq ein:ac-cache-matches (append matches ein:ac-cache-matches))
  (with-syntax-table ein:ac-dotty-syntax-table
    (auto-complete '(ac-source-ein-direct))))

;; FIXME: add a cleanup function to limit `ein:ac-cache-matches'.

(defadvice ac-prefix
  (around ein:ac-always-dotty (requires ignore-list))
  "Monkey patch `auto-complete' internal function to enable
dotty completion."
  (if ein:notebook
      (with-syntax-table ein:ac-dotty-syntax-table
        ad-do-it)
    ad-do-it))

(defun ein:ac-superpack ()
  "Enable dotty syntax table for auto-complete.
This function monkey patches `ac-prefix' to make \".\" as a part of word."
  (interactive)
  (ad-enable-advice 'ac-prefix 'around 'ein:ac-always-dotty)
  (ad-activate 'ac-prefix))

(defun ein:ac-setup ()
  "Call this function from mode hook (see `ein:ac-config')."
  ;; Note that `ac-source-ein-cached' can still be useful even if
  ;; `ein:ac-always-dotty' advice is not enabled.  So add this source
  ;; anyway.  Also note that this source must come at the head of the
  ;; sources.
  (setq ac-sources (append '(ac-source-ein-cached) ein:ac-sources)))

(defun ein:ac-config (&optional superpack)
  "Install auto-complete-mode for notebook modes.
Specifying non-`nil' to SUPERPACK enables dotty auto completion
\(see `ein:ac-superpack')."
  (add-hook 'ein:notebook-mumamo-mode-hook 'ein:ac-setup)
  (add-hook 'ein:notebook-plain-mode 'ein:ac-setup)
  (when superpack
    (ein:ac-superpack)))

(provide 'ein-ac)

;;; ein-ac.el ends here
