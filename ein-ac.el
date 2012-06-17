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
(require 'auto-complete nil t)

(require 'ein-utils)
(eval-when-compile (require 'ein-notebook)
                   (require 'ein-mumamo))

(defvar ein:ac-sources (default-value 'ac-sources)
  "Extra `ac-sources' used in notebook.")

(defcustom ein:ac-max-cache 1000
  "Maximum number of cache to store."
  :type 'integer
  :group 'ein)

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
  (run-with-idle-timer 1 nil #'ein:ac-clear-cache)
  (with-syntax-table ein:dotty-syntax-table
    (auto-complete '(ac-source-ein-direct))))

(defun ein:ac-clear-cache ()
  (setq ein:ac-cache-matches
        (setcdr (nthcdr (1- ein:ac-max-cache)
                        (delete-dups ein:ac-cache-matches)) nil)))

(defadvice ac-prefix
  (around ein:ac-always-dotty (requires ignore-list))
  "Monkey patch `auto-complete' internal function to enable
dotty completion."
  (if (or ein:notebook (ein:eval-if-bound 'ein:@connect))
      (with-syntax-table ein:dotty-syntax-table
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

(defun ein:ac-setup-maybe ()            ; [#hook]_
  (and ein:notebook
       (eql major-mode ein:mumamo-codecell-mode)
       (ein:ac-setup)))

(defun ein:ac-config (&optional superpack)
  "Install auto-complete-mode for notebook modes.
Specifying non-`nil' to SUPERPACK enables dotty auto completion
\(see `ein:ac-superpack')."
  (add-hook 'after-change-major-mode-hook 'ein:ac-setup-maybe) ; [#hook]_
  (add-hook 'ein:notebook-plain-mode 'ein:ac-setup)
  (when superpack
    (ein:ac-superpack)))
;; .. [#hook] Setting `ein:notebook-mumamo-mode-hook' does not work
;;    because `ac-sources' in `ein:notebook-mumamo-mode'-enabled
;;    buffer is *chunk local*, rather than buffer local.


(defvar ein:ac-config-once-called nil)

(defun ein:ac-config-once (&optional superpack)
  (unless ein:ac-config-once-called
    (setq ein:ac-config-once-called t)
    (ein:ac-config superpack)))

(provide 'ein-ac)

;;; ein-ac.el ends here
