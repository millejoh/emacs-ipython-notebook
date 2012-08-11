;;; ein-ac.el --- Auto-complete extension

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

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

(defvar ein:ac-sources (and (boundp 'ac-sources)
                            (default-value 'ac-sources))
  "Extra `ac-sources' used in notebook.")

(defcustom ein:ac-max-cache 1000
  "Maximum number of cache to store."
  :type 'integer
  :group 'ein)

(defvar ein:ac-syntax-table
  (let ((table (make-syntax-table ein:dotty-syntax-table)))
    (modify-syntax-entry ?~ "w" table)
    table)
  "`ein:dotty-syntax-table' plus \"~\" considered as a word character.")

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
  (ein:log 'debug "COMPLETER-FINISH-COMPLETING-AC: matched-text=%S matches=%S"
           matched-text matches)
  (when matches      ; No auto-complete drop-down list when no matches
    (setq ein:ac-direct-matches matches)  ; let-binding won't work
    (setq ein:ac-cache-matches (append matches ein:ac-cache-matches))
    (run-with-idle-timer 1 nil #'ein:ac-clear-cache)
    (with-syntax-table ein:ac-syntax-table
      (auto-complete '(ac-source-ein-direct)))))

(defun ein:ac-clear-cache ()
  (setq ein:ac-cache-matches
        (setcdr (nthcdr (1- ein:ac-max-cache)
                        (delete-dups ein:ac-cache-matches)) nil)))

(defun ein:ac-request-document-for-selected-candidate ()
  "Request object information for the candidate at point.
This is called via `ac-next'/`ac-previous'/`ac-update' and set
`document' property of the current candidate string.  If server
replied within `ac-quick-help-delay' seconds, auto-complete will
popup help string."
  (let* ((candidate (ac-selected-candidate))
         (kernel (ein:pytools-get-kernel))
         (callbacks (list :object_info_reply
                          (cons #'ein:ac-set-document candidate))))
    (when (and candidate
               kernel
               (not (get-text-property 0 'document candidate)))
      (ein:log 'debug "Requesting object info for AC candidate %S"
               candidate)
      (ein:kernel-object-info-request kernel candidate callbacks))))

(defun ein:ac-set-document (candidate content -metadata-not-used-)
  (ein:log 'debug "EIN:AC-SET-DOCUMENT candidate=%S content=%S"
           candidate content)
  (put-text-property 0 (length candidate)
                     'document (ein:kernel-construct-help-string content)
                     candidate))

(defadvice ac-next (after ein:ac-next-request)
  "Monkey patch `auto-complete' internal function to request
help documentation asynchronously."
  (ein:ac-request-document-for-selected-candidate))

(defadvice ac-previous (after ein:ac-previous-request)
  "Monkey patch `auto-complete' internal function to request
help documentation asynchronously."
  (ein:ac-request-document-for-selected-candidate))

(defadvice ac-update (after ein:ac-update-request)
  "Monkey patch `auto-complete' internal function to request help
documentation asynchronously.  This will request info for the
first candidate when the `ac-menu' pops up."
  (ein:ac-request-document-for-selected-candidate))

(defadvice ac-prefix
  (around ein:ac-always-dotty (requires ignore-list))
  "Monkey patch `auto-complete' internal function to enable
dotty completion."
  (if (or ein:notebook (ein:eval-if-bound 'ein:@connect))
      (with-syntax-table ein:ac-syntax-table
        ad-do-it)
    ad-do-it))

(defun ein:ac-superpack ()
  "Enable richer auto-completion.

* Enable omni completion by using dotty syntax table for auto-complete.
  Monkey patch `ac-prefix' to make \".\" as a part of word.
* Enable auto-completion help by monkey patching `ac-next'/`ac-previous'"
  (interactive)
  (ad-enable-advice 'ac-next     'after 'ein:ac-next-request)
  (ad-enable-advice 'ac-previous 'after 'ein:ac-previous-request)
  (ad-enable-advice 'ac-update   'after 'ein:ac-update-request)
  (ad-enable-advice 'ac-prefix 'around 'ein:ac-always-dotty)
  (ad-activate 'ac-next)
  (ad-activate 'ac-previous)
  (ad-activate 'ac-update)
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
Specifying non-`nil' to SUPERPACK enables richer auto-completion
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
