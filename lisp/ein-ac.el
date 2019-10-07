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

(require 'auto-complete)

(require 'ein-core)
(eval-when-compile (require 'ein-notebook)
                   (defvar ein:mumamo-codecell-mode))


;;; Configuration
(defcustom ein:use-auto-complete-superpack nil
  "Set to `t' to use preset a little bit hacky auto-complete configuration.
When this option is enabled, cached omni completion is available."
  :type 'boolean
  :group 'ein)

(defvar ein:ac-sources (and (boundp 'ac-sources)
                            (default-value 'ac-sources))
  "Extra `ac-sources' used in notebook.")


;;; Chunk (adapted from auto-complete-chunk.el)

(defvar ein:ac-chunk-regex
  (rx (group (| (syntax whitespace)
                (syntax open-parenthesis)
                (syntax close-parenthesis)
                (syntax string-quote) ; Complete files for `open("path/..`
                bol))
      (? (syntax punctuation))          ; to complete ``~/PATH/...``
      (* (+ (| (syntax word) (syntax symbol)))
         (syntax punctuation))
      (+ (| (syntax word) (syntax symbol)))
      (? (syntax punctuation))
      point)
  "A regexp that matches to a \"chunk\" containing words and dots.")

(defun ein:ac-chunk-beginning ()
  "Return the position where the chunk begins."
  (ignore-errors
    (save-excursion
      (+ (re-search-backward ein:ac-chunk-regex) (length (match-string 1))))))

(defun ein:ac-chunk-candidates-from-list (chunk-list)
  "Return matched candidates in CHUNK-LIST."
  (let* ((start (ein:ac-chunk-beginning)))
    (when start
      (cl-loop with prefix = (buffer-substring start (point))
        for cc in chunk-list
        when (string-prefix-p prefix cc)
        collect cc))))


;;; AC Source

(defvar ein:ac-direct-matches nil
  "Variable to store completion candidates for `auto-completion'.")
;; FIXME: Maybe this should be buffer-local?

(defun ein:ac-direct-get-matches ()
 (ein:ac-chunk-candidates-from-list ein:ac-direct-matches))

(ac-define-source ein-direct
  '((candidates . ein:ac-direct-get-matches)
    (requires . 0)
    (prefix . ein:ac-chunk-beginning)
    (symbol . "s")))

(ac-define-source ein-async
  '((candidates . ein:ac-direct-get-matches)
    (requires . 0)
    (prefix . ein:ac-chunk-beginning)
    (init . ein:ac-request-in-background)
    (symbol . "c")))

(define-obsolete-function-alias 'ac-complete-ein-cached 'ac-complete-ein-async
  "0.2.1")
(define-obsolete-variable-alias 'ac-source-ein-cached 'ac-source-ein-async
  "0.2.1")

(defun ein:ac-request-in-background ()
  (cl-ecase ein:completion-backend
    (ein:use-ac-backend (ein:aif (ein:get-kernel)
                            (ein:completer-complete
                             it
                             (list :complete_reply
                                   (cons (lambda (_ content __)
                                           (ein:ac-prepare-completion (plist-get content :matches)))
                                         nil))
                             #'ignore)))))


;;; Completer interface

(defun ein:ac-dot-complete (callback)
  "Insert a dot and request completion via CALLBACK of 0-arity"
  (interactive (list (lambda () (call-interactively #'ein:completer-complete))))
  (insert ".")
  (if (not (ac-cursor-on-diable-face-p))
      (funcall callback)))

(defun ein:ac-prepare-completion (matches)
  "Prepare `ac-source-ein-direct' using MATCHES from kernel.
Call this function before calling `auto-complete'."
  (when matches
    (setq ein:ac-direct-matches matches)))  ; let-binding won't work

(cl-defun ein:completer-finish-completing-ac
    (matched-text
     matches
     &key (expand ac-expand-on-auto-complete)
     &allow-other-keys)
  "Invoke completion using `auto-complete'.
Only the argument MATCHES is used.  MATCHED-TEXT is for
compatibility with `ein:completer-finish-completing-default'."
  ;; I don't need to check if the point is at right position, as in
  ;; `ein:completer-finish-completing-default' because `auto-complete'
  ;; checks it anyway.

  (ein:log 'debug "COMPLETER-FINISH-COMPLETING-AC: matched-text=%S matches=%S"
           matched-text matches)
  (ein:ac-prepare-completion matches)
  (when matches      ; No auto-complete drop-down list when no matches
    (let ((ac-expand-on-auto-complete expand))
      (ac-start))))


;;; Async document request hack

(defun ein:ac-request-document-for-selected-candidate ()
  "Request object information for the candidate at point.
This is called via `ac-next'/`ac-previous'/`ac-update' and set
`document' property of the current candidate string.  If server
replied within `ac-quick-help-delay' seconds, auto-complete will
popup help string."
  (ein:aif (ein:get-kernel)
      (let* ((candidate (ac-selected-candidate))
             (kernel it)
             (api-version (ein:$kernel-api-version kernel))
             (callbacks (list (if (< api-version 3)
                                  :object_info_reply
                                :inspect_request)
                              (cons #'ein:ac-set-document candidate))))
        (when (and candidate
                   (ein:kernel-live-p kernel)
                   (not (get-text-property 0 'document candidate)))
          (ein:log 'debug "Requesting object info for AC candidate %S"
                   candidate)
          (ein:kernel-object-info-request kernel candidate callbacks)))))

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


;;; Setup

(defun ein:ac-superpack ()
  "Enable richer auto-completion.

* Enable auto-completion help by monkey patching `ac-next'/`ac-previous'"
  (interactive)
  (ad-enable-advice 'ac-next     'after 'ein:ac-next-request)
  (ad-enable-advice 'ac-previous 'after 'ein:ac-previous-request)
  (ad-enable-advice 'ac-update   'after 'ein:ac-update-request)
  (ad-activate 'ac-next)
  (ad-activate 'ac-previous)
  (ad-activate 'ac-update))

(defun ein:ac-setup ()
  "Call this function from mode hook (see `ein:ac-config')."
  (setq ac-sources (append '(ac-source-ein-async) ein:ac-sources)))

(defun ein:ac-setup-maybe ()
  "Setup `ac-sources' for mumamo.

.. note:: Setting `ein:notebook-mumamo-mode-hook' does not work
   because `ac-sources' in `ein:notebook-mumamo-mode'-enabled
   buffer is *chunk local*, rather than buffer local.

   Making `ac-sources' permanent-local also addresses issue of
   MuMaMo discarding `ac-sources'.  However, it effects to entire
   Emacs setting.  So this is not the right way to do it.

   Using `mumamo-make-variable-buffer-permanent' (i.e., adding
   `ac-sources' to `mumamo-per-buffer-local-vars' or
   `mumamo-per-main-major-local-vars') is also not appropriate.
   Adding `ac-sources' to them makes it impossible to different
   `ac-sources' between chunks, which is good for EIN but may not
   for other package."
  (and (ein:eval-if-bound 'ein:notebook-mode)
       (ein:eval-if-bound 'ein:notebook-mumamo-mode)
       (eql major-mode ein:mumamo-codecell-mode)
       (ein:ac-setup)))

(defun ein:ac-config (&optional superpack)
  "Install auto-complete-mode for notebook modes.
Specifying non-`nil' to SUPERPACK enables richer auto-completion
\(see `ein:ac-superpack')."
  (add-hook 'after-change-major-mode-hook 'ein:ac-setup-maybe)
  (add-hook 'ein:notebook-mode-hook 'ein:ac-setup)
  (when superpack
    (ein:ac-superpack)))

(ein:ac-config ein:use-auto-complete-superpack)
(provide 'ein-ac)

;;; ein-ac.el ends here
