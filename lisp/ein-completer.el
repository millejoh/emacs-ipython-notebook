;;; ein-completer.el --- Completion module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-completer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-completer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-completer.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(declare-function ac-cursor-on-diable-face-p "auto-complete")

(require 'ein-core)
(require 'ein-log)
(require 'ein-subpackages)
(require 'ein-kernel)

(defun ein:completer-choose ()
  (when (require 'auto-complete nil t)
    (require 'ein-ac))
  (cond
   ((and (or ein:use-auto-complete
             ein:use-auto-complete-superpack)
         (ein:eval-if-bound 'auto-complete-mode)
         (fboundp 'ein:completer-finish-completing-ac))
    #'ein:completer-finish-completing-ac)
   (t
    #'ein:completer-finish-completing-default)))

(defun ein:completer-beginning (matched-text)
  (save-excursion
    (re-search-backward (concat matched-text "\\="))))

(defun ein:completer-finish-completing (_dummy_ content -metadata-not-used-)
  (ein:log 'debug "COMPLETER-FINISH-COMPLETING: content=%S" content)
  (let ((matched-text (plist-get content :matched_text))
        (matches (plist-get content :matches))
        (completer (ein:completer-choose)))
    (ein:log 'debug "COMPLETER-FINISH-COMPLETING: completer=%s" completer)
    (funcall completer matched-text matches)))

(defun ein:completer-finish-completing-default (matched-text matches)
  (let* ((end (point))
         (beg (ein:completer-beginning matched-text))
         (word (if (and beg matches)
                   (completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

(defun* ein:completer-complete
    (kernel
     &optional
     (callbacks (list :complete_reply
                      (cons #'ein:completer-finish-completing nil))))
  (interactive (list (ein:get-kernel)))
  (ein:kernel-complete kernel
                       (thing-at-point 'line)
                       (current-column)
                       callbacks))

(defun ein:completer-dot-complete ()
  "Insert dot and request completion."
  (interactive)
  (insert ".")
  (ein:and-let* ((kernel (ein:get-kernel))
                 ((not (ac-cursor-on-diable-face-p)))
                 ((ein:kernel-live-p kernel)))
    (ein:completer-complete kernel)))

(defcustom ein:complete-on-dot t
  "Start completion when inserting a dot.  Note that
`ein:use-auto-complete' (or `ein:use-auto-complete-superpack')
must be `t' to enable this option.  This variable has effect on
notebook buffers and connected buffers."
  :type 'boolean
  :group 'ein)

(defun ein:complete-on-dot-install (map &optional func)
  (if (and ein:complete-on-dot
           (featurep 'auto-complete)
           (or ein:use-auto-complete
               ein:use-auto-complete-superpack))
      (define-key map "." (or func #'ein:completer-dot-complete))
    (define-key map "." nil)))

(provide 'ein-completer)


;;; ein-completer.el ends here
