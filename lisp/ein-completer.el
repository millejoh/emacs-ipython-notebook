;;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; ein-completer.el --- Completion module

;; Copyright (C) 2018- Takafumi Arakaki / John Miller

;; Author: Takafumi Arakaki <aka.tkf at gmail.com> / John Miller <millejoh at mac.com>

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

(eval-when-compile (require 'cl))

(require 'ein-core)
(require 'ein-log)
(require 'ein-subpackages)
(require 'ein-kernel)

(defun ein:completer-choose ()
  (require 'ein-ac)
  (cond
   ((and (or (eql ein:completion-backend 'ein:use-ac-backend)
             (eql ein:completion-backend 'ein:use-ac-jedi-backend))
         (ein:eval-if-bound 'auto-complete-mode)
         (fboundp 'ein:completer-finish-completing-ac))
    #'ein:completer-finish-completing-ac)
   (t
    #'ein:completer-finish-completing-default)))

(defun ein:completer-beginning (matched-text)
  (save-excursion
    (re-search-backward (concat matched-text "\\="))))

(defun ein:completer-finish-completing (args content _metadata)
  (ein:log 'debug "COMPLETER-FINISH-COMPLETING: content=%S" content)
  (let* ((beg (point))
         (delta (- (plist-get content :cursor_end)
                   (plist-get content :cursor_start)))
         (matched-text (buffer-substring beg (- beg delta)))
         (matches (plist-get content :matches))
         (completer (ein:completer-choose)))
    (ein:log 'debug "COMPLETER-FINISH-COMPLETING: completer=%s" completer)
    (apply completer matched-text matches args)))

(defun ein:completer-finish-completing-default (matched-text matches
                                                &rest _ignore)
  (let* ((end (point))
         (beg (ein:completer-beginning matched-text))
         (word (if (and beg matches)
                   (completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

(defun* ein:completer-complete
    (kernel &rest args &key callbacks &allow-other-keys)
  "Start completion for the code at point.

.. It sends `:complete_request' to KERNEL.
   CALLBACKS is passed to `ein:kernel-complete'.

   If you specify CALLBACKS explicitly (i.e., you are not using
   `ein:completer-finish-completing'), keyword argument will be
   ignored.  Otherwise, ARGS are passed as additional arguments
   to completer callback functions.  ARGS **must** be keyword
   arguments.

   EXPAND keyword argument is supported by
   `ein:completer-finish-completing-ac'.  When it is specified,
   it overrides `ac-expand-on-auto-complete' when calling
   `auto-complete'."
  (interactive (list (ein:get-kernel)))
  (unless callbacks
    (setq callbacks
          (list :complete_reply
                (cons #'ein:completer-finish-completing
                      (ein:plist-exclude args '(:callbacks))))))
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
    (ein:completer-complete kernel :expand nil)))

(defcustom ein:complete-on-dot t
  "Start completion when inserting a dot.  Note that
`ein:use-auto-complete' (or `ein:use-auto-complete-superpack')
must be `t' to enable this option.  This variable has effect on
notebook buffers and connected buffers."
  :type 'boolean
  :group 'ein-completion)

(defun ein:complete-on-dot-install (map &optional func)
  (if (and ein:complete-on-dot
           (featurep 'auto-complete)
           (or (eql ein:completion-backend 'ein:use-ac-backend)
               (eql ein:completion-backend 'ein:use-ac-jedi-backend)))
      (define-key map "." (or func #'ein:completer-dot-complete))
    (define-key map "." nil)))


;;; Retrieving Python Object Info
(defvar *ein:oinfo-cache* (make-hash-table :test #'equal))

(defun ein:completions--get-oinfo (obj)
  (let ((d (deferred:new #'identity))
        (kernel (ein:get-kernel)))
    (if (ein:kernel-live-p kernel)
        (ein:kernel-execute
         kernel
         (format "__import__('ein').print_object_info_for(%s)" obj)
         (list
          :output (cons (lambda (d &rest args) (deferred:callback-post d args))
                        d)))
      (deferred:callback-post d (list nil nil)))
    d))

(defun ein:clear-oinfo-cache ()
  (clrhash *ein:oinfo-cache*))

(defun ein:completions--build-oinfo-cache (objs)
  (dolist (o objs)
    (deferred:$
      (deferred:next
        (lambda ()
          (ein:completions--get-oinfo o)))
      (deferred:nextc it
        (lambda (output)
          (ein:completions--prepare-oinfo output o))))))

(defun ein:completions--prepare-oinfo (output obj)
  (condition-case _
      (destructuring-bind (msg-type content _) output
        (ein:case-equal msg-type
          (("stream" "display_data")
           (let* ((oinfo (ein:json-read-from-string (plist-get content :text))))
             (setf (gethash obj *ein:oinfo-cache*) oinfo)))))
    (error (setf (gethash obj *ein:oinfo-cache*) ""))))

;;; Support for Eldoc

(defun ein:completer--get-eldoc-signature ()
  (let* ((func (ein:function-at-point))
         (oinfo (gethash func *ein:oinfo-cache* nil)))
    (if (not oinfo)
        (ein:completions--build-oinfo-cache (list func))
      (plist-get oinfo :definition))))

(defun ein:notebook--enable-eldoc ()
  (set (make-local-variable 'eldoc-documentation-function)
       #'ein:completer--get-eldoc-signature))

(provide 'ein-completer)

;;; ein-completer.el ends here
