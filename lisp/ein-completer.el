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
(require 'ein-pytools)
(require 'ein-ac)
(require 'dash)

(make-obsolete-variable 'ein:complete-on-dot nil "0.15.0")

(defun ein:completer-choose ()
  (cond
   ((eq ein:completion-backend 'ein:use-none-backend) #'ignore)
   ((ein:eval-if-bound 'auto-complete-mode) #'ein:completer-finish-completing-ac)
   (t #'ein:completer-finish-completing-default)))

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

(defun ein:completer-complete (kernel callbacks errback)
  "Start completion for the code at point.

   EXPAND keyword argument is supported by
   `ein:completer-finish-completing-ac'.  When it is specified,
   it overrides `ac-expand-on-auto-complete' when calling
   `auto-complete'."
  (interactive (list (ein:get-kernel) 
                     (list :complete_reply
                           (cons #'ein:completer-finish-completing '(:expand nil)))
                     #'ignore))
  (ein:kernel-complete kernel
                       (thing-at-point 'line)
                       (current-column)
                       callbacks errback))

;;; Retrieving Python Object Info
(defun ein:completions--reset-oinfo-cache (kernel)
  (setf (ein:$kernel-oinfo-cache kernel) (make-hash-table :test #'equal)))

(defun ein:completions-get-cached (partial oinfo-cache)
  (loop for candidate being the hash-keys of oinfo-cache
        when (string-prefix-p partial candidate)
        collect candidate))

(defun ein:completions--get-oinfo (obj)
  (let ((d (deferred:new #'identity))
        (kernel (ein:get-kernel)))
    (if (ein:kernel-live-p kernel)
        (ein:kernel-execute
         kernel
         (format "__ein_print_object_info_for(__ein_maybe_undefined_object(r\"%s\", locals()))" obj)
         (list
          :output `(,(lambda (d* &rest args) (deferred:callback-post d* args)) . ,d)))
      (deferred:callback-post d "kernel not live"))
    d))

(defun ein:completions--build-oinfo-cache (objs)
  (let ((kernel (ein:get-kernel)))
    (dolist (o (-non-nil objs))
      (deferred:$
        (deferred:next
          (lambda ()
            (ein:completions--get-oinfo (ein:trim o "\\s-\\|\n\\|\\."))))
        (deferred:nextc it
          (lambda (output)
            (if (stringp output)
                (ein:display-warning output :error)
              (ein:completions--prepare-oinfo output o kernel))))))))

(defun ein:completions--prepare-oinfo (output obj kernel)
  (condition-case err
      (destructuring-bind (msg-type content _) output
        (ein:case-equal msg-type
          (("stream" "display_data" "pyout" "execute_result")
           (ein:aif (plist-get content :text)
               (setf (gethash obj (ein:$kernel-oinfo-cache kernel)) (ein:json-read-from-string it))))
          (("error" "pyerr")
           ;; This should only happen if ein-pytools is not loaded, which can
           ;; happen if the user restarts the kernel. Could probably use better logic
           ;; to determine if pytools have been loaded or not.
           (ein:pytools-load-safely kernel)
           (ein:log 'verbose "ein:completions--prepare-oinfo: %S" (plist-get content :traceback)))))
    ;; It's okay, bad things happen. Not everything in python is going to have a
    ;; pdef, which might cause the call to the json parser to fail. No need to
    ;; log an error as that will unnecessarily fill the log buffer, but we do
    ;; register a debug message in case someone really needs to know what is
    ;; happening.
    (error (ein:log 'debug "ein:completions--prepare-oinfo: [%s] %s" err obj)
           (setf (gethash obj (ein:$kernel-oinfo-cache kernel)) :json-false))))

;;; Support for Eldoc

(defun ein:completer--get-eldoc-signature ()
  (let ((func (ein:function-at-point))
        (kernel (ein:get-kernel)))
    (ein:aif (gethash func (ein:$kernel-oinfo-cache kernel))
        (ein:kernel-construct-defstring it)
      (ein:completions--build-oinfo-cache (list func)))))

(provide 'ein-completer)

;;; ein-completer.el ends here
