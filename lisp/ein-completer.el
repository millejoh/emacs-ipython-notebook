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

(require 'ein-core)
(require 'ein-log)
(require 'ein-subpackages)
(require 'ein-kernel)
(require 'ein-pytools)
(require 'dash)

(make-obsolete-variable 'ein:complete-on-dot nil "0.15.0")

(defun ein:completer-choose ()
  (cond
   ((eq ein:completion-backend 'ein:use-none-backend) #'ignore)
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
                   (ein:completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

;;; Retrieving Python Object Info
(defun ein:completions--reset-oinfo-cache (kernel)
  (setf (ein:$kernel-oinfo-cache kernel) (make-hash-table :test #'equal)))

(defun ein:completions-get-cached (partial oinfo-cache)
  (loop for candidate being the hash-keys of oinfo-cache
        when (string-prefix-p partial candidate)
        collect candidate))

(defun ein:completions--get-oinfo (_obj)
  (let ((d (deferred:new #'identity))
        (_kernel (ein:get-kernel)))
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
           (aif (plist-get content :text)
               (let ((oinfo (ein:json-read-from-string it)))
                 (unless (string= (plist-get oinfo :string_form) "None")
                   (setf (gethash obj (ein:$kernel-oinfo-cache kernel))
                         oinfo)))))
          (("error" "pyerr")
           (ein:log 'verbose "ein:completions--prepare-oinfo: %s"
                    (plist-get content :traceback)))))
    (error
     (ein:log 'verbose "ein:completions--prepare-oinfo: [%s]"
              (error-message-string err))
     (let (eval-expression-print-length eval-expression-print-level)
       (prin1 output #'external-debugging-output))
     (setf (gethash obj (ein:$kernel-oinfo-cache kernel)) :json-false))))

(provide 'ein-completer)

;;; ein-completer.el ends here
