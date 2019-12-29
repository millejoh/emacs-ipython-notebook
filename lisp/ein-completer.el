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
                   (ein:completing-read "Complete: " matches
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
  (multiple-value-bind (code pos) (ein:get-completion-context (ein:$kernel-api-version kernel))
    (ein:log 'debug (format "EIN:COMPLETER-COMPLETE Code block: %s at position :%s" code pos))
    (ein:kernel-complete kernel
                         code ;; (thing-at-point 'line)
                         pos ;; (current-column)
                         callbacks errback)))

(defun ein:get-completion-context (api-version)
  (cond ((< api-version 5)
         (values (thing-at-point 'line) (current-column)))
        ((and (ein:get-kernel) (ein:get-cell-at-point))
         (let* ((cell (ein:get-cell-at-point))
                (code (ein:cell-get-text cell))
                (beg (ein:cell-input-pos-min cell)))
           (values code (- (point) beg))))
        ((ein:get-kernel)
         (values (buffer-string) (1- (point))))))

;;; Retrieving Python Object Info
(defun ein:completions--reset-oinfo-cache (kernel)
  (setf (ein:$kernel-oinfo-cache kernel) (make-hash-table :test #'equal)))

(defun ein:dev-clear-oinfo-cache (kernel)
  (interactive (list (ein:get-kernel)))
  (ein:completions--reset-oinfo-cache kernel))

(defun ein:completions-get-cached (partial oinfo-cache)
  (cl-loop for candidate being the hash-keys of oinfo-cache
           when (string-prefix-p partial candidate)
           collect candidate))

(defun ein:completions--get-oinfo (objs)
  (let ((d (deferred:new #'identity))
        (kernel (ein:get-kernel)))
    (ein:case-equal (ein:kernel-language kernel)
      (("python")
       (if (ein:kernel-live-p kernel)
           (ein:kernel-execute
            kernel
            (format "__ein_generate_oinfo_data(%s, locals())" objs)
            (list
             :output `(,(lambda (d* &rest args) (deferred:callback-post d* args)) . ,d)))
         (deferred:callback-post d "kernel not live"))))
    d))

(defvar ein:oinfo-chunk-size 50)

(defun ein:completions--build-oinfo-cache (objects)
  (cl-labels ((object-string (o)
                             (format "'%s'" (ein:trim o "\\s-\\|\n\\|\\.")))
              (to-ostrings (objs)
                           (s-join ", " (-map #'(lambda (x) (object-string x))
                                              objs)))
              (do-completions (ostrings kernel)
                              (deferred:$
                                (deferred:next
                                  (lambda ()
                                    (ein:completions--get-oinfo ostrings)))
                                (deferred:nextc it
                                  (lambda (output)
                                    (if (stringp output)
                                        (ein:display-warning output :error)
                                      (ein:completions--prepare-oinfo output objects kernel)))))))
    (if (< (length objects) ein:oinfo-chunk-size)
        (do-completions (format "[%s]" (to-ostrings (-non-nil objects))) (ein:get-kernel))
      (dolist (chunk (-partition-all ein:oinfo-chunk-size (-non-nil objects)))
        (do-completions (format "[%s]" (to-ostrings chunk)) (ein:get-kernel))))))


(defun ein:completions--prepare-oinfo (output objs kernel)
  (condition-case err
      (cl-destructuring-bind (msg-type content _) output
        (ein:case-equal msg-type
          (("stream" "display_data" "pyout" "execute_result")
           (ein:aif (plist-get content :text)
               (let ((all-oinfo (ein:json-read-from-string it)))
                 (cl-loop for oinfo in all-oinfo
                          for obj in objs
                          doing (unless (string= (plist-get oinfo :string_form) "None")
                                  (setf (gethash obj (ein:$kernel-oinfo-cache kernel))
                                        oinfo))))))
          (("error" "pyerr")
           (ein:log 'verbose "ein:completions--prepare-oinfo: %s"
                    (plist-get content :traceback)))))
    (error
     (ein:log 'verbose "ein:completions--prepare-oinfo: [%s]"
              (error-message-string err))
     (let (eval-expression-print-length eval-expression-print-level)
       (prin1 output #'external-debugging-output)))))

;;; Support for Eldoc

(defun ein:completer--get-eldoc-signature ()
  (ein:and-let* ((func (ein:function-at-point))
                 (kernel (ein:get-kernel)))
    (ein:aif (gethash func (ein:$kernel-oinfo-cache kernel))
        (ein:kernel-construct-defstring it)
      (ein:completions--build-oinfo-cache (list func))
      nil)))

(provide 'ein-completer)

;;; ein-completer.el ends here
