;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; ein-company.el --- Support for completion using company back-end.

;; Copyright (C) 2017 - John Miller

;; Author: John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-company.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-company.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-company.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'company nil t)
(require 'dash)
(require 'jedi-core nil t)
(require 'deferred)
(require 'ein-core)

(defun ein:company--deferred-complete ()
  (let ((d (deferred:new #'identity))
        (kernel (ein:get-kernel)))
    (if (ein:kernel-live-p kernel)
        (ein:completer-complete
         kernel
         :callbacks
         (list :complete_reply
               (cons (lambda (d &rest args) (deferred:callback-post d args))
                     d)))
      ;; Pass "no match" result when kernel the request was not sent:
      (deferred:callback-post d (list nil nil)))
    d))

(defun ein:company--complete (fetcher-callback &optional use-jedi)
  (deferred:$
    (deferred:next
      (lambda ()
        (ein:company--deferred-complete)))
    (deferred:nextc it
      (lambda (replies)
        (ein:completions--prepare-matches fetcher-callback replies)))))

(defun ein:completions--prepare-matches (cb replies)
  (destructuring-bind
      ((&key matched_text matches &allow-other-keys) ; :complete_reply
       _)
      replies
    (funcall cb matches)))

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

(let ((pdef-cache (make-hash-table :test #'equal)))
  (defun ein:clear-pdf-cache ()
    (clrhash pdef-cache))

  (defun ein:completions--get-pdef (callback obj)
    (ein:aif (gethash obj pdef-cache nil)
        (funcall callback it)
      (deferred:$
        (deferred:next
          (lambda ()
            (ein:completions--get-oinfo obj)))
        (deferred:nextc it
          (lambda (output)
            (ein:completions--prepare-pdef callback output obj))))))

  (defun ein:completions--prepare-pdef (callback output obj)
    (destructuring-bind (msg-type content _) output
      (ein:case-equal msg-type
        (("stream" "display_data")
         (condition-case _
             (let* ((oinfo (ein:json-read-from-string (plist-get content :text)))
                    (pdef (plist-get oinfo :definition)))
               (setf (gethash obj pdef-cache) pdef)
               (funcall callback pdef))
           (error (funcall callback ""))))))))



;;;###autoload
(defun ein:company-backend (command &optional arg &rest _)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend) )
    (prefix (and (--filter (and (boundp it) (symbol-value it) (eql it 'ein:notebook-minor-mode))
                           minor-mode-list)
                 (ein:object-at-point)))
    (annotation (if ein:allow-company-annotations
                    (cons :async
                          (lambda (cb)
                            (ein:completions--get-pdef cb arg)))))
    (doc-buffer (lexical-let ((arg arg))
                  (cons :async
                        (lambda (cb)
                          (ein:company-handle-doc-buffer arg cb)))))
    (location (lexical-let ((obj arg))
                (cons :async
                      (lambda (cb)
                        (ein:pytools-find-source (ein:get-kernel-or-error)
                                                 obj
                                                 cb)))))
    (candidates () (lexical-let ((kernel (ein:get-kernel-or-error))
                                 (col (current-column)))
                     (unless (ein:company-backend--punctuation-check (thing-at-point 'line) col)
                       (cons :async
                             (lambda (cb)
                               (ein:company--complete cb))))))))

;; (ein:kernel-complete kernel
;;                      (thing-at-point 'line)
;;                      col
;;                      (list :complete_reply
;;                            (cons #'ein:completer-finish-completing-company
;;                                  (list :callback cb))))

(defun ein:company-backend--punctuation-check (thing col)
  (let ((query (ein:trim-right (subseq thing 0 col) "[\n]")))
    (string-match "[]()\",[{}'=: ]$" query (- col 2))))

;; (cl-defun ein:completer-finish-completing-company (packed content -metadata-not-used-)
;;   (ein:log 'debug "EIN:COMPANY-FINISH-COMPLETING: content=%S" content)
;;   (let* ((beg (point))
;;          (delta (- (plist-get content :cursor_end)
;;                    (plist-get content :cursor_start)))
;;          (matched-text (buffer-substring beg (- beg delta)))
;;          (matches (-filter #'(lambda (s)
;;                                (s-starts-with-p matched-text s))
;;                            (plist-get content :matches))))
;;     (ein:log 'debug "EIN:COMPANY-FINISH-COMPLETING: matches=%s" matches)
;;     (funcall (plist-get packed :callback) matches)))

(defun ein:company-handle-doc-buffer-finish (packed content -metadata-not-used-)
  (when (plist-get content :found)
    (funcall (plist-get packed :callback) (company-doc-buffer
                                           (ansi-color-apply (cadr (plist-get content :data)))))))

(defun ein:company-handle-doc-buffer (object cb)
  (ein:kernel-object-info-request (ein:get-kernel-or-error)
                                  object
                                  (list :inspect_reply
                                        (cons #'ein:company-handle-doc-buffer-finish
                                              (list :object object
                                                    :callback cb)))))

(defun ein:company-handle-meta (object cb)
  )

(setq ein:complete-on-dot nil)

(provide 'ein-company)
