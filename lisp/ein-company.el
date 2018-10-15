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
(require 'jedi-core nil t)
(require 'deferred)
(require 'ein-completer)
(require 'company nil t)

(autoload 'company-begin-backend "company")
(autoload 'company-doc-buffer "company")

;; Duplicates ein:jedi--completer-complete in ein-jedi.
;; Let's refactor and enhance our calm!
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

(defun ein:company--complete (fetcher-callback)
  (deferred:$
    (deferred:next
      (lambda ()
        (ein:company--deferred-complete)))
    (deferred:nextc it
      (lambda (replies)
        (ein:completions--prepare-matches fetcher-callback replies)))))

(defun ein:company--complete-jedi (fetcher-callback)
  (deferred:$
    (deferred:parallel
      ;;     (jedi:complete-request) ;; we need tkf-emacs submodule
     (ein:company--deferred-complete))
    (deferred:nextc it
      (lambda (replies)
        (ein:completions--prepare-matches-jedi fetcher-callback replies)))))

(defun ein:completions--prepare-matches-jedi (cb replies)
  (destructuring-bind
      (_ ((&key matches &allow-other-keys) ; :complete_reply
          _))
      replies
    (ein:completions--build-oinfo-cache matches)
    (funcall cb matches)))

(defun ein:completions--prepare-matches (cb replies)
  (destructuring-bind
      ((&key _matched_text matches &allow-other-keys) ; :complete_reply
       _)
      replies
    (ein:completions--build-oinfo-cache matches)
    (funcall cb matches)))

;;;###autoload
(defun ein:company-backend (command &optional arg &rest _)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend) )
    (prefix (and (--filter (and (boundp it) (symbol-value it) (or (eql it 'ein:notebook-minor-mode)
                                                                  (eql it 'ein:connect-mode)))
                           minor-mode-list)
                 (ein:object-at-point)))
    (annotation (if ein:allow-company-annotations
                    (ein:aif (gethash arg *ein:oinfo-cache*)
                        (plist-get it :definition)
                      "")))
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
                       (case ein:completion-backend
                         (ein:use-company-jedi-backend
                          (cons :async (lambda (cb)
                                         (ein:company--complete-jedi cb))))
                         (t
                          (cons :async
                                  (lambda (cb)
                                    (ein:company--complete cb))))))))))


(defun ein:company-backend--punctuation-check (thing col)
  (let ((query (ein:trim-right (subseq thing 0 col) "[\n]")))
    (string-match "[]()\",[{}'=: ]$" query (- col 2))))


(defun ein:company-handle-doc-buffer-finish (packed content _metadata-not-used_)
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

(setq ein:complete-on-dot nil)

(provide 'ein-company)
