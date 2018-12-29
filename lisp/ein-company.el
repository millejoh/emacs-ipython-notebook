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

(declare-function jedi:complete-request "jedi-core")

(autoload 'company-begin-backend "company")
(autoload 'company-doc-buffer "company")

;; Duplicates ein:jedi--completer-complete in ein-jedi.
;; Let's refactor and enhance our calm!
(defun ein:company--deferred-complete ()
  (let ((d (deferred:new #'identity)))
    (ein:completer-complete
     (ein:get-kernel)
     (list :complete_reply
           (cons (lambda (d* &rest args) (deferred:callback-post d* args))
                 d))
     (apply-partially (lambda (d* err) (deferred:callback-post d* err)) d))
    d))

(defun ein:company--complete (prefix fetcher)
  (deferred:$
    (deferred:next
      (lambda ()
        (ein:company--deferred-complete)))
    (deferred:nextc it
      (lambda (replies)
        (unless (stringp replies) ;; if not an error
          (ein:completions--prepare-matches prefix fetcher replies))))))

(defun ein:company--complete-jedi (fetcher)
  (deferred:$
    (deferred:parallel
      (jedi:complete-request)
     (ein:company--deferred-complete))
    (deferred:nextc it
      (lambda (replies)
        (ein:completions--prepare-matches-jedi fetcher replies)))))

(defun ein:completions--prepare-matches-jedi (cb replies)
  (destructuring-bind
      (_ ((&key matches &allow-other-keys) ; :complete_reply
          _metadata))
      replies
    (ein:completions--build-oinfo-cache matches)
    (funcall cb matches)))

(defun ein:completions--prepare-matches (prefix fetcher replies)
  (destructuring-bind
      ((&key matches cursor_start cursor_end &allow-other-keys) ; :complete_reply
       _metadata)
      replies
    (let ((nix (- cursor_end cursor_start))
          prefixed-matches)
      (dolist (match matches)
        (setq prefixed-matches 
              (nconc prefixed-matches (list (concat prefix (substring match nix))))))
      (ein:completions--build-oinfo-cache prefixed-matches)
      (funcall fetcher prefixed-matches))))

;;;###autoload
(defun ein:company-backend (command &optional arg &rest _)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ein:company-backend))
    (prefix (and (eq major-mode 'ein:notebook-multilang-mode) (ein:object-at-point)))
    (annotation (let ((kernel (ein:get-kernel)))
                  (ein:aif (gethash arg (ein:$kernel-oinfo-cache kernel))
                           (plist-get it :definition))))
    (doc-buffer (cons :async
                      (lambda (cb)
                        (ein:company-handle-doc-buffer arg cb))))
    (location (cons :async
                    (lambda (cb)
                      (ein:pytools-find-source (ein:get-kernel-or-error)
                                               arg
                                               cb))))
    (candidates 
     (let* ((kernel (ein:get-kernel-or-error))
            (cached (ein:completions-get-cached arg (ein:$kernel-oinfo-cache kernel))))
       (ein:aif cached it
         (unless (ein:company--punctuation-check (thing-at-point 'line) (current-column))
           (case ein:completion-backend
             (ein:use-company-jedi-backend
              (cons :async (lambda (cb)
                             (ein:company--complete-jedi cb))))
             (t
              (cons :async
                    (lambda (cb)
                      (ein:company--complete arg cb)))))))))))

(defun ein:company--punctuation-check (thing col)
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

(when (boundp 'company-backends)
  (add-to-list 'company-backends 'ein:company-backend))

(provide 'ein-company)
