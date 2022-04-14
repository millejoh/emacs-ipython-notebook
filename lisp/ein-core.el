;;; ein-core.el --- EIN core    -*- lexical-binding:t -*-

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-core.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-core.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-core.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein)  ; get autoloaded functions into namespace
(require 'ein-utils)
(require 'anaphora)
(require 'request)

(defgroup ein nil
  "IPython notebook client in Emacs"
  :group 'applications
  :prefix "ein:")

(define-obsolete-variable-alias 'ein:url-or-port 'ein:urls "0.17.0")
(defcustom ein:urls nil
  "List of default urls."
  :type '(repeat (choice (string :tag "Remote url")
                         (integer :tag "Local port" 8888)))
  :group 'ein)

(make-obsolete-variable 'ein:default-url-or-port nil "0.17.0")

(defconst ein:source-dir (file-name-directory load-file-name)
  "Directory in which `ein*.el` files are located.")

(defun ein:version (&optional interactively copy-to-kill)
  "Return a longer version string.
With prefix argument, copy the string to kill ring.
The result contains `ein:version' and either git revision (if
the source is in git repository) or elpa version."
  (interactive (list t current-prefix-arg))
  (let* ((version
          (or (and (ein:git-root-p
                    (concat (file-name-as-directory ein:source-dir) ".."))
                   (let ((default-directory ein:source-dir))
                     (ein:git-revision-dirty)))
              (and (string-match "/ein-\\([0-9\\.]*\\)/$" ein:source-dir)
                   (match-string 1 ein:source-dir)))))
    (when interactively
      (message "EIN version is %s" version))
    (when copy-to-kill
      (kill-new version))
    version))

;;; Server attribute getters.  These should be moved to ein-open.el

(defvar *ein:notebook-api-version* (make-hash-table :test #'equal)
  "url-or-port to major notebook version")

(defvar *ein:kernelspecs* (make-hash-table :test #'equal)
  "url-or-port to kernelspecs")

(defun ein:get-kernelspec (url-or-port name &optional lang)
  (let* ((kernelspecs (ein:need-kernelspecs url-or-port))
         (name (if (stringp name)
                   (intern (format ":%s" name))
                 name))
         (ks (or (plist-get kernelspecs name)
                 (cl-loop for (_key spec) on (ein:plist-exclude kernelspecs '(:default)) by 'cddr
                       if (string= (ein:$kernelspec-language spec) lang)
                       return spec
                       end))))
    (cond ((stringp ks)
           (ein:get-kernelspec url-or-port ks))
          (t ks))))

(defun ein:need-kernelspecs (url-or-port)
  "Callers assume ein:query-kernelspecs succeeded.  If not, nil."
  (aif (gethash url-or-port *ein:kernelspecs*) it
    (ein:log 'warn "No recorded kernelspecs for %s" url-or-port)
    nil))

(defsubst ein:notebook-api-version-numeric (url-or-port)
  (truncate (string-to-number (ein:need-notebook-api-version url-or-port))))

(defun ein:need-notebook-api-version (url-or-port)
  "Callers assume `ein:query-notebook-api-version' succeeded.
If not, we hardcode a guess."
  (aif (gethash url-or-port *ein:notebook-api-version*) it
    (ein:log 'warn "No recorded notebook version for %s" url-or-port)
    "5"))

(defun ein:generic-getter (func-list)
  "Internal function for generic getter functions (`ein:get-*').

FUNC-LIST is a list of function which takes no argument and
return what is desired or nil.  Each function in FUNC-LIST is
called one by one and the first non-nil result will be used.  The
function is not called when it is not bound.  So, it is safe to
give functions defined in lazy-loaded sub-modules.

This is something similar to dispatching in generic function such
as `defgeneric' in EIEIO, but it takes no argument.  Actual
implementation is chosen based on context (buffer, point, etc.).
This helps writing generic commands which requires same object
but can operate in different contexts."
  (cl-loop for func in func-list
        if (and (functionp func) (funcall func))
        return it))

(defun ein:get-url-or-port ()
  (ein:generic-getter '(ein:get-url-or-port--notebooklist
                        ein:get-url-or-port--notebook
                        ein:get-url-or-port--worksheet
                        ein:get-url-or-port--shared-output)))

(defun ein:get-kernel ()
  (ein:generic-getter '(ein:get-kernel--notebook
                        ein:get-kernel--worksheet
                        ein:get-kernel--shared-output
                        ein:get-kernel--connect)))

(defun ein:get-kernel-or-error ()
  (or (ein:get-kernel)
      (error "No kernel related to the current buffer.")))

(defun ein:get-cell-at-point ()
  (ein:generic-getter '(ein:get-cell-at-point--worksheet
                        ein:get-cell-at-point--shared-output)))

(defun ein:get-traceback-data ()
  (append (ein:generic-getter '(ein:get-traceback-data--worksheet
                                ein:get-traceback-data--shared-output
                                ein:get-traceback-data--connect))
          nil))

;;; Emacs utilities

(defun ein:clean-compiled-files ()
  (let* ((files (directory-files ein:source-dir 'full "^ein-.*\\.elc$")))
    (mapc #'delete-file files)
    (message "Removed %s byte-compiled files." (length files))))

(defun ein:byte-compile-ein ()
  "Byte compile EIN files."
  (interactive)
  (ein:clean-compiled-files)
  (let* ((files (directory-files ein:source-dir 'full "^ein-.*\\.el$"))
         (errors (cl-mapcan (lambda (f) (unless (byte-compile-file f) (list f)))
                            files)))
    (aif errors
        (error "Got %s errors while compiling these files: %s"
               (length errors)
               (ein:join-str " " (mapcar #'file-name-nondirectory it))))
    (message "Compiled %s files" (length files))))

(provide 'ein-core)

;;; ein-core.el ends here
