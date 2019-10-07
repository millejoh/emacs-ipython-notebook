;;; ein-core.el --- EIN core   -*- lexical-binding: t -*-

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

;; Optional dependency on tramp:
(declare-function tramp-make-tramp-file-name "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")


(require 'ein)  ; get autoloaded functions into namespace
(require 'ein-utils)

(defvar ein:force-sync) ; defcustom in ein-contents-api which requires this file
(defvar ein:content-query-timeout) ; likewise

(defgroup ein nil
  "IPython notebook client in Emacs"
  :group 'applications
  :prefix "ein:")

;;; Configuration

(defcustom ein:url-or-port '(8888)
  "List of default url-or-port values.
This will be used for completion. So put your IPython servers.
You can connect to servers not in this list \(but you will need
to type every time)."
  :type '(repeat (choice (integer :tag "Port number" 8888)
                         (string :tag "URL" "http://127.0.0.1:8888")))
  :group 'ein)

(defcustom ein:default-url-or-port nil
  "Default URL or port.  This should be your main IPython
Notebook server."
  :type '(choice (integer :tag "Port number" 8888)
                 (string :tag "URL" "http://127.0.0.1:8888")
                 (const :tag "First value of `ein:url-or-port'" nil))
  :group 'ein)

(defcustom ein:filename-translations nil
  "Convert file paths between Emacs and Python process.

This value can take these form:

alist
    Its key specifies URL-OR-PORT and value must be a list of two
    functions: (TO-PYTHON FROM-PYTHON).  Key (URL-OR-PORT) can be
    string (URL), integer (port), or `default' (symbol).  The
    value of `default' is used when other key does not much.
function
    Called with an argument URL-OR-PORT (integer or string).
    This function must return a list of two functions:
    (TO-PYTHON FROM-PYTHON).

Here, the functions TO-PYTHON and FROM-PYTHON are defined as:

TO-PYTHON
    A function which converts a file name (returned by
    `buffer-file-name') to the one Python understands.
FROM-PYTHON
    A function which converts a file path returned by
    Python process to the one Emacs understands.

Use `ein:tramp-create-filename-translator' to easily generate the
pair of TO-PYTHON and FROM-PYTHON."
  ;; I've got the idea from `slime-filename-translations'.
  :type '(choice
          (alist :tag "Translations mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (list (function :tag "TO-PYTHON")
                                   (function :tag "FROM-PYTHON")))
          (function :tag "Translations getter"))
  :group 'ein)



;;; Constants

(defconst ein:source-dir (file-name-directory load-file-name)
  "Directory in which `ein*.el` files are located.")


;;; Configuration getter

(defun ein:default-url-or-port ()
  (or ein:default-url-or-port (car ein:url-or-port) 8888))

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

(defvar *ein:notebook-version* (make-hash-table :test #'equal)
  "url-or-port to major notebook version")

(defvar *ein:kernelspecs* (make-hash-table :test #'equal)
  "url-or-port to kernelspecs")

(defun ein:get-kernelspec (url-or-port name)
  (let* ((kernelspecs (ein:need-kernelspecs url-or-port))
         (name (if (stringp name)
                   (intern (format ":%s" name))
                 name))
         (ks (plist-get kernelspecs name)))
    (if (stringp ks)
        (ein:get-kernelspec url-or-port ks)
      ks)))

(defun ein:need-kernelspecs (url-or-port)
  "Callers assume ein:query-kernelspecs succeeded.  If not, nil."
  (ein:aif (gethash url-or-port *ein:kernelspecs*) it
    (ein:log 'warn "No recorded kernelspecs for %s" url-or-port)
    nil))

(defun ein:query-kernelspecs (url-or-port callback &optional iteration)
  "Send for kernelspecs of URL-OR-PORT with CALLBACK arity 0 (just a semaphore)"
  (unless iteration
    (setq iteration 0))
  (ein:query-singleton-ajax
   (list 'ein:query-kernelspecs url-or-port)
   (ein:url url-or-port "api/kernelspecs")
   :type "GET"
   :timeout ein:content-query-timeout
   :parser 'ein:json-read
   :sync ein:force-sync
   :complete (apply-partially #'ein:query-kernelspecs--complete url-or-port)
   :success (apply-partially #'ein:query-kernelspecs--success url-or-port callback)
   :error (apply-partially #'ein:query-kernelspecs--error url-or-port callback iteration)))

(defun ein:normalize-kernelspec-language (name)
  "Normalize the kernelspec language string"
  (if (stringp name)
      (replace-regexp-in-string "[ ]" "-" name)
    name))

(cl-defun ein:query-kernelspecs--success (url-or-port callback
                                          &key data _symbol-status _response &allow-other-keys)
  (let ((ks (list :default (plist-get data :default)))
        (specs (ein:plist-iter (plist-get data :kernelspecs))))
    (setf (gethash url-or-port *ein:kernelspecs*)
          (ein:flatten (dolist (spec specs ks)
                         (let ((name (car spec))
                               (info (cdr spec)))
                           (push (list name (make-ein:$kernelspec :name (plist-get info :name)
                                                                  :display-name (plist-get (plist-get info :spec)
                                                                                           :display_name)
                                                                  :resources (plist-get info :resources)
                                                                  :language (ein:normalize-kernelspec-language
                                                                             (plist-get (plist-get info :spec)
                                                                                        :language))
                                                                  :spec (plist-get info :spec)))
                                 ks))))))
  (when callback (funcall callback)))

(cl-defun ein:query-kernelspecs--error (url-or-port callback iteration
                                        &key response error-thrown &allow-other-keys)
  (if (< iteration 3)
      (progn
        (ein:log 'verbose "Retry kernelspecs #%s in response to %s" iteration (request-response-status-code response))
        (ein:query-kernelspecs url-or-port callback (1+ iteration)))
    (ein:log 'error
             "ein:query-kernelspecs--error %s: ERROR %s DATA %s" url-or-port (car error-thrown) (cdr error-thrown))
    (when callback (funcall callback))))

(cl-defun ein:query-kernelspecs--complete (_url-or-port &key data response &allow-other-keys
                                           &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:query-kernelspecs--complete %s" resp-string))

(defsubst ein:notebook-version-numeric (url-or-port)
  (truncate (string-to-number (ein:need-notebook-version url-or-port))))

(defun ein:need-notebook-version (url-or-port)
  "Callers assume ein:query-notebook-version succeeded.  If not, we hardcode a guess."
  (ein:aif (gethash url-or-port *ein:notebook-version*) it
    (ein:log 'warn "No recorded notebook version for %s" url-or-port)
    "5.7.0"))

(defun ein:query-notebook-version (url-or-port callback)
  "Send for notebook version of URL-OR-PORT with CALLBACK arity 0 (just a semaphore)"
  (ein:query-singleton-ajax
   (list 'query-notebook-version url-or-port)
   (ein:url url-or-port "api")
   :parser #'ein:json-read
   :sync ein:force-sync
   :complete (apply-partially #'ein:query-notebook-version--complete url-or-port callback)))

(cl-defun ein:query-notebook-version--complete (url-or-port callback
                                                &key data response &allow-other-keys
                                                &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:query-notebook-version--complete %s" resp-string)
  (ein:aif (plist-get data :version)
      (setf (gethash url-or-port *ein:notebook-version*) it)
    (cl-case (request-response-status-code response)
      (404 (ein:log 'warn "notebook version api not implemented")
           (setf (gethash url-or-port *ein:notebook-version*) "2.0.0"))
      (t (ein:log 'warn "notebook version currently unknowable"))))
  (when callback (funcall callback)))

;;; File name translation (tramp support)

;; Probably it's better to define `ein:filename-translations-get' as
;; an EIEIO method so that I don't have to re-define functions such as
;; `ein:kernel-filename-to-python' and `ein:kernel-filename-from-python'.

(defun ein:filename-translations-get (url-or-port)
  (ein:choose-setting 'ein:filename-translations url-or-port))

(defun ein:filename-to-python (url-or-port filename)
  (ein:aif (car (ein:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein:filename-from-python (url-or-port filename)
  (ein:aif (cadr (ein:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein:make-tramp-file-name (username remote-host python-filename)
  "Old (with multi-hops) tramp compatibility function.
Adapted from `slime-make-tramp-file-name'."
  (if (boundp 'tramp-multi-methods)
      (tramp-make-tramp-file-name nil nil
                                  username
                                  remote-host
                                  python-filename)
    (tramp-make-tramp-file-name nil
                                username
                                remote-host
                                python-filename)))

(defun ein:tramp-create-filename-translator (remote-host &optional username)
  "Generate a pair of TO-PYTHON and FROM-PYTHON for
`ein:filename-translations'.

Usage::

    (setq ein:filename-translations
          `((8888
             . ,(ein:tramp-create-filename-translator \"MY-HOSTNAME\"))))
    ;; Equivalently:
    (setq ein:filename-translations
          (lambda (url-or-port)
            (when (equal url-or-port 8888)
              (ein:tramp-create-filename-translator \"MY-HOSTNAME\"))))

This setting assumes that the IPython server which can be
connected using the port 8888 in localhost is actually running in
the host named MY-HOSTNAME.

Adapted from `slime-create-filename-translator'."
  (require 'tramp)
  (let ((remote-host remote-host)
        (username (or username (user-login-name))))
    (list (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          (lambda (python-filename)
            (ein:make-tramp-file-name username remote-host python-filename)))))



;;; Generic getter

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
                        ein:get-url-or-port--shared-output
                        ein:get-url-or-port--connect)))

(defun ein:get-notebook ()
  (ein:generic-getter '(ein:get-notebook--notebook
                        ;; ein:get-notebook--shared-output
                        ein:get-notebook--connect)))

(defun ein:get-notebook-or-error ()
  (or (ein:get-notebook)
      (error "No notebook related to the current buffer.")))

(defun ein:get-kernel ()
  (ein:generic-getter '(ein:get-kernel--notebook
                        ein:get-kernel--worksheet
                        ein:get-kernel--shared-output
                        ein:get-kernel--connect
                        ein:get-kernel--worksheet-in-edit-cell)))

(defun ein:get-kernel-or-error ()
  (or (ein:get-kernel)
      (error "No kernel related to the current buffer.")))

(defun ein:get-cell-at-point ()
  (ein:generic-getter '(ein:get-cell-at-point--worksheet
                        ein:get-cell-at-point--shared-output)))

(defun ein:get-traceback-data ()
  (ein:generic-getter '(ein:get-traceback-data--worksheet
                        ein:get-traceback-data--shared-output
                        ein:get-traceback-data--connect)))



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
         (errors (mapcan (lambda (f) (unless (byte-compile-file f) (list f)))
                         files)))
    (ein:aif errors
        (error "Got %s errors while compiling these files: %s"
               (length errors)
               (ein:join-str " " (mapcar #'file-name-nondirectory it))))
    (message "Compiled %s files" (length files))))

(provide 'ein-core)

;;; ein-core.el ends here
