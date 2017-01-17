;;; ein-core.el --- EIN core

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

(eval-when-compile (require 'cl))

;; Optional dependency on tramp:
(declare-function tramp-make-tramp-file-name "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")


(require 'ein)  ; get autoloaded functions into namespace
(require 'ein-utils)


(defgroup ein nil
  "IPython notebook client in Emacs"
  :group 'applications
  :prefix "ein:")

(defvar ein:version "0.12.1"
  "Version number for Emacs IPython Notebook (EIN).")


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

(defvar ein:source-dir (file-name-directory load-file-name)
  "Directory in which ``ein*.el`` locate.")


;;; Configuration getter

(defun ein:default-url-or-port ()
  (or ein:default-url-or-port (car ein:url-or-port) 8888))

(defun ein:version ()
  "Return a string containing `ein:version' and git revision if
the source is in git repository."
  (ein:aif (when (ein:git-root-p
                  (concat (file-name-as-directory ein:source-dir) ".."))
             (let ((default-directory ein:source-dir))
               (ein:git-revision-dirty)))
      (concat ein:version "." it)
    ein:version))

(defvar *running-ipython-version* (make-hash-table))

(defun ein:get-ipython-major-version (vstr)
  (string-to-number (car (split-string vstr "\\."))))

;; TODO: Use symbols instead of numbers for ipython version ('jupyter and 'legacy)?
(defun ein:query-ipython-version (&optional url-or-port force)
  (ein:aif (and (not force) (gethash (or url-or-port (ein:default-url-or-port)) *running-ipython-version*))
      it
    (let ((resp (request (ein:url (or url-or-port
                                      (ein:default-url-or-port))
                                  "api")
                         :parser #'(lambda ()
                                     (ignore-errors
                                       (ein:json-read)))
                         :timeout 0.5
                         :sync t)))
      (if (eql 404 (request-response-status-code resp))
          (progn
            (ein:log 'blather "Version api not implemented, assuming we are working with IPython 2.x")
            (setf (gethash url-or-port *running-ipython-version*) 2))
        (setf (gethash url-or-port *running-ipython-version*)
              (ein:get-ipython-major-version (plist-get (request-response-data resp) :version)))))))

(defun ein:force-ipython-version-check ()
  (interactive)
  (maphash #'(lambda (url-or-port --ignore--)
               (ein:query-ipython-version url-or-port t))
           *running-ipython-version*))


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
  (lexical-let ((remote-host remote-host)
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
  (loop for func in func-list
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
                        ein:get-kernel--connect)))

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

(defun ein:byte-compile-ein ()
  "Byte compile EIN files."
  (interactive)
  (let* ((files (directory-files ein:source-dir 'full "^ein-.*\\.el$"))
         (errors (ein:filter
                  'identity
                  (mapcar (lambda (f) (unless (byte-compile-file f) f))
                          files))))
    (ein:aif errors
        (error "Got %s errors while compiling these files: %s"
               (length errors)
               (ein:join-str " " (mapcar #'file-name-nondirectory it))))
    (message "Compiled %s files" (length files))))


(provide 'ein-core)

;;; ein-core.el ends here
