;;; ein-console.el --- IPython console integration

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-console.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-console.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-console.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'ein)
(require 'ein-utils)

;; Functions from `Fabian Gallina's python.el`_
;; NOTE: Do *not* load python.el here, since user may be using the other
;;       version of python-mode.
(declare-function python-shell-make-comint "python")
(declare-function python-shell-get-process-name "python")
(declare-function python-shell-switch-to-shell "python")


(defcustom ein:console-security-dir ""
  "Security directory setting.

Following types are valid:

string
    Use this value as a path to security directory.
    Handy when you have only one IPython server.
alist
    An alist whose element is \"(URL-OR-PORT . DIR)\".
    Key (URL-OR-PORT) can be string (URL), integer (port), or
    `default' (symbol).  The value of `default' is used when
    other key does not much.  Normally you should have this
    entry.
function
    Called with an argument URL-OR-PORT (integer or string).
    You can have complex setting using this."
  :type '(choice
          (string :tag "Security directory"
                  "~/.config/ipython/profile_nbserver/security/")
          (alist :tag "Security directory mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "Security directory"))
          (function :tag "Security directory getter"
                    (lambda (url-or-port)
                      (format "~/.config/ipython/profile_%s/security/"
                              url-or-port))))
  :group 'ein)

(defcustom ein:console-executable (executable-find "ipython")
  "IPython executable used for console.

Example: ``\"/user/bin/ipython\"``.
Types same as `ein:console-security-dir' are valid."
  :type '(choice
          (string :tag "IPython executable" "/user/bin/ipython")
          (alist :tag "IPython executable mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "IPython executable"
                                     "/user/bin/ipython"))
          (function :tag "IPython executable getter"
                    (lambda (url-or-port) (executable-find "ipython"))))
  :group 'ein)

(defcustom ein:console-args "--profile nbserver"
  "Additional argument when using console.

Example: ``\"--ssh HOSTNAME\"``.
Types same as `ein:console-security-dir' are valid."
  :type '(choice
          (string :tag "Arguments to IPython"
                  "--profile nbserver --ssh HOSTNAME")
          (alist :tag "Arguments mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "Arguments to IPython"
                                     "--profile nbserver --ssh HOSTNAME"))
          (function :tag "Additional arguments getter"
                    (lambda (url-or-port)
                      (format "--ssh %s" url-or-port))))
  :group 'ein)

(defun ein:console-security-dir-get (url-or-port)
  (let ((dir (ein:choose-setting 'ein:console-security-dir url-or-port)))
    (if (equal dir "")
        dir
    (file-name-as-directory (expand-file-name dir)))))

(defun ein:console-executable-get (url-or-port)
  (ein:choose-setting 'ein:console-executable url-or-port))

(defun ein:console-args-get (url-or-port)
  (ein:choose-setting 'ein:console-args url-or-port))

(defun ein:console-make-command ()
  (let* ((url-or-port (or (ein:get-url-or-port)
                          (error "Cannot find notebook to connect!")))
         (dir (ein:console-security-dir-get url-or-port))
         (kid (ein:kernel-id (ein:get-kernel)))
         (ipy (ein:console-executable-get url-or-port))
         (args (ein:console-args-get url-or-port)))
    (format "python %s console --existing %skernel-%s.json %s"
            ipy dir kid args)))

(defun ein:console-open ()
  "Open IPython console.
To use this function, `ein:console-security-dir' and
`ein:console-args' must be set properly.
This function requires `Fabian Gallina's python.el`_ for now;
It should be possible to support python-mode.el.  Patches are welcome!

.. _`Fabian Gallina's python.el`: https://github.com/fgallina/python.el"
  ;; FIXME: use %connect_info to get connection file, then I can get
  ;; rid of `ein:console-security-dir'.
  (interactive)
  (if (fboundp 'python-shell-switch-to-shell)
      (let ((cmd (ein:console-make-command))
            ;; python.el settings:
            (python-shell-setup-codes nil)
            ;; python.el makes dedicated process when
            ;; `buffer-file-name' has some value.
            (buffer-file-name (buffer-name)))
        ;; The following line does what `run-python' does.
        ;; But as `run-python' changed the call signature in the new
        ;; version, let's do this manually.
        ;; See also: https://github.com/tkf/emacs-ipython-notebook/pull/50
        (python-shell-make-comint cmd (python-shell-get-process-name t))
        ;; Pop to inferior Python process buffer
        (python-shell-switch-to-shell))
    (error "python.el is not loaded!")))


;;; Define aliases to old variables and functions.

(define-obsolete-variable-alias
  'ein:notebook-console-security-dir 'ein:console-security-dir "0.1.2")
(define-obsolete-variable-alias
  'ein:notebook-console-executable 'ein:console-executable "0.1.2")
(define-obsolete-variable-alias
  'ein:notebook-console-args 'ein:console-args "0.1.2")
(define-obsolete-function-alias
  'ein:notebook-console-open 'ein:console-open "0.1.2")

(provide 'ein-console)

;;; ein-console.el ends here

