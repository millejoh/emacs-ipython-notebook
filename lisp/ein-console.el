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


(require 'ein-core)

;; Functions from `Fabian Gallina's python.el`_
;; NOTE: Do *not* load python.el here, since user may be using the other
;;       version of python-mode.
(declare-function python-shell-make-comint "python")
(declare-function python-shell-get-process-name "python")
(declare-function python-shell-switch-to-shell "python")



;;; Define aliases to old variables and functions.

(define-obsolete-variable-alias
  'ein:notebook-console-security-dir 'ein:console-security-dir "0.1.2")
(define-obsolete-variable-alias
  'ein:notebook-console-executable 'ein:console-executable "0.1.2")
(define-obsolete-variable-alias
  'ein:notebook-console-args 'ein:console-args "0.1.2")
(define-obsolete-function-alias
  'ein:notebook-console-open 'ein:console-open "0.1.2")


;;; Configuration

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

(defcustom ein:console-executable (executable-find "jupyter")
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

(defcustom ein:console-args '()
  "Additional argument when using console.

.. warning:: Space-separated string is obsolete now.  Use a list
   of string as value now.

Setting to use IPython profile named \"YOUR-IPYTHON-PROFILE\"::

    (setq ein:console-args '(\"--profile\" \"YOUR-IPYTHON-PROFILE\"))

Together with `ein:console-security-dir', you can open IPython
console connecting to a remote kernel.::

    (setq ein:console-args '(\"--ssh\" \"HOSTNAME\"))
    (setq ein:console-security-dir \"PATH/TO/SECURITY/DIR\")

You can setup `ein:console-args' per server basis using alist form::

    (setq ein:console-args
          '((8888 . '(\"--profile\" \"PROFILE\"))
            (8889 . '(\"--ssh\" \"HOSTNAME\"))
            (default . '(\"--profile\" \"default\"))))

If you want to use more complex setting, you can set a function to it::

    (setq ein:console-args
          (lambda (url-or-port) '(\"--ssh\" \"HOSTNAME\")))

See also: `ein:console-security-dir'."
  :type '(choice
          (repeat (string :tag "Arguments to IPython" "--profile"))
          (alist :tag "Arguments mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type
                 (repeat (string :tag "Arguments to IPython" "--profile")))
          (function :tag "Additional arguments getter"
                    (lambda (url-or-port)
                      (list "--ssh" (format "%s" url-or-port)))))
  :group 'ein)

(defun ein:console-security-dir-get (url-or-port)
  (let ((dir (ein:choose-setting 'ein:console-security-dir url-or-port)))
    (if (equal dir "")
        dir
    (file-name-as-directory (expand-file-name dir)))))

(defun ein:console-executable-get (url-or-port)
  (ein:choose-setting 'ein:console-executable url-or-port))

(defun ein:console-args-get (url-or-port)
  (ein:choose-setting 'ein:console-args url-or-port
                      (lambda (x)
                        (or (stringp x)
                            (and (listp x)
                                 (stringp (car x)))))))

(defun ein:console-make-command ()
  ;; FIXME: use %connect_info to get connection file, then I can get
  ;; rid of `ein:console-security-dir'.
  (let* ((url-or-port (or (ein:get-url-or-port)
                          (error "Cannot find notebook to connect!")))
         (dir (ein:console-security-dir-get url-or-port))
         (kid (ein:kernel-id (ein:get-kernel)))
         (ipy (ein:console-executable-get url-or-port))
         (args (ein:console-args-get url-or-port)))
    ;; FIMXE: do I need "python" here?
    (append (list ipy "console" "--existing"
                  (format "%skernel-%s.json" dir kid))
            (if (listp args)
                args
              (ein:display-warning-once
               "String value for `ein:console-args' is obsolete.
Use list of string instead of space separated string.")
              (split-string-and-unquote args)))))

(defun ein:console-get-buffer ()
  (let ((url-or-port (ein:get-url-or-port))
        (notebook (ein:get-notebook)))
    (format "*ein:console %s/%s*" url-or-port (ein:notebook-name notebook))))

;;;###autoload
(defun ein:console-open ()
  "Open IPython console.
To use this function, `ein:console-security-dir' and
`ein:console-args' must be set properly.
This function works best with the new python.el_ which is shipped
with Emacs 24.2 or later.  If you don't have it, this function
opens a \"plain\" command line interpreter (comint) buffer where
you cannot use fancy stuff such as TAB completion.
It should be possible to support python-mode.el.  Patches are welcome!

.. _python.el: https://github.com/fgallina/python.el"
  (interactive)
  ;; FIXME: Workaround for running current version of Jupyter console on windows
  (when (eql system-type 'windows-nt)
    (if (string-match-p "jupyter" (ein:console-executable-get (or (ein:get-url-or-port)
                                                                  (error "Cannot find notebook to connect!"))))
        (setenv "JUPYTER_CONSOLE_TEST" "1")
      (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))
  (if (fboundp 'python-shell-switch-to-shell)
      (let ((cmd (mapconcat #'shell-quote-argument
                            (ein:console-make-command) " "))
            ;; python.el settings:
            (python-shell-setup-codes nil)
            ;; python.el makes dedicated process when
            ;; `buffer-file-name' has some value.
            (buffer-file-name (buffer-name)))
        ;; The following line does what `run-python' does.
        ;; But as `run-python' changed the call signature in the new
        ;; version, let's do this manually.
        ;; See also: https://github.com/tkf/emacs-ipython-notebook/pull/50
        (run-python cmd)
                                        ;(python-shell-make-comint cmd (python-shell-get-process-name t))
        ;; Pop to inferior Python process buffer
        (python-shell-switch-to-shell))
    (let* ((command (ein:console-make-command))
           (program (car command))
           (args (cdr command))
           (buffer (ein:console-get-buffer)))
      (apply #'make-comint-in-buffer
             "ein:console" buffer program nil args)
      (pop-to-buffer buffer))))

(provide 'ein-console)

;;; ein-console.el ends here
