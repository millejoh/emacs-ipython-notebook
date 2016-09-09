;;; request-testing.el --- Testing framework for request.el -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; request-testing.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; request-testing.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request-testing.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'request-deferred)


;; Compatibility

(defun request-testing-string-prefix-p (prefix str &optional ignore-case)
  (let ((case-fold-search ignore-case))
    (string-match-p (format "^%s" (regexp-quote prefix)) str)))

(unless (fboundp 'string-prefix-p)      ; not defined in Emacs 23.1
  (fset 'string-prefix-p (symbol-function 'request-testing-string-prefix-p)))


;;;

(defvar request-testing-source-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar request-testing-timeout 3000)

(defmacro request-testing-with-response-slots (response &rest body)
  "Destructure RESPONSE object and execute BODY.
Following symbols are bound:

  response / status-code / history / data / error-thrown /
  symbol-status / url / done-p / settings / -buffer / -timer

The symbols other than `response' is bound using `cl-symbol-macrolet'."
  (declare (indent 1))
  `(let ((response ,response))
     (cl-symbol-macrolet
         ,(cl-loop for slot in '(status-code
                                 history
                                 data
                                 error-thrown
                                 symbol-status
                                 url
                                 done-p
                                 settings
                                 -buffer
                                 -timer)
                   for accessor = (intern (format "request-response-%s" slot))
                   collect `(,slot (,accessor response)))
       ,@body)))

(defvar request-testing-server--process nil)
(defvar request-testing-server--port nil)

(defun request-testing--wait-process-until (process output-regexp)
  "Wait until PROCESS outputs text which matches to OUTPUT-REGEXP."
  (cl-loop with buffer = (process-buffer process)
           repeat 30
           do (accept-process-output process 0.1 nil t)
           for str = (with-current-buffer buffer (buffer-string))
           do (cond
               ((string-match output-regexp str)
                (return str))
               ((not (eq 'run (process-status process)))
                (error "Server startup error.")))
           finally do (error "Server timeout error.")))

(defun request-testing-server ()
  "Get running test server and return its root URL."
  (interactive)
  (unless request-testing-server--port
    (let ((process (start-process "request-testing" " *request-testing*"
                                  "python"
                                  (expand-file-name
                                   "testserver.py"
                                   request-testing-source-dir))))
      (setq request-testing-server--process process)
      (setq request-testing-server--port
            (string-to-number
             (request-testing--wait-process-until process "^[0-9]+$")))
      (request-testing--wait-process-until process "Running on")))
  (request-testing-url))

(defun request-testing-stop-server ()
  (interactive)
  (let ((process request-testing-server--process))
    (if (and (processp process) (request--process-live-p process))
        (quit-process process)
      (unless noninteractive
        (message "No server is running!"))))
  (setq request-testing-server--port nil)
  (setq request-testing-server--process nil))
(add-hook 'kill-emacs-hook 'request-testing-stop-server)

(defun request-testing-url (&rest path)
  (cl-loop with url = (format "http://127.0.0.1:%s" request-testing-server--port)
           for p in path
           do (setq url (concat url "/" p))
           finally return url))

(defun request-testing-async (url &rest args)
  (apply #'request (request-testing-url url) args))

(defun request-testing-sync (url &rest args)
  (let (err timeout)
    (let ((result
           (deferred:sync!
             (deferred:timeout
               request-testing-timeout
               (setq timeout t)
               (deferred:try
                 (apply #'request-deferred (request-testing-url url) args)
                 :catch
                 (lambda (x) (setq err x)))))))
      (if timeout
          (error "Timeout.")
        (or result err)))))

(defun request-testing-sort-alist (alist)
  (sort alist (lambda (x y)
                (setq x (symbol-name (car x))
                      y (symbol-name (car y)))
                (string-lessp x y))))

(defun request-deftest--url-retrieve-isolate (body)
  "[Macro helper] Isolate execution of BODY from normal environment."
  `((let (url-cookie-storage
          url-cookie-secure-storage
          url-cookie-file
          url-cookies-changed-since-last-save)
      ,@body)))

(defun request-deftest--tempfiles (tempfiles body)
  "[Macro helper] Execute BODY with TEMPFILES and then remove them."
  (let ((symbols (cl-loop for f in tempfiles
                          collect (make-symbol (format "%s*" f)))))
    `((let ,(cl-loop for s in symbols
                     collect `(,s (make-temp-file "emacs-request-")))
        (let ,(cl-loop for f in tempfiles
                       for s in symbols
                       collect `(,f ,s))
          (unwind-protect
              (progn ,@body)
            ,@(cl-loop for s in symbols
                       collect `(ignore-errors (delete-file ,s)))))))))

(defun request-deftest--backends (backends name body)
  "[Macro helper] Execute BODY only when `request-backend' is in BACKENDS."
  `((if (and ',backends (not (memq request-backend ',backends)))
        (message "REQUEST: Skip %s for backend %s."
                 ',name request-backend)
      ,@body)))

(defvar request-testing-capture-message t
  "Set this to nil to suppress message capturing during test case
execution.  If it is non-nil, messages are not shown in the terminal
unless an error occurs.")

(defun request-deftest--capture-message (body)
  (let ((orig-message (make-symbol "orig-message"))
        (messages (make-symbol "messages"))
        (noerror (make-symbol "noerror")))
    `((if (and noninteractive request-testing-capture-message)
          (let ((,orig-message (symbol-function 'message))
                ,messages
                ,noerror)
            (unwind-protect
                (progn
                  (fset 'message (lambda (&rest args) (push args ,messages)))
                  ,@body
                  (setq ,noerror t))
              (fset 'message ,orig-message)
              (unless ,noerror
                (cl-loop for m in (nreverse ,messages)
                         do (apply #'message m)))))
        ,@body))))

(defmacro* request-deftest (name () &body docstring-and-body)
  "`ert-deftest' for test requiring test server.

Additional keyword arguments:

BACKENDS
  If non-nil, indicate backends that can pass this test.
  Backend not listed here may fail this test.

TEMPFILES
  A list of variables to be bound to paths of temporary files.
  The temporary files are cleaned automatically after the test.
"
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (let ((docstring (car docstring-and-body))
        (body (cdr docstring-and-body))
        ert-keys
        req-keys)

    ;; If docstring is not given...
    (unless (stringp docstring)
      (setq docstring nil)
      (setq body docstring-and-body))

    ;; Handle keywords
    (let (key val)
      (while (progn
               (setq key (car body))
               (and (symbolp key) (symbol-name key)))
        (setq val (cadr body))
        (if (memq key '(:backends :tempfiles))
            (progn
              (push key req-keys)
              (push val req-keys))
          (push key ert-keys)
          (push val ert-keys))
        (setq body (cddr body)))
      (setq ert-keys (nreverse ert-keys))
      (setq req-keys (nreverse req-keys)))

    ;; "Decorate" BODY.
    (setq body (request-deftest--capture-message body))
    (setq body (request-deftest--url-retrieve-isolate body))
    (cl-destructuring-bind (&key backends tempfiles) req-keys
      (setq body (request-deftest--tempfiles tempfiles body))
      (setq body (request-deftest--backends backends name body)))

    ;; Finally, define test.
    `(ert-deftest ,name ()
       ,@(when docstring (list docstring))
       ,@ert-keys
       (request-testing-server)
       ,@body)))

(provide 'request-testing)

;;; request-testing.el ends here
