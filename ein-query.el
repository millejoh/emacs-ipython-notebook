;;; ein-query.el --- jQuery like interface on to of url-retrieve

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-query.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-query.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-query.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'url)

(require 'ein-utils)
(require 'ein-log)


;;; Utils

(defun ein:safe-funcall-packed (packed &rest args)
  (when packed
    (ein:log-ignore-errors (apply #'ein:funcall-packed packed args))))

(defmacro ein:with-live-buffer (buffer &rest body)
  "Execute BODY if BUFFER is alive."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p ,buffer) (with-current-buffer ,buffer ,@body)))


;;; Variables

(defcustom ein:query-timeout 5000
  "Default query timeout."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "No timeout" nil))
  :group 'ein)

(ein:deflocal ein:query-ajax-timer nil)


;;; Functions

(defun* ein:query-ajax (url &rest settings
                            &key
                            (cache t)
                            (type "GET")
                            (data nil)
                            (parser nil)
                            (headers nil)
                            (success nil)
                            (error nil)
                            (timeout nil)
                            (status-code nil))
  "Mimic `$.ajax'.

:CACHE       (nil/t) : append time-stamp to URL so the URL is always loaded.
:TYPE       (string) : sets `url-request-method'
:DATA       (string) : sets `url-request-data'
:PARSER     (symbol) : a function that reads current buffer and return data
:HEADERS     (alist) : sets `url-request-extra-headers'
:SUCCESS      (cons) : called on success
:ERROR        (cons) : called on error
:TIMEOUT    (number) : timeout in millisecond
:STATUS-CODE (alist) : map status code (int) to callback (cons)

* Callback functions

All callbacks must be given as `cons' where car is a FUNCTION and
cdr is its first ARGUMENT.  It is analogous of `$.proxy'.  Call
signature is like this:
    \(FUNCTION ARGUMENT [other callback specific arguments])

Also note that the callback FUNCTION must be defined
using `defun*' with `&key' and `&allow-other-keys' to ignore
missing/extra arguments as some callback (namely :ERROR) changes
arguments to be passed, depending on situation.

* :ERROR callback

:SYMBOL-STATUS (`error'/`timeout') : analogous of `textStatus'
:STATUS                     (list) : see `url-retrieve'
:RESPONSE-STATUS                   : = `url-http-response-status'

* :SUCCESS callback

This callback takes :DATA (object), which is a data object parsed
by :PARSER.  If :PARSER is not specified, this is nil.
The :SUCCESS callback also takes the :STATUS and :RESPONSE-STATUS
argument.

* :STATUS-CODE callback

Each value of this alist is a callback which is similar to :ERROR
or :SUCCESS callback.  However, current buffer of this callback
is not guaranteed to be the process buffer.

* :PARSER function

This is analogous to the `dataType' argument of `$.ajax'.
Only this function can accuses to the process buffer, which
is killed immediately after the execution of this function.

* See also: http://api.jquery.com/jQuery.ajax/"
  (ein:log 'debug "EIN:QUERY-AJAX")
  (unless cache
    (setq url (ein:url-no-cache url)))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'ein:query-ajax-callback settings)))
    (unless timeout (setq timeout ein:query-timeout))
    (when timeout
      (ein:log 'debug "Start timer: timeout=%s ms" timeout)
      (with-current-buffer buffer
        (setq ein:query-ajax-timer
              (apply #'run-at-time
                     (/ timeout 1000.0) nil
                     #'ein:query-ajax-timeout-callback
                     (cons buffer settings)))))
    buffer))

(defun* ein:query-ajax-callback (status &key
                                        (headers nil)
                                        (parser nil)
                                        (success nil)
                                        (error nil)
                                        (timeout nil)
                                        (status-code nil)
                                        &allow-other-keys)
  (declare (special url-http-response-status))

  (ein:log 'debug "EIN:QUERY-AJAX-CALLBACK")
  (ein:log 'debug "status = %S" status)
  (ein:log 'debug "url-http-response-status = %s" url-http-response-status)
  (ein:log 'debug "(buffer-string) =\n%s" (buffer-string))

  (ein:query-ajax-cancel-timer)
  (let* ((buffer (current-buffer)) ; `parser' could change buffer...
         (response-status url-http-response-status)
         (status-code-callback (cdr (assq response-status status-code)))
         (status-error (plist-get status :error))
         (data (if (and parser (not status-error))
                   (unwind-protect
                       (funcall parser)
                     (kill-buffer buffer)))))
    (ein:log 'debug "data = %s" data)

    (ein:log 'debug "Executing success/error callback.")
    (apply #'ein:safe-funcall-packed
           (append (if (plist-get status :error)
                       (list error :symbol-status 'error)
                     (list success))
                   (list :status status :data data
                         :response-status response-status)))

    (ein:log 'debug "Executing status-code callback.")
    (ein:safe-funcall-packed status-code-callback
                             :status status :data data)))

(defun* ein:query-ajax-timeout-callback (buffer &key
                                                (error nil)
                                                &allow-other-keys)
  (ein:log 'debug "EIN:QUERY-AJAX-TIMEOUT-CALLBACK buffer = %s" buffer)
  (ein:with-live-buffer buffer
    (ein:safe-funcall-packed error :symbol-status 'timeout)
    (let ((proc (get-buffer-process buffer)))
      (delete-process proc)
      (kill-buffer buffer))))

(defun ein:query-ajax-cancel-timer ()
  (ein:log 'debug "EIN:QUERY-AJAX-CANCEL-TIMER")
  (when ein:query-ajax-timer
    (cancel-timer ein:query-ajax-timer)
    (setq ein:query-ajax-timer nil)))

(provide 'ein-query)

;;; ein-query.el ends here
