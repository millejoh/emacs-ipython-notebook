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

(defun ein:safe-funcall-packed (packed &rest args)
  (when packed
    (ein:log-ignore-errors (apply #'ein:funcall-packed packed args))))

(ein:deflocal ein:query-ajax-timer nil)

(defun* ein:query-ajax (url &rest settings
                            &key
                            (cache t)
                            (type "GET")
                            (data nil)
                            (headers nil)
                            (success nil)
                            (error nil)
                            (timeout nil)
                            (status-code nil))
  "Mimic `$.ajax'.

:CACHE       (nil/t) : append time-stamp to URL so the URL is always loaded.
:TYPE       (string) : sets `url-request-method'
:DATA       (string) : sets `url-request-data'
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

The second note is that the callback FUNCTION must be defined
using `defun*' with `&key' and `&allow-other-keys' to ignore
missing/extra arguments as some callback (namely :ERROR) changes
arguments to be passed, depending on situation.

Finally, note that callback FUNCTION is executed on the process
buffer, meaning that response data is on the current buffer when
the FUNCTION is called.  There is no `:data' argument for
FUNCTION so it must be fetched from the buffer.

* :ERROR callback

:SYMBOL-STATUS (`error'/`timeout') : analogous of `textStatus'
:STATUS                     (list) : see `url-retrieve'

The :SUCCESS callback also takes the :STATUS argument.

* See also: http://api.jquery.com/jQuery.ajax/"
  (ein:log 'debug "EIN:QUERY-AJAX")
  (unless cache
    (setq url (ein:url-no-cache url)))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'ein:query-ajax-callback settings)))
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
                                        (success nil)
                                        (error nil)
                                        (timeout nil)
                                        (status-code nil)
                                        &allow-other-keys)
  (unwind-protect
      (progn
        (ein:log 'debug "EIN:QUERY-AJAX-CALLBACK")
        (ein:log 'debug "status = %S" status)
        (ein:log 'debug "url-http-response-status = %s"
                 (ein:eval-if-bound 'url-http-response-status))
        (ein:log 'debug "(buffer-string) =\n%s" (buffer-string))

        (apply #'ein:safe-funcall-packed
               (if (plist-get status :error)
                   (list error :symbol-status 'error :status status)
                 (list success :status status)))
        (ein:aif (assq (ein:eval-if-bound 'url-http-response-status)
                       status-code)
            (ein:safe-funcall-packed (cdr it))))
    (ein:query-ajax-cancel-timer)
    (kill-buffer)))

(defun* ein:query-ajax-timeout-callback (buffer &key
                                                (error nil)
                                                &allow-other-keys)
  (ein:log 'debug "EIN:QUERY-AJAX-TIMEOUT-CALLBACK buffer = %s" buffer)
  (with-current-buffer buffer
    (ein:safe-funcall-packed error :symbol-status 'timeout))
  (let ((proc (process-buffer buffer)))
    (kill-process proc)
    (kill-buffer buffer)))

(defun ein:query-ajax-cancel-timer ()
  (ein:log 'debug "EIN:QUERY-AJAX-CANCEL-TIMER")
  (when ein:query-ajax-timer
    (cancel-timer ein:query-ajax-timer)
    (setq ein:query-ajax-timer nil)))

(provide 'ein-query)

;;; ein-query.el ends here
