;;; ein-query.el --- jQuery like interface on to of url-retrieve

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

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
(require 'request)
(require 'url)

(require 'ein-core)
(require 'ein-log)


;;; Utils

(defun ein:safe-funcall-packed (packed &rest args)
  (when packed
    (ein:log-ignore-errors (apply #'ein:funcall-packed packed args))))


;;; Variables

(defcustom ein:query-timeout
  (if (eq request-backend 'url-retrieve) 1000 nil)
  "Default query timeout for HTTP access in millisecond.

Setting this to `nil' means no timeout.
If you have ``curl`` command line program, it is automatically set to
`nil' as ``curl`` is reliable than `url-retrieve' therefore no need for
a workaround (see below).

If you do the same operation before the timeout, old operation
will NO LONGER be canceled (as it the cookie jar gets clobbered when curl
aborts).  Instead you will see Race! in debug messages.

.. note:: This value exists because it looks like `url-retrieve'
   occasionally fails to finish \(start?) querying.  Timeout is
   used to let user notice that their operation is not finished.
   It also prevent opening a lot of useless process buffers.
   You will see them when closing Emacs if there is no timeout.

   If you know how to fix the problem with `url-retrieve', please
   let me know or send pull request at github!
   \(Related bug report in Emacs bug tracker:
   http://debbugs.gnu.org/cgi/bugreport.cgi?bug=11469)"
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "No timeout" nil))
  :group 'ein)


;;; Functions

(defvar ein:query-running-process-table (make-hash-table :test 'equal))

(defvar ein:query-xsrf-cache (make-hash-table :test 'equal)
  "Hack: remember the last xsrf token by host in case we catch cookie jar in transition.  The proper fix is to sempahore between competing curl processes.")

(defun ein:query-prepare-header (url settings &optional securep)
  "Ensure that REST calls to the jupyter server have the correct _xsrf argument."
  (let* ((host (url-host (url-generic-parse-url url)))
         (cookies (request-cookie-alist host "/" securep))
         (xsrf (or (cdr (assoc-string "_xsrf" cookies))
                   (gethash host ein:query-xsrf-cache))))
    (when xsrf
      (setq settings (plist-put settings :headers
                                (append (plist-get settings :headers)
                                        (list (cons "X-XSRFTOKEN" xsrf)))))
      (setf (gethash host ein:query-xsrf-cache) xsrf))
    settings))

(defcustom ein:max-simultaneous-queries 100
  "Limit number of simultaneous queries to Jupyter server.

If too many calls to `request' are made at once Emacs may
complaint and raise a 'Too Many Files' exception. By setting this
variable to a reasonable value you can avoid this situation."
  :group 'ein
  :type 'integer)

(defsubst ein:query-enforce-curl ()
  (when (not (eq request-backend 'curl))
    (ein:display-warning 
     (format "request-backend: %s unsupported" request-backend))
    (if (executable-find "curl")
        (setq request-backend 'curl)
      (ein:display-warning "The 'curl' program was not found"))))

(defun* ein:query-singleton-ajax (key url &rest settings
                                      &key
                                      (timeout ein:query-timeout)
                                      &allow-other-keys)
  "Cancel the old process if there is a process associated with
KEY, then call `request' with URL and SETTINGS.  KEY is compared by
`equal'."
  (ein:query-enforce-curl)
  (with-local-quit
    (when timeout
      (setq settings (plist-put settings :timeout (/ timeout 1000.0))))
    (loop do (ein:query-gc-running-process-table)
          for running = (hash-table-count ein:query-running-process-table)
          until (< running ein:max-simultaneous-queries)
          do (ein:log 'warn "ein:query-singleton-ajax: %d running processes"
                      running)
          do (sleep-for 3))
    (ein:aif (gethash key ein:query-running-process-table)
        (unless (request-response-done-p it)
          (ein:log 'debug "Race! %s %s" key (request-response-data it))))
    (let ((response (apply #'request (url-encode-url url)
                           (ein:query-prepare-header url settings))))
      (puthash key response ein:query-running-process-table)
      response)))

(defun ein:query-gc-running-process-table ()
  "Garbage collect dead processes in `ein:query-running-process-table'."
  (maphash
   (lambda (key buffer)
     (when (request-response-done-p buffer)
       (remhash key ein:query-running-process-table)))
   ein:query-running-process-table))

(defun ein:get-response-redirect (response)
  "Determine if the query has been redirected, and if so return then URL the request was redirected to."
  (if (length (request-response-history response))
      (let ((url (url-generic-parse-url (format "%s" (request-response-url response)))))
        (format "%s://%s:%s"
                (url-type url)
                (url-host url)
                (url-port url)))))


;;; Cookie

(defalias 'ein:query-get-cookie 'request-cookie-string)

(provide 'ein-query)

;;; ein-query.el ends here
