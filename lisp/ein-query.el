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
(require 'url)

(require 'ein-core)
(require 'ein-log)


;;; Utils

(defun ein:safe-funcall-packed (packed &rest args)
  (when packed
    (ein:log-ignore-errors (apply #'ein:funcall-packed packed args))))


;;; Variables

(defcustom ein:query-timeout 1000
  "Default query timeout for HTTP access in millisecond.

Setting this to `nil' means no timeout.

If you do the same operation before the timeout, old operation
will be canceled \(see also `ein:query-singleton-ajax').

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

;; FIXME: Passing around timer object using buffer local variable is
;;        not a good idea, as it looks like `url-retrieve' opens
;;        different buffer when following redirections (probably
;;        another bug...).

(ein:deflocal ein:%query-ajax-timer% nil)

(ein:deflocal ein:%query-ajax-canceled% nil
  "Buffer local variable which is set to the reason for cancel (a symbol)
when it is cancelled.")


;;; Functions

(defun* ein:query-default-error-callback (url &key symbol-status
                                              &allow-other-keys)
  (ein:log 'error
    "Error (%s) while connecting to %s.  Please retry."
    symbol-status url))

(defun ein:query-get-default-error-callback (url)
  (cons #'ein:query-default-error-callback url))

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
  (unless error
    (setq error (ein:query-get-default-error-callback url))
    (setq settings (plist-put settings :error error)))
  (when (and (equal type "POST") data)
    (push '("Content-Type" . "application/x-www-form-urlencoded") headers)
    (setq settings (plist-put settings :headers headers)))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'ein:query-ajax-callback settings)))
    (ein:log 'debug "Start querying: %s" url)
    (unless timeout (setq timeout ein:query-timeout))
    (when timeout
      (ein:log 'debug "Start timer: timeout=%s ms" timeout)
      (with-current-buffer buffer
        (setq ein:%query-ajax-timer%
              (apply #'run-at-time
                     (/ timeout 1000.0) nil
                     #'ein:query-ajax-timeout-callback
                     (cons buffer settings)))))
    (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
    buffer))

(defun ein:query-ajax--parse-data (parser status-error)
  "Run PARSER in current buffer if STATUS-ERROR is nil,
then kill the current buffer."
  (let ((buffer (current-buffer)) ; NOTE: `parser' could change buffer...
        noerror)
    (unwind-protect
        (prog1
            (when (and parser (not status-error))
              (funcall parser))
          (setq noerror t))
      (unless noerror
        (ein:log 'error "QUERY-AJAX--PARSE-DATA: error from parser %S"
                 parser))
      (kill-buffer buffer))))

(defun* ein:query-ajax-callback (status &key
                                        (headers nil)
                                        (parser nil)
                                        (success nil)
                                        (error nil)
                                        (timeout nil)
                                        (status-code nil)
                                        &allow-other-keys)
  (declare (special url-http-method
                    url-http-response-status))

  (ein:log 'debug "EIN:QUERY-AJAX-CALLBACK")
  (ein:log 'debug "status = %S" status)
  (ein:log 'debug "url-http-method = %s" url-http-method)
  (ein:log 'debug "url-http-response-status = %s" url-http-response-status)
  (ein:log 'debug "(buffer-string) =\n%s" (buffer-string))

  (ein:query-ajax-cancel-timer)
  (let* ((response-status url-http-response-status)
         (status-code-callback (cdr (assq response-status status-code)))
         (status-error (plist-get status :error))
         (canceled ein:%query-ajax-canceled%)
         (data (ein:query-ajax--parse-data parser status-error)))
    (ein:log 'debug "data = %s" data)
    (ein:log 'debug "canceled = %s" canceled)

    (ein:log 'debug "Executing success/error callback.")
    (apply #'ein:safe-funcall-packed
           (append (if (or (plist-get status :error) canceled)
                       (list error :symbol-status
                             (or canceled 'error))
                     (list success))
                   (list :status status :data data
                         :response-status response-status)))

    (unless canceled
      (ein:log 'debug "Executing status-code callback.")
      (ein:safe-funcall-packed status-code-callback
                               :status status :data data))))

(defun* ein:query-ajax-timeout-callback (buffer &key
                                                error parser
                                                &allow-other-keys)
  (ein:log 'debug "EIN:QUERY-AJAX-TIMEOUT-CALLBACK buffer = %S" buffer)
  (ein:with-live-buffer buffer
    (setq ein:%query-ajax-canceled% 'timeout)
    (let ((proc (get-buffer-process buffer)))
      (ein:log 'debug "EIN:QUERY-AJAX-TIMEOUT-CALLBACK proc = %S" proc)
      (if proc
          ;; This will call `ein:query-ajax-callback'.
          (delete-process proc)
        ;; No associated process.  This means that `url-retrieve' failed
        ;; to call callback function.  This happens sometimes.
        ;; Let's call the error callback manually.
        (destructuring-bind (&key code &allow-other-keys)
            (progn
              (goto-char (point-min))
              (ein:query--parse-response-at-point))
          (ein:log 'debug "(buffer-string) =\n%s" (buffer-string))
          ;; FIXME: error callback may be called already in
          ;;        `ein:query-ajax-callback'.  This happens when
          ;;        `ein:query-ajax-callback' is called in
          ;;        differnt buffer.
          (ein:safe-funcall-packed
           error
           ;; Passing data to error callback makes no sense, but it is
           ;; needed for implementing `ein:notebooklist-login--error'.
           ;; Also, this kills the buffer.
           :data (ein:query-ajax--parse-data parser nil)
           :symbol-status 'timeout :response-status code))))))

(defun ein:query--parse-response-at-point ()
  (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)")
  (list :version (match-string 1)
        :code (string-to-number (match-string 2))))

(defun ein:query-ajax-cancel-timer ()
  (ein:log 'debug "EIN:QUERY-AJAX-CANCEL-TIMER")
  (when ein:%query-ajax-timer%
    (cancel-timer ein:%query-ajax-timer%)
    (setq ein:%query-ajax-timer% nil)))

(defvar ein:query-running-process-table (make-hash-table :test 'equal))

(defun ein:query-singleton-ajax (key &rest args)
  "Cancel the old process if there is a process associated with
KEY, then call `ein:query-ajax' with ARGS.  KEY is compared by
`equal'."
  (ein:query-gc-running-process-table)
  (ein:aif (gethash key ein:query-running-process-table)
      (ein:with-live-buffer it
        (setq ein:%query-ajax-canceled% 'user-cancel)
        (let ((proc (get-buffer-process it)))
          ;; This will call `ein:query-ajax-callback'.
          (delete-process proc))))
  (let ((buffer (apply #'ein:query-ajax args)))
    (puthash key buffer ein:query-running-process-table)
    buffer))

(defun ein:query-gc-running-process-table ()
  "Garbage collect dead processes in `ein:query-running-process-table'."
  (maphash
   (lambda (key buffer)
     (unless (buffer-live-p buffer)
       (remhash key ein:query-running-process-table)))
   ein:query-running-process-table))


;;; Cookie

(defun ein:query-get-cookie (host &optional localpart secure)
  "Return cookie string (like `document.cookie').

Example::

   (ein:query-get-cookie \"127.0.0.1\" \"/\")
"
  (let ((cookies (mapcar
                  (lambda (c) (cons (url-cookie-name c) (url-cookie-value c)))
                  (url-cookie-retrieve host localpart secure))))
    (mapconcat
     (lambda (nv) (concat (car nv) "=" (cdr nv)))
     cookies
     "; ")))

(provide 'ein-query)

;;; ein-query.el ends here
