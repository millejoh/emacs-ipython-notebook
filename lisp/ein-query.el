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
(require 'request-deferred)
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


;;; Jupyterhub
(defvar *ein:jupyterhub-servers* (make-hash-table :test #'equal))

(defstruct ein:$jh-conn
  "Data representing a connection to a jupyterhub server."
  url
  version
  user
  token)

(defstruct ein:$jh-user
  "A jupyterhub user, per https://jupyterhub.readthedocs.io/en/latest/_static/rest-api/index.html#/definitions/User."
  name
  admin
  groups
  server
  pending
  last-activity)



(defun ein:get-jh-conn (url)
  (gethash url *ein:jupyterhub-servers*))

(defun ein:reset-jh-servers ()
  (setq *ein:jupyterhub-servers* (make-hash-table :test #'equal)))

(defun ein:jupyterhub-url-p (url)
  "Does URL reference a jupyterhub server? If so then return the
connection structure representing the server."
  (let ((parsed (url-generic-parse-url url)))
    (or (gethash (format "http://%s:%s" (url-host parsed) (url-port parsed))
                 *ein:jupyterhub-servers*)
        (gethash (format "https://%s:%s" (url-host parsed) (url-port parsed))
                 *ein:jupyterhub-servers*))))

(defun ein:jupyterhub-correct-query-url-maybe (url-or-port)
  (let* ((parsed-url (url-generic-parse-url url-or-port))
         (hostport (format "http://%s:%s" (url-host parsed-url) (url-port parsed-url)))
         (command (url-filename parsed-url)))
    (ein:aif (ein:jupyterhub-url-p hostport)
        (let ((user-server-path (ein:$jh-user-server (ein:$jh-conn-user it))))
          (ein:url hostport
                   user-server-path
                   command))
      url-or-port)))

;;; Functions

(defvar ein:query-running-process-table (make-hash-table :test 'equal))

(defun ein:query-prepare-header (url settings &optional securep)
  "Ensure that REST calls to the jupyter server have the correct
_xsrf argument."
  (let* ((parsed-url (url-generic-parse-url url))
         (cookies (request-cookie-alist (url-host parsed-url)
                                       "/" securep)))
    (ein:aif (assoc-string "_xsrf" cookies)
        (setq settings (plist-put settings :headers (list (cons "X-XSRFTOKEN" (cdr it))))))
    (ein:aif (ein:jupyterhub-url-p (format "http://%s:%s" (url-host parsed-url) (url-port parsed-url)))
        (progn
          (unless (string-equal (ein:$jh-conn-url it)
                                (ein:url (ein:$jh-conn-url it) "hub/login"))
            (setq settings (plist-put settings :headers (append (plist-get settings :headers)
                                                                (list (cons "Referer"
                                                                            (ein:url (ein:$jh-conn-url it)
                                                                       "hub/login")))))))
          (when (ein:$jh-conn-token it)
            (setq settings (plist-put settings :headers (append (plist-get settings :headers)
                                                                (list (cons "Authorization"
                                                                            (format "token %s"
                                                                                    (ein:$jh-conn-token it))))))))))
    settings))

(defun* ein:query-singleton-ajax (key url &rest settings
                                      &key
                                      (timeout ein:query-timeout)
                                      &allow-other-keys)
  "Cancel the old process if there is a process associated with
KEY, then call `request' with URL and SETTINGS.  KEY is compared by
`equal'."
  (with-local-quit
    (ein:query-gc-running-process-table)
    (when timeout
      (setq settings (plist-put settings :timeout (/ timeout 1000.0))))
    (ein:aif (gethash key ein:query-running-process-table)
        (unless (request-response-done-p it)
          (request-abort it)))            ; This will run callbacks
    (let ((response (apply #'request (url-encode-url (ein:jupyterhub-correct-query-url-maybe url))
                           (ein:query-prepare-header url settings))))
      (puthash key response ein:query-running-process-table)
      response)))

(defun* ein:query-deferred (url &rest settings
                                &key
                                (timeout ein:query-timeout)
                                &allow-other-keys)
  ""
  (apply #'request-deferred (url-encode-url url)
         (ein:query-prepare-header url settings)))

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
