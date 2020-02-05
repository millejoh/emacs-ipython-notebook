;;; ein-query.el --- jQuery like interface on to of url-retrieve -*- lexical-binding: t -*-

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

(defvar ein:query-xsrf-cache (make-hash-table :test 'equal)
  "Hack: remember the last xsrf token by host in case we catch cookie jar in transition.  The proper fix is to sempahore between competing curl processes.")

(defun ein:query-prepare-header (url settings &optional securep)
  "Ensure that REST calls to the jupyter server have the correct _xsrf argument."
  (let* ((host (url-host (url-generic-parse-url url)))
         (cookies (request-cookie-alist host "/" securep))
         (xsrf (or (cdr (assoc-string "_xsrf" cookies))
                   (gethash host ein:query-xsrf-cache))))
    (ein:log 'debug "EIN:QUERY-PREPARE-HEADER: Found xsrf: %s" xsrf)
    (setq settings (plist-put settings :headers
                              (append (plist-get settings :headers)
                                      (list (cons "User-Agent" "Mozilla/5.0")))))
    (when xsrf
      (setq settings (plist-put settings :headers
                                (append (plist-get settings :headers)
                                        (list (cons "X-XSRFTOKEN" xsrf)))))
      (setf (gethash host ein:query-xsrf-cache) xsrf))
    (setq settings (plist-put settings :encoding 'binary))
    settings))

(let ((checked-curl-version nil))
  (defun ein:warn-on-curl-version ()
    (let ((curl (executable-find request-curl)))
      (unless checked-curl-version
        (setq checked-curl-version t)
        (with-temp-buffer
          (call-process curl nil t nil "--version")
          (goto-char (point-min))
          (when (search-forward "mingw32" nil t)
            (warn "The current version of curl (%s) may not work with ein. We recommend you install the latest, official version from the curl website: https://curl.haxx.se" (buffer-string))))))))

(defsubst ein:query-enforce-curl ()
  (ein:warn-on-curl-version)
  (unless (eq request-backend 'curl)
    (ein:display-warning
     (format "request-backend: %s unsupported" request-backend))
    (if (executable-find request-curl)
        (setq request-backend 'curl)
      (ein:display-warning
       (format "The %s program was not found" request-curl) :error))))

(cl-defun ein:query-singleton-ajax (url &rest settings
                                        &key (timeout ein:query-timeout)
                                        &allow-other-keys)
  (ein:query-enforce-curl)
  (when timeout
    (setq settings (plist-put settings :timeout (/ timeout 1000.0))))
  (setq settings (plist-put settings :sync ein:force-sync))
  (apply #'request (url-encode-url url) (ein:query-prepare-header url settings)))

;;; Cookie

(defalias 'ein:query-get-cookie 'request-cookie-string)

(provide 'ein-query)

;;; ein-query.el ends here
