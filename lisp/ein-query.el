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


;;; Functions

(defvar ein:query-running-process-table (make-hash-table :test 'equal))

(defun* ein:query-singleton-ajax (key url &rest settings
                                      &key
                                      (timeout ein:query-timeout)
                                      &allow-other-keys)
  "Cancel the old process if there is a process associated with
KEY, then call `request' with URL and SETTINGS.  KEY is compared by
`equal'."
  (ein:query-gc-running-process-table)
  (when timeout
    (setq settings (plist-put settings :timeout (/ timeout 1000.0))))
  (ein:aif (gethash key ein:query-running-process-table)
      (unless (request-response-done-p it)
        (request-abort it)))            ; This will run callbacks
  (let ((response (apply #'request url settings)))
    (puthash key response ein:query-running-process-table)
    response))

(defun ein:query-gc-running-process-table ()
  "Garbage collect dead processes in `ein:query-running-process-table'."
  (maphash
   (lambda (key buffer)
     (when (request-response-done-p buffer)
       (remhash key ein:query-running-process-table)))
   ein:query-running-process-table))


;;; Cookie

(defalias 'ein:query-get-cookie 'request-cookie-string)

(provide 'ein-query)

;;; ein-query.el ends here
