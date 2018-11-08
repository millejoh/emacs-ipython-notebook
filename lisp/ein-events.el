;;; ein-events.el --- Event module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-events.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-events.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-events.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ein-core)
(require 'ein-classes)
(require 'ein-log)

(defun ein:events-new ()
  "Return a new event handler instance."
  (make-instance 'ein:events))

(defun ein:events-trigger (events event-type &optional data)
  "Trigger EVENT-TYPE and let event handler EVENTS handle that event."
  (ein:log 'debug "Event: %S" event-type)
  (ein:aif (gethash event-type (slot-value events 'callbacks))
      (mapc (lambda (cb-arg) (ein:funcall-packed cb-arg data)) it)
    (ein:log 'info "Unknown event: %S" event-type)))


(cl-defmethod ein:events-on ((events ein:events) event-type
                             callback &optional arg)
  "Set event trigger hook.

When EVENT-TYPE is triggered on the event handler EVENTS,
CALLBACK is called.  CALLBACK must take two arguments:
ARG as the first argument and DATA, which is passed via
`ein:events-trigger', as the second."
  (assert (symbolp event-type))
  (let* ((table (slot-value events 'callbacks))
         (cbs (gethash event-type table)))
    (push (cons callback arg) cbs)
    (puthash event-type cbs table)))


(provide 'ein-events)

;;; ein-events.el ends here
