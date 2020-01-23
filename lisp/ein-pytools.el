;;; ein-pytools.el --- Python tools build on top of kernel

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-pytools.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pytools.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pytools.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-kernel)
(require 'ein-notebook)

(defun ein:goto-file (filename lineno &optional other-window)
  "Jump to file FILEAME at line LINENO.
If OTHER-WINDOW is non-`nil', open the file in the other window."
  (funcall (if other-window #'find-file-other-window #'find-file) filename)
  (goto-char (point-min))
  (forward-line (1- lineno)))

(defun ein:goto-marker (marker &optional other-window)
  (funcall (if other-window #'pop-to-buffer #'switch-to-buffer)
           (marker-buffer marker))
  (goto-char marker))

(defcustom ein:propagate-connect t
  "Set to `t' to connect to the notebook after jumping to a buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

(defun ein:pytools-request-help (kernel func)
  (interactive (list (ein:get-kernel-or-error)
                     (ein:object-at-point-or-error)))
  (ein:kernel-execute kernel
                      (format "%s?" func) ; = code
                      nil                 ; = callbacks
                      ;; It looks like that magic command does
                      ;; not work in silent mode.
                      :silent nil))

(defvar ein:pytools-jump-stack nil)

(defvar ein:pytools-jump-to-source-not-found-regexp
  (ein:join-str "\\|"
                (list "^WARNING: .*"
                      "^Traceback (most recent call last):\n"
                      "^.*<ipython-input-[^>\n]+>\n"
                      "^\n")))

(defun ein:pytools-jump-to-source-1 (packed msg-type content -metadata-not-used-)
  (ein:log 'debug "msg-type[[%s]] content[[%s]]" msg-type content)
  (destructuring-bind (kernel object other-window notebook) packed
    (ein:log 'debug "object[[%s]] other-window[[%s]]" object other-window)
    (ein:case-equal msg-type
      (("stream" "display_data")
       (aif (or (plist-get content :text) (plist-get (plist-get content :data) :text/plain))
           (if (string-match ein:pytools-jump-to-source-not-found-regexp it)
               (ein:log 'info
                 "Jumping to the source of %s...Not found" object)
             (destructuring-bind (filename &optional lineno &rest ignore)
                 (split-string it "\n")
               (setq lineno (string-to-number lineno)
                     filename (ein:kernel-filename-from-python kernel filename))
               (ein:log 'debug "filename[[%s]] lineno[[%s]] ignore[[%s]]"
                        filename lineno ignore)
               (unless (file-exists-p filename)
                 (ein:log 'info
                   "Jumping to the source of %s...Not found" object))))))
      (("pyerr" "error")
       (ein:log 'info "Jumping to the source of %s...Not found" object)))))

(defun ein:pytools-jump-to-source (kernel object &optional
                                          other-window notebook)
  (ein:log 'info "Jumping to the source of %s..." object)
  (let ((last (car ein:pytools-jump-stack)))
    (if (ein:aand last (eql (current-buffer) (marker-buffer it)))
        (unless (equal (point) (marker-position last))
          (push (point-marker) ein:pytools-jump-stack))
      (setq ein:pytools-jump-stack (list (point-marker)))))
  (ein:kernel-execute
   kernel
   (format "__ein_find_source('%s')" object)
   (list
    :output
    (cons
     #'ein:pytools-jump-to-source-1
     (list kernel object other-window notebook)))))

(defun ein:pytools-jump-to-source-command (&optional other-window)
  "Jump to the source code of the object at point.
When the prefix argument ``C-u`` is given, open the source code
in the other window.  You can explicitly specify the object by
selecting it."
  (interactive "P")
  (if poly-ein-mode
      (cl-letf (((symbol-function 'xref--prompt-p) #'ignore))
        (if other-window
            (call-interactively #'xref-find-definitions-other-window)
          (call-interactively #'xref-find-definitions)))
    (let ((kernel (ein:get-kernel))
          (object (ein:object-at-point)))
      (assert (ein:kernel-live-p kernel) nil "Kernel is not ready.")
      (assert object nil "Object at point not found.")
      (ein:pytools-jump-to-source kernel object other-window
                                  (when ein:propagate-connect
                                    (ein:get-notebook))))))

(defun ein:pytools-jump-back-command (&optional other-window)
  "Go back to the point where `ein:pytools-jump-to-source-command'
is executed last time.  When the prefix argument ``C-u`` is
given, open the last point in the other window."
  (interactive "P")
  (if poly-ein-mode
      (call-interactively #'xref-pop-marker-stack)
    (when (ein:aand (car ein:pytools-jump-stack)
                    (equal (point) (marker-position it)))
      (setq ein:pytools-jump-stack (cdr ein:pytools-jump-stack)))
    (aif (car ein:pytools-jump-stack)
        (ein:goto-marker it other-window)
      (ein:log 'info "Nothing on stack."))))

(define-obsolete-function-alias
  'ein:pytools-eval-string-internal
  'ein:shared-output-eval-string "0.1.2")

(provide 'ein-pytools)

;;; ein-pytools.el ends here
