;; -*- lexical-binding: t -*-
;;; ein-shared-output.el --- Output buffer for ein-connect.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-shared-output.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-shared-output.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-shared-output.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When executing code from outside of notebook, some place for output
;; is needed.  This module buffer containing one special cell for that
;; purpose.

;; TODO - Undo accounting is almost certainly broken by this module

;;; Code:

(require 'eieio)

(require 'ein-cell)


;;; Classes and variables

(defclass ein:shared-output-cell (ein:codecell)
  ((cell-type :initarg :cell-type :initform "shared-output")
   ;; (element-names :initform (:prompt :output :footer))
   (popup :initarg :popup :initform nil :type boolean)
   (callback :initarg :callback :initform #'ignore :type function)
   (callback-called :initarg :callback-called :initform nil :type boolean))
  "A singleton cell to show output from non-notebook buffers.")

(defclass ein:shared-output ()
  ((cell :initarg :cell :type ein:shared-output-cell)
   (events :initarg :events :type ein:events)
   (ewoc :initarg :ewoc :type ewoc)))

(defvar *ein:shared-output* nil
  "Hold an instance of `ein:shared-output'.")

(defconst ein:shared-output-buffer-name "*ein:shared-output*")


;;; Cell related

(cl-defmethod ein:cell-insert-prompt ((cell ein:shared-output-cell))
  "Insert prompt of the CELL in the buffer.
Called from ewoc pretty printer via `ein:cell-pp'."
  ;; Newline is inserted in `ein:cell-insert-input'.
  (ein:insert-read-only
   (concat
    (format "In [%s]" (or (ein:oref-safe cell 'input-prompt-number)  " "))
    (when (slot-value cell 'autoexec) " %s" ein:cell-autoexec-prompt))
   'font-lock-face 'ein:cell-input-prompt))

(cl-defmethod ein:cell-execute ((cell ein:shared-output-cell) kernel code popup
                                &rest args)
  (unless (plist-get args :silent)
    (setq args (plist-put args :silent nil)))
  (setf (slot-value cell 'popup) popup)
  (setf (slot-value cell 'kernel) kernel)
  (apply #'ein:cell-execute-internal cell kernel code args))

(cl-defmethod ein:cell--handle-output ((cell ein:shared-output-cell)
                                       msg-type _content _metadata)
  (ein:log 'debug
    "ein:cell--handle-output (cell ein:shared-output-cell): %s" msg-type)
  (when (slot-value cell 'popup)
    (pop-to-buffer (ein:shared-output-create-buffer)))
  (cl-call-next-method)
  (ein:aif (ein:oref-safe cell 'callback)
      (progn
        (funcall it cell)
        (setf (slot-value cell 'callback-called) t))))

(cl-defmethod ein:cell--handle-execute-reply ((cell ein:shared-output-cell)
                                              content _metadata)
  (ein:log 'debug
    "ein:cell--handle-execute-reply (cell ein:shared-output-cell): %s"
    content)
  (cl-call-next-method)
  (ein:aif (ein:oref-safe cell 'callback)
    ;; clear the way for waiting block in `ob-ein--execute-async'
    ;; but only after 2 seconds to allow for handle-output stragglers
    ;; TODO avoid this hack
      (progn
        (unless (ein:oref-safe cell 'callback-called)
          (funcall it cell)
          (setf (slot-value cell 'callback-called) t))
        (run-at-time 2 nil (lambda ()
                             (ein:log 'debug "Clearing callback shared output cell")
                             (setf (slot-value cell 'callback) #'ignore)
                             (setf (slot-value cell 'callback-called) nil))))))


;;; Main

(defun ein:shared-output-create-buffer ()
  "Get or create the shared output buffer."
  (get-buffer-create ein:shared-output-buffer-name))

(defun ein:shared-output-buffer ()
  "Get the buffer associated with `*ein:shared-output*'."
  (ewoc-buffer (slot-value *ein:shared-output* 'ewoc)))

(defun ein:shared-output-buffer-p (&optional buffer)
  "Return non-`nil' when BUFFER (or current buffer) is shared-output buffer."
  (eq (or buffer (current-buffer)) (ein:shared-output-buffer)))

(defun ein:shared-output-healthy-p ()
  (and (ein:shared-output-p *ein:shared-output*)
       (buffer-live-p (ein:shared-output-buffer))))

(defun ein:shared-output-get-or-create ()
  (if (ein:shared-output-healthy-p)
      *ein:shared-output*
    (with-current-buffer (ein:shared-output-create-buffer)
      ;; FIXME: This is a duplication of `ein:worksheet-render'.
      (let* ((inhibit-read-only t)
             ;; Apply read-only text property to newlines by
             ;; setting nonsep flag to `ein:ewoc-create'
             (ewoc (let ((buffer-undo-list t))
                     (ein:ewoc-create 'ein:worksheet-pp
                                      (ein:propertize-read-only "\n")
                                      nil t)))
             (events (ein:events-new))
             (cell (ein:shared-output-cell :ewoc ewoc
                                           :events events)))
        (erase-buffer)
        (ein:shared-output-bind-events events)
        (setq *ein:shared-output*
              (ein:shared-output :ewoc ewoc :cell cell
                                 :events events))
        (ein:cell-enter-last cell))
      (setq buffer-read-only t)
      (ein:shared-output-mode)
      *ein:shared-output*)))

(defun ein:shared-output-bind-events (events)
  "Add dummy event handlers."
  (ein:events-on events 'set_dirty.Worksheet #'ignore)
  (ein:events-on events 'maybe_reset_undo.Worksheet #'ignore))

(defun ein:shared-output-get-cell ()
  "Get the singleton shared output cell.
Create a cell if the buffer has none."
  (slot-value (ein:shared-output-get-or-create) 'cell))

;;;###autoload
(defun ein:shared-output-pop-to-buffer ()
  "Open shared output buffer."
  (interactive)
  (ein:shared-output-get-or-create)
  (pop-to-buffer (ein:shared-output-create-buffer)))

(cl-defmethod ein:shared-output-show-code-cell ((cell ein:codecell))
  "Show code CELL in shared-output buffer."
  (let ((new (ein:cell-convert cell "shared-output")))
    ;; Make sure `*ein:shared-output*' is initialized:
    (ein:shared-output-get-or-create)
    (with-current-buffer (ein:shared-output-create-buffer)
      (let ((inhibit-read-only t)
            (ein:cell-max-num-outputs nil))
        (setf (slot-value new 'ewoc) (slot-value *ein:shared-output* 'ewoc))
        (setf (slot-value new 'events) (slot-value *ein:shared-output* 'events))
        (erase-buffer)  ; because there are only one cell anyway
        (setf (slot-value *ein:shared-output* 'cell) new)
        (ein:cell-enter-last new)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun ein:shared-output-show-code-cell-at-point ()
  "Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein:cell-max-num-outputs'."
  (interactive)
  (let ((cell (ein:get-cell-at-point)))
    (if (ein:codecell-p cell)
        (ein:shared-output-show-code-cell cell)
      (error "No code cell at point."))))

(defvar ein:shared-output-eval-string-history nil
  "History of the `ein:shared-output-eval-string' prompt.")

;;;###autoload
(defun ein:shared-output-eval-string (kernel code popup &rest args)
  "Evaluate a piece of code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ein:shared-output-pop-to-buffer'.

.. ARGS is passed to `ein:kernel-execute'.  Unlike `ein:kernel-execute',
   `:silent' is `nil' by default."
  (interactive
   (list nil
         (read-string
          "IP[y]: "
          (when (region-active-p)
            (buffer-substring (region-beginning) (region-end)))
          'ein:shared-output-eval-string-history)
         nil))
  (unless kernel (setq kernel (ein:get-kernel-or-error)))
  (let ((cell (ein:shared-output-get-cell)))
    (ein:kernel-when-ready
     kernel
     (lambda (ready-kernel)
       (apply #'ein:cell-execute cell ready-kernel (ein:trim-indent code) popup args)))))


;;; Generic getter

(defun ein:get-url-or-port--shared-output ()
  (ein:aand (ein:get-kernel--shared-output) (ein:kernel-url-or-port it)))

;; (defun ein:get-notebook--shared-output ())

(defun ein:get-kernel--shared-output ()
  (let ((cell (ein:get-cell-at-point--shared-output)))
    (when (and (eieio-object-p cell) (slot-boundp cell :kernel))
      (slot-value cell 'kernel))))

(defun ein:get-cell-at-point--shared-output ()
  (when (and (ein:shared-output-p *ein:shared-output*)
             (ein:shared-output-buffer-p))
    (slot-value *ein:shared-output* 'cell)))

(defun ein:get-traceback-data--shared-output ()
  (ein:aand (ein:get-cell-at-point--shared-output) (ein:cell-get-tb-data it)))


;;; ein:shared-output-mode

(defvar ein:shared-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-x" 'ein:tb-show)
    (define-key map "\M-."          'ein:pytools-jump-to-source-command)
    (define-key map (kbd "C-c C-.") 'ein:pytools-jump-to-source-command)
    map)
  "The map for ein:shared-output-mode-map.")

(define-derived-mode ein:shared-output-mode special-mode "ein:so"
  "Shared output mode."
  (font-lock-mode))

(add-hook 'ein:shared-output-mode-hook 'ein:truncate-lines-on)


(provide 'ein-shared-output)

;;; ein-shared-output.el ends here
