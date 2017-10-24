;;; ein-notification.el --- Notification widget for Notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notification.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notification.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notification.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ein-core)
(require 'ein-classes)
(require 'ein-events)


;; Class and variable

(ein:deflocal ein:%notification% nil
  "Buffer local variable to hold an instance of `ein:notification'.")
(define-obsolete-variable-alias 'ein:@notification 'ein:%notification% "0.1.2")

(defvar ein:header-line-format '(:eval (ein:header-line)))
(defvar ein:header-line-tab-map (make-sparse-keymap))
(defvar ein:header-line-insert-tab-map (make-sparse-keymap))
(defvar ein:header-line-switch-kernel-map (make-sparse-keymap))
(defvar ein:header-line-tab-help
  "\
mouse-1 (left click) : switch to this tab
mouse-3 (right click) : pop to this tab
mouse-2 (middle click) : delete this tab
M-mouse-1/3 (Alt + left/right click): insert new tab to left/right
S-mouse-1/3 (Shift + left/right click): move this tab to left/right"
  "Help message.")
;; Note: can't put this below of `ein:notification-setup'...

(defmethod ein:notification-status-set ((ns ein:notification-status) status)
  (let* ((message (cdr (assoc status (slot-value ns 's2m)))))
    (setf (slot-value ns 'status) status)
    (setf (slot-value ns 'message) message)))

(defmethod ein:notification-bind-events ((notification ein:notification)
                                         events)
  "Bind a callback to events of the event handler EVENTS which
just set the status \(= event-type):
    \(ein:notification-status-set NS EVENT-TYPE)
where NS is `:kernel' or `:notebook' slot of NOTIFICATION."
  (loop for ns in (list (slot-value notification 'kernel)
                        (slot-value notification 'notebook))
        for statuses = (mapcar #'car (slot-value ns 's2m))
        do (loop for st in statuses
                 do (ein:events-on events
                                   st   ; = event-type
                                   #'ein:notification--callback
                                   (cons ns st))))
  (ein:events-on events
                 'notebook_checkpoint_created.Notebook
                 #'ein:notification--fadeout-callback
                 (list (slot-value notification 'notebook)
                       "Checkpoint created."
                       'notebook_checkpoint_created.Notebook
                       nil))
  (ein:events-on events
                 'notebook_saved.Notebook
                 #'ein:notification--fadeout-callback
                 (list (slot-value notification 'notebook)
                       "Notebook is saved"
                       'notebook_saved.Notebook
                       nil))
  (ein:events-on events
                 'execution_count.Kernel
                 #'ein:notification--set-execution-count
                 notification)
  (ein:events-on events
                 'status_restarting.Kernel
                 #'ein:notification--fadeout-callback
                 (list (slot-value notification 'kernel)
                       "Restarting kernel..."
                       'status_restarting.Kernel
                       'status_idle.Kernel)))

(defun ein:notification--callback (packed data)
  (let ((ns (car packed))
        (status (cdr packed)))
    (ein:notification-status-set ns status)))

(defun ein:notification--set-execution-count (notification count)
  (oset notification :execution-count count))

(defun ein:notification--fadeout-callback (packed data)
  ;; FIXME: I can simplify this.
  ;;        Do not pass around message, for exmaple.
  (let ((ns (nth 0 packed))
        (message (nth 1 packed))
        (status (nth 2 packed))
        (next (nth 3 packed)))
    (oset ns :status status)
    (oset ns :message message)
    (apply #'run-at-time
           1 nil
           (lambda (ns message status next)
             (when (equal (slot-value ns 'status) status)
               (ein:notification-status-set ns next)
               ;; (ein:with-live-buffer (slot-value ns :buffer)
               ;;   (force-mode-line-update))
               ))
           packed)))

(defun ein:notification-setup (buffer events &rest tab-slots)
  "Setup a new notification widget in the BUFFER.
This function saves the new notification widget instance in the
local variable of the BUFFER.

Rest of the arguments are for TABs in `header-line'.

GET-LIST : function
  Return a list of worksheets.

GET-CURRENT : function
  Return the current worksheet.

GET-NAME : function
  Return a name of the worksheet given as its argument.

GET-BUFFER : function
  Get a buffer of given worksheet.  Render it if needed.

DELETE : function
  Remove a given worksheet.

INSERT-PREV / INSERT-NEXT : function
  Insert new worksheet before/after the specified worksheet.

MOVE-PREV / MOVE-NEXT : function
  Switch this worksheet to the previous/next one.

\(fn buffer events &key get-list get-current get-name get-buffer delete \
insert-prev insert-next move-prev move-next)"
  (with-current-buffer buffer
    (setq ein:%notification%
          (make-instance 'ein:notification
                         :buffer buffer))
    (setq header-line-format ein:header-line-format)
    (ein:notification-bind-events ein:%notification% events)
    (oset ein:%notification% :tab
          (apply #'make-instance 'ein:notification-tab tab-slots))
    ein:%notification%))


;;; Tabs

(defface ein:notification-tab-selected
  '((t :inherit (header-line match) :underline t))
  "Face for headline selected tab."
  :group 'ein)

(defface ein:notification-tab-normal
  '((t :inherit (header-line) :underline t :height 0.8))
  "Face for headline selected tab."
  :group 'ein)

(defmethod ein:notification-tab-create-line ((tab ein:notification-tab))
  (let ((list (funcall (slot-value tab 'get-list)))
        (current (funcall (slot-value tab 'get-current)))
        (get-name (slot-value tab 'get-name)))
    (ein:join-str
     " "
     (append
      (loop for i from 1
            for elem in list
            if (eq elem current)
            collect (propertize
                     (or (ein:and-let* ((name (funcall get-name elem)))
                           (format "/%d: %s\\" i name))
                         (format "/%d\\" i))
                     'ein:worksheet elem
                     'keymap ein:header-line-tab-map
                     'help-echo ein:header-line-tab-help
                     'mouse-face 'highlight
                     'face 'ein:notification-tab-selected)
            else
            collect (propertize
                     (format "/%d\\" i)
                     'ein:worksheet elem
                     'keymap ein:header-line-tab-map
                     'help-echo ein:header-line-tab-help
                     'mouse-face 'highlight
                     'face 'ein:notification-tab-normal))
      (list
       (propertize "[+]"
                   'keymap ein:header-line-insert-tab-map
                   'help-echo "Click (mouse-1) to insert a new tab."
                   'mouse-face 'highlight
                   'face 'ein:notification-tab-normal)
       (propertize (ein:aif (ein:$notebook-kernelspec ein:%notebook%)
                       (format "|%s|" (ein:$kernelspec-name it))
                     "|unknown: please click and select a kernel|")
                   'keymap ein:header-line-switch-kernel-map
                   'help-echo "Click (mouse-1) to change the running kernel."
                   'mouse-face 'highlight
                   'face 'ein:notification-tab-normal))))))


;;; Header line

(let ((map ein:header-line-tab-map))
  (define-key map [header-line M-mouse-1] 'ein:header-line-insert-prev-tab)
  (define-key map [header-line M-mouse-3] 'ein:header-line-insert-next-tab)
  (define-key map [header-line S-mouse-1] 'ein:header-line-move-prev-tab)
  (define-key map [header-line S-mouse-3] 'ein:header-line-move-next-tab)
  (define-key map [header-line mouse-1] 'ein:header-line-switch-to-this-tab)
  (define-key map [header-line mouse-2] 'ein:header-line-delete-this-tab)
  (define-key map [header-line mouse-3] 'ein:header-line-pop-to-this-tab))

(define-key ein:header-line-insert-tab-map
  [header-line mouse-1] 'ein:header-line-insert-new-tab)

(define-key ein:header-line-switch-kernel-map
  [header-line mouse-1] 'ein:header-line-switch-kernel)

(defmacro ein:with-destructuring-bind-key-event (key-event &rest body)
  (declare (debug (form &rest form))
           (indent 1))
  ;; See: (info "(elisp) Click Events")
  `(destructuring-bind
       (event-type
        (window pos-or-area (x . y) timestamp
                object text-pos (col . row)
                image (dx . dy) (width . height)))
       ,key-event
     ,@body))

(defun ein:header-line-select-window (key-event)
  (ein:with-destructuring-bind-key-event key-event (select-window window)))

(defun ein:header-line-key-event-get-worksheet (key-event)
  (ein:with-destructuring-bind-key-event key-event
    (get-char-property (cdr object) 'ein:worksheet (car object))))

(defun ein:header-line-key-event-get-buffer (key-event)
  (funcall (slot-value (slot-value ein:%notification% 'tab) 'get-buffer)
           (ein:header-line-key-event-get-worksheet key-event)))

(defun ein:header-line-switch-to-this-tab (key-event)
  (interactive "e")
  (ein:header-line-select-window key-event)
  (switch-to-buffer (ein:header-line-key-event-get-buffer key-event)))

(defun ein:header-line-pop-to-this-tab (key-event)
  (interactive "e")
  (ein:header-line-select-window key-event)
  (pop-to-buffer (ein:header-line-key-event-get-buffer key-event)))

(defun ein:header-line-do-slot-function (key-event slot)
  "Call SLOT function on worksheet instance fetched from KEY-EVENT."
  (ein:header-line-select-window key-event)
  (funcall (slot-value (slot-value ein:%notification% 'tab) slot)
           (ein:header-line-key-event-get-worksheet key-event)))

(defmacro ein:header-line-define-mouse-commands (&rest name-slot-list)
  `(progn
     ,@(loop for (name slot) on name-slot-list by 'cddr
             collect
             `(defun ,name (key-event)
                ,(format "Run slot %s
Generated by `ein:header-line-define-mouse-commands'" slot)
                (interactive "e")
                (ein:header-line-do-slot-function key-event ,slot)))))

(ein:header-line-define-mouse-commands
 ein:header-line-delete-this-tab :delete
 ein:header-line-insert-prev-tab :insert-prev
 ein:header-line-insert-next-tab :insert-next
 ein:header-line-move-prev-tab :move-prev
 ein:header-line-move-next-tab :move-next
 )

(defun ein:header-line-insert-new-tab (key-event)
  "Insert new tab."
  (interactive "e")
  (ein:header-line-select-window key-event)
  (let ((notification (slot-value ein:%notification% 'tab)))
    (funcall (slot-value notification 'insert-next)
             (car (last (funcall (slot-value notification 'get-list)))))))

(defun ein:header-line-switch-kernel (key-event)
  (interactive "e")
  (let* ((notebook (or (ein:get-notebook)
                       (completing-read
                        "Select notebook [URL-OR-PORT/NAME]: "
                        (ein:notebook-opened-buffer-names))))
         (kernel-name (completing-read
                       "Select kernel: "
                       (ein:list-available-kernels (ein:$notebook-url-or-port notebook)))))
    (ein:notebook-switch-kernel notebook kernel-name)))

(defun ein:header-line ()
  (format
   "IP[%s]: %s"
   (slot-value ein:%notification% 'execution-count)
   (ein:join-str
    " | "
    (ein:filter
     'identity
     (list (slot-value (slot-value ein:%notification% 'notebook) 'message)
           (slot-value (slot-value ein:%notification% 'kernel) 'message)
           (ein:notification-tab-create-line
            (slot-value ein:%notification% 'tab)))))))

(defun ein:header-line-setup-maybe ()
  "Setup `header-line-format' for mumamo.
As `header-line-format' is buffer local variable, it must be set
for each chunk when in
See also `ein:ac-setup-maybe'."
  (and (ein:eval-if-bound 'ein:%notebook%)
       (ein:eval-if-bound 'mumamo-multi-major-mode)
       (setq header-line-format ein:header-line-format)))
(add-hook 'after-change-major-mode-hook 'ein:header-line-setup-maybe)

(provide 'ein-notification)

;;; ein-notification.el ends here
