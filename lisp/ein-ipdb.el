;;; ein-ipdb.el --- Support ipython debugger (ipdb)

;; Copyright (C) 2015 - John Miller

;; Author: John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-ipdb.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ipdb.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar *ein:ipdb-sessions* (make-hash-table))
(defconst *ein:ipdb-prompt* "(Pdb) ")
(defvar ein:ipdb-buffer-active-kernel nil)
(defvar ein:ipdb-buffer-prompt nil)

(defstruct ein:$ipdb-session
  buffer
  notebook-buffer
  kernel
  current-payload)

(defun ein:find-or-create-ipdb-session (kernel &optional buffer)
  (ein:aif (gethash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*)
      it
    (let ((db-session (make-ein:$ipdb-session
                       :kernel kernel
                       :notebook-buffer buffer
                       :current-payload "Debugger started.")))
      (setf (gethash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*) db-session)
      db-session)))

(defun ein:pdb-session-id (session)
  (ein:$kernel-kernel-id (ein:$ipdb-session-kernel session)))

(defun ein:run-ipdb-session (kernel prompt)
  (unless (gethash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*)
    (let ((pdb-session (ein:find-or-create-ipdb-session kernel (current-buffer))))
      (setf (ein:$kernel-stdin-activep kernel) t)
      (ein:prepare-ipdb-session pdb-session prompt))))

(defun ein:prepare-ipdb-session-old (session prompt)
  (with-current-buffer  (setf (ein:$ipdb-session-buffer session)
                              (get-buffer-create (format "*ipdb: %s*" (ein:pdb-session-id session))))
    (add-hook 'kill-buffer-hook 'ein:ipdb-on-stop)
    (ein:ipdb-mode)
    (setq comint-prompt-regexp (concat "^" (regexp-quote prompt)))
    (setq comint-input-sender 'ein:ipdb-input-sender)

    (unless (comint-check-proc (current-buffer))
      ;; Was cat, but on non-Unix platforms that might not exist, so
      ;; use hexl instead, which is part of the Emacs distribution.
      (let ((fake-proc
             (condition-case nil
                 (start-process "ein:ipdb" (current-buffer) "cat")
               (file-error (start-process "ein:ipdb" (current-buffer) "hexl")))))
        (set-process-query-on-exit-flag fake-proc nil)
        (insert "#ipdb#\n")
        (set-marker
         (process-mark fake-proc) (point))
        (comint-output-filter fake-proc prompt)))
    (setq ein:ipdb-buffer-active-kernel (ein:pdb-session-id session))
    (setq ein:ipdb-buffer-prompt prompt)


    (switch-to-buffer (ein:$ipdb-session-buffer session))))

(defun ein:prepare-ipdb-session (session prompt)
  (with-current-buffer  (setf (ein:$ipdb-session-buffer session)
                              (get-buffer-create (format "*ipdb: %s*" (ein:pdb-session-id session))))
    (set (make-local-variable 'ein:ipdb-buffer-active-kernel) (ein:pdb-session-id session))
    (set (make-local-variable 'ein:ipdb-buffer-prompt) prompt)
    (put 'ein:ipdb-buffer-active-kernel 'permanent-local t)
    (put 'ein:ipdb-buffer-prompt 'permanent-local t)
    (add-hook 'kill-buffer-hook 'ein:ipdb-on-stop)

    (ein:ipdb-mode)

    (pop-to-buffer (ein:$ipdb-session-buffer session))))

(defun ein:ipdb-on-stop ()
  (when ein:ipdb-buffer-active-kernel
    (let* ((kernel (ein:$ipdb-session-kernel
                    (gethash ein:ipdb-buffer-active-kernel *ein:ipdb-sessions*)))
           (msg (ein:kernel--get-msg kernel "input_reply" (list :value "q"))))
      (ein:websocket-send-stdin-channel kernel msg)
      (setf (ein:$kernel-stdin-activep kernel) nil)
      (remhash ein:ipdb-buffer-active-kernel *ein:ipdb-sessions*))))

(defun ein:ipdb--handle-iopub-reply (kernel packet)
  (destructuring-bind
      (&key content metadata parent_header header &allow-other-keys)
      (ein:json-read-from-string packet)
    (let ((msg-type (plist-get header :msg_type)))
      (if (string-equal msg-type "stream")
          (let* ((session (ein:find-or-create-ipdb-session kernel))
                 (buf (ein:$ipdb-session-buffer session))
                 (text (plist-get content :text)))
            (with-current-buffer buf
              (setf (ein:$ipdb-session-current-payload session) text)
              (let ((buffer-read-only nil)
                    (proc (get-buffer-process buf)))
                (comint-output-filter proc text)
                (comint-output-filter proc ein:ipdb-buffer-prompt))
              (when ein:ipdb--received-quit-p
                (kill-buffer)
                (ein:aif (ein:$ipdb-session-notebook-buffer session)
                    (pop-to-buffer it)))))))))


;;; Now try with comint

(defun ein:ipdb-input-sender (proc input)
  (with-current-buffer (process-buffer proc)
    (assert (not (null ein:ipdb-buffer-active-kernel)) t "No active kernel associated with this buffer %s.")
    (let* ((session (gethash ein:ipdb-buffer-active-kernel *ein:ipdb-sessions*))
           (buffer-read-only nil)
           (kernel (ein:$ipdb-session-kernel session))
           (content (list :value input))
           (msg (ein:kernel--get-msg kernel "input_reply" content)))
      (ein:websocket-send-stdin-channel kernel msg)
      (when (or (string= input "q")
                (string= input "quit"))
        (setq ein:ipdb--received-quit-p t)))))

(defun ein:ipdb-buffer-initialize ()
  "Helper function to initialize a newly minted ein:ipdb buffer."
  (setq comint-use-prompt-regexp t))

(defvar ein:ipdb--received-quit-p nil)

(define-derived-mode ein:ipdb-mode comint-mode "EIN:IPDB"
    "Run an EIN debug session.

\\<ein:ipdb-mode-map>"
                                        ;:syntax-table python-mode-syntax-table
    (setq comint-prompt-regexp (concat "^" (regexp-quote ein:ipdb-buffer-prompt)))
    (setq comint-input-sender 'ein:ipdb-input-sender)
    (setq comint-prompt-read-only t)
    (set (make-local-variable 'comint-output-filter-functions)
         '(ansi-color-process-output
           python-pdbtrack-comint-output-filter-function))
    (set (make-local-variable 'ein:ipdb--received-quit-p) nil)

    (unless (comint-check-proc (current-buffer))
      ;; Was cat, but on non-Unix platforms that might not exist, so
      ;; use hexl instead, which is part of the Emacs distribution.
      (let ((fake-proc
             (condition-case nil
                 (start-process "ein:ipdb" (current-buffer) "cat")
               (file-error (start-process "ein:ipdb" (current-buffer) "hexl")))))
        (set-process-query-on-exit-flag fake-proc nil)
        (insert "=== EIN IPython Debugger ===\n")
        (set-marker (process-mark fake-proc) (point))
        (comint-output-filter fake-proc ein:ipdb-buffer-prompt))))

(add-hook 'ein:ipdb-mode-hook 'ein:ipdb-buffer-initialize)

(provide 'ein-ipdb)
