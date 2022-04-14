;;; ein-ipdb.el --- Support ipython debugger (ipdb)    -*- lexical-binding:t -*-

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

(require 'cl-lib)

(defvar *ein:ipdb-sessions* (make-hash-table)
  "Kernel Id to ein:$ipdb-session.")

(declare-function ein:kernel--get-msg "ein-kernel")

(cl-defstruct ein:$ipdb-session buffer kernel prompt notebook)

(defun ein:ipdb-get-session (kernel)
  (gethash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*))

(defun ein:ipdb-start-session (kernel prompt notebook)
  (let* ((buffer (get-buffer-create
                  (format "*ipdb: %s*"
                          (ein:$kernel-kernel-id kernel))))
         (session (make-ein:$ipdb-session :buffer buffer
                                          :kernel kernel
                                          :prompt prompt
                                          :notebook notebook)))
    (puthash (ein:$kernel-kernel-id kernel) session *ein:ipdb-sessions*)
    (with-current-buffer buffer
      (kill-all-local-variables)
      (add-hook 'kill-buffer-hook
                (apply-partially #'ein:ipdb-quit-session session) nil t)
      (ein:ipdb-mode)
      (setq comint-use-prompt-regexp t)
      (setq comint-prompt-regexp (concat "^" (regexp-quote prompt)))
      (setq comint-input-sender (apply-partially #'ein:ipdb-input-sender session))
      (setq comint-prompt-read-only t)
      (set (make-local-variable 'comint-output-filter-functions)
           '(ansi-color-process-output))
      (let ((proc (start-process "ein:ipdb" buffer "cat"))
            (sentinel (lambda (process _event)
                        (when (memq (process-status process) '(exit signal))
                          (ein:ipdb-cleanup-session session)))))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc sentinel)
        (set-marker (process-mark proc) (point))
        (comint-output-filter proc (concat "\n" (ein:$ipdb-session-prompt session)))))
    (pop-to-buffer buffer)))

(defun ein:ipdb-quit-session (session)
  (let* ((kernel (ein:$ipdb-session-kernel session))
         (msg (ein:kernel--get-msg kernel "input_reply" (list :value "exit"))))
    (ein:websocket-send-stdin-channel kernel msg)))

(defun ein:ipdb-stop-session (session)
  (awhen (get-buffer-process (ein:$ipdb-session-buffer session))
    (when (process-live-p it)
      (kill-process it))))

(defun ein:ipdb-cleanup-session (session)
  (let ((kernel (ein:$ipdb-session-kernel session))
        (notebook (ein:$ipdb-session-notebook session))
        (buffer (ein:$ipdb-session-buffer session)))
    (remhash (ein:$kernel-kernel-id kernel) *ein:ipdb-sessions*)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (insert "\nFinished\n")))
    (awhen (ein:notebook-buffer notebook)
      (when (buffer-live-p it)
        (pop-to-buffer it)))))

(defun ein:ipdb--handle-iopub-reply (kernel packet)
  (cl-destructuring-bind
      (&key content &allow-other-keys)
      (ein:json-read-from-string packet)
    (-when-let* ((session (ein:ipdb-get-session kernel))
                 (buffer (ein:$ipdb-session-buffer session))
                 (prompt (ein:$ipdb-session-prompt session))
                 (proc (get-buffer-process buffer))
                 (proc-live-p (process-live-p proc)))
      (let ((text (plist-get content :text))
            (ename (plist-get content :ename)))
        (when (stringp text)
          (comint-output-filter proc text))
        (if (and (stringp ename) (string-match-p "bdbquit" ename))
            (ein:ipdb-stop-session session)
          (comint-output-filter proc prompt))))))

(defun ein:ipdb-input-sender (session proc input)
  ;; in case of eof, comint-input-sender-no-newline is t
  (if comint-input-sender-no-newline
      (ein:ipdb-quit-session session)
    (when (process-live-p proc)
      (with-current-buffer (process-buffer proc)
        (let* ((buffer-read-only nil)
               (kernel (ein:$ipdb-session-kernel session))
               (content (list :value input))
               (msg (ein:kernel--get-msg kernel "input_reply" content)))
          (ein:websocket-send-stdin-channel kernel msg))))))

(define-derived-mode ein:ipdb-mode comint-mode "ein:debugger"
    "Run an EIN debug session.

\\<ein:ipdb-mode-map>")

(provide 'ein-ipdb)
