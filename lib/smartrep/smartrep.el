;;; smartrep.el --- Support sequential operation which omitted prefix keys.

;; Filename: smartrep.el
;; Description: Support sequential operation which omitted prefix keys.
;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; Maintainer: myuhe
;; Copyright (C) :2011,2012 myuhe all rights reserved.
;; Created: :2011-12-19
;; Version: 1.0.0
;; Keywords: convenience
;; URL: https://github.com/myuhe/smartrep.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:

;; Installation:
;;   Put the smartrep.el to your load-path.
;;   And add to .emacs: (require 'smartrep)

;;; Changelog:

;; 2012-01-06 Remove unnecessary cord.
;; 2012-01-06 read-key is replaced read-event for compatibility. thanks @tomy_kaira !!
;; 2012-01-11 Support function calling form. (buzztaiki)
;;            Call interactively when command. (buzztaiki) 
;;            Support unquoted function. (buzztaiki)
;; 2012-01-11 new command `smartrep-restore-original-position' `smartrep-quit' (rubikitch)
;;            add mode line notification (rubikitch)
;; 2012-01-12 add mode-line-color notification
;;            

;;; Code:
(eval-when-compile
  (require 'cl))

(defgroup smartrep nil
  "Support sequential operation which omitted prefix keys"
  :group 'keyboard)

(defcustom smartrep-mode-line-string-activated "========== SMARTREP =========="
  "Lighter when smartrep-mode is activated"
  :type 'string
  :group 'smartrep)

(defcustom smartrep-mode-line-active-bg (face-background 'highlight)
  "Background color when smartrep-mode is activated"
  :type 'string
  :group 'smartrep)

(defvar smartrep-key-string nil)

(defvar smartrep-read-event
  (if (fboundp 'read-event) 'read-event 'read-key)
  "Function to be used for reading keyboard events.")

(defvar smartrep-mode-line-string nil
  "Mode line indicator for smartrep.")

(defvar smartrep-global-alist-hash (make-hash-table :test 'equal))

(defvar smartrep-original-position nil
  "A cons holding the point and window-start when smartrep is invoked.")

(let ((cell (or (memq 'mode-line-position mode-line-format) 
		(memq 'mode-line-buffer-identification mode-line-format))) 
      (newcdr 'smartrep-mode-line-string))
  (when cell
    (unless (member newcdr mode-line-format)
      (setcdr cell (cons newcdr (cdr cell))))))

(defun smartrep-define-key (keymap prefix alist)
  (when (eq keymap global-map)
    (puthash prefix alist smartrep-global-alist-hash))
  (setq alist
        (if (eq keymap global-map)
            alist
          (append alist (gethash prefix smartrep-global-alist-hash))))
  (let ((oa (make-vector 13 nil)))
    (mapc (lambda(x)
	    (let ((obj (intern (prin1-to-string
				(smartrep-unquote (cdr x)))
			       oa)))
	      (fset obj (smartrep-map alist))
	      (define-key keymap
		(read-kbd-macro 
		 (concat prefix " " (car x))) obj)))
	  alist)))
(put 'smartrep-define-key 'lisp-indent-function 2)

(defun smartrep-map (alist)
  (lexical-let ((lst alist))
    (lambda () (interactive) (smartrep-map-internal lst))))

(defun smartrep-restore-original-position ()
  (interactive)
  (destructuring-bind (pt . wstart) smartrep-original-position
    (goto-char pt)
    (set-window-start (selected-window) wstart)))

(defun smartrep-quit ()
  (interactive)
  (setq smartrep-mode-line-string "")
  (smartrep-restore-original-position)
  (keyboard-quit))

(defun smartrep-map-internal (lst)
  (interactive)
  (setq smartrep-mode-line-string smartrep-mode-line-string-activated)
  (let ((ml-original-bg (face-background 'mode-line)))
      (when smartrep-mode-line-active-bg
        (set-face-background 'mode-line smartrep-mode-line-active-bg)
        (force-mode-line-update))
      (setq smartrep-original-position (cons (point) (window-start)))
      (unwind-protect
          (let ((repeat-repeat-char last-command-event))
              (smartrep-do-fun repeat-repeat-char lst)
            (when repeat-repeat-char
              (smartrep-read-event-loop lst)))
        (setq smartrep-mode-line-string "")
        (when smartrep-mode-line-active-bg
          (set-face-background 'mode-line ml-original-bg)
          (force-mode-line-update)))))

(defun smartrep-read-event-loop (lst)
  (lexical-let ((undo-inhibit-record-point t))
    (unwind-protect
        (while
            (lexical-let ((evt (funcall smartrep-read-event)))
              ;; (eq (or (car-safe evt) evt)
              ;;     (or (car-safe repeat-repeat-char)
              ;;         repeat-repeat-char))
              (setq smartrep-key-string evt)
              (smartrep-extract-char evt lst))
          (smartrep-do-fun smartrep-key-string lst)))
    (setq unread-command-events (list last-input-event))))

(defun smartrep-extract-char (char alist)
  (car (smartrep-filter char alist)))

(defun smartrep-extract-fun (char alist)
  (let* ((rawform (cdr (smartrep-filter char alist)))
         (form (smartrep-unquote rawform)))
    (cond
     ((commandp form) 
      (setq this-command form)
      (unwind-protect
          (call-interactively form)
        (setq last-command form)))
     ((functionp form) (funcall form))
     ((and (listp form) (symbolp (car form))) (eval form))
     (t (error "Unsupported form %c %s" char rawform)))))

(defun smartrep-do-fun (char alist)
  (condition-case err
      (progn
        (run-hooks 'pre-command-hook)
        (smartrep-extract-fun char alist)
        (run-hooks 'post-command-hook))
    (error
     (ding)
     (message "%s" (cdr err)))))
    

(defun smartrep-unquote (form)
  (if (and (listp form) (memq (car form) '(quote function)))
      (eval form)
    form))

(defun smartrep-filter (char alist)
  (loop for (key . form) in alist
        for rkm = (read-kbd-macro key)
        for number = (if (vectorp rkm)
                         (aref rkm 0)
                       (string-to-char rkm))
        if (eq char number)
        return (cons number form)))

(provide 'smartrep)

;;; smartrep.el ends here
