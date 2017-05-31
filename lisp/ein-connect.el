;;; ein-connect.el --- Connect external buffers to IPython

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-connect.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-connect.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-connect.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: There is a problem when connected notebook is closed.
;;        This can be fixed in some ways:
;; * Turn off ein:connect when the command that uses kernel is invoked
;;   but corresponding notebook was closed already.
;; * Connect directly to ein:kernel and make its destructor to care
;;   about connecting buffers.

;;; Code:

(require 'eieio)
(eval-when-compile (require 'auto-complete nil t))

(require 'ein-notebook)

(declare-function ein:notebooklist-list-notebooks "ein-notebooklist")
(declare-function ein:notebooklist-open-notebook-global "ein-notebooklist")


;;; Utils

(defun ein:maybe-save-buffer (option)
  "Conditionally save current buffer.
Return `t' if the buffer is unmodified or `nil' otherwise.
If the buffer is modified, buffer is saved depending on the value
of OPTION:
  ask  : Ask whether the buffer should be saved.
  yes  : Save buffer always.
  no   : Do not save buffer."
  (if (not (buffer-modified-p))
      t
    (case option
      (ask (when (y-or-n-p "Save buffer? ")
             (save-buffer)
             t))
      (yes (save-buffer)
           t)
      (t nil))))


;;; Configuration

(defcustom ein:connect-run-command "%run"
  "``%run`` magic command used for `ein:connect-run-buffer'.
Types same as `ein:console-security-dir' are valid."
  :type '(choice
          (string :tag "command" "%run")
          (alist :tag "command mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "command" "%run"))
          (function :tag "command getter"
                    (lambda (url-or-port) (format "%%run -n -i -t -d"))))
  :group 'ein)

(defcustom ein:connect-reload-command "%run -n"
  "Setting for `ein:connect-reload-buffer'.
Same as `ein:connect-run-command'."
  :type '(choice
          (string :tag "command" "%run")
          (alist :tag "command mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "command" "%run"))
          (function :tag "command getter"
                    (lambda (url-or-port) (format "%%run -n -i -t -d"))))
  :group 'ein)

(defun ein:connect-run-command-get ()
  (ein:choose-setting 'ein:connect-run-command
                      (ein:$notebook-url-or-port (ein:connect-get-notebook))))

(defcustom ein:connect-save-before-run 'yes
  "Whether the buffer should be saved before `ein:connect-run-buffer'."
  :type '(choice (const :tag "Always save buffer" yes)
                 (const :tag "Always do not save buffer" no)
                 (const :tag "Ask" ask))
  :group 'ein)

(defcustom ein:connect-aotoexec-lighter nil
  "String appended to the lighter of `ein:connect-mode' (`ein:c')
when auto-execution mode is on.  When `nil', use the same string
as `ein:cell-autoexec-prompt'."
  :type '(choice (string :tag "String appended to ein:c" "@")
                 (const :tag "Use `ein:cell-autoexec-prompt'." nil))
  :group 'ein)

(defcustom ein:connect-default-notebook nil
  "Notebook to be connect when `ein:connect-to-default-notebook' is called.

Example setting to connect to \"My_Notebook\" in the server at
port 8888 when opening any buffer in `python-mode'::

  (setq ein:connect-default-notebook \"8888/My_Notebook\")
  (add-hook 'python-mode-hook 'ein:connect-to-default-notebook)

`ein:connect-default-notebook' can also be a function without any
argument.  This function must return a string (notebook path of
the form \"URL-OR-PORT/NOTEBOOK-NAME\").

As `ein:connect-to-default-notebook' requires notebook list to be
loaded, consider using `ein:notebooklist-load' to load notebook
list if you want to connect to notebook without manually opening
notebook list."
  :type '(choice (string :tag "URL-OR-PORT/NOTEBOOK-NAME")
                 (function :tag "Notebook path getter"))
  :group 'ein)


;;; Class

(ein:deflocal ein:%connect% nil
  "Buffer local variable to store an instance of `ein:connect'")
(define-obsolete-variable-alias 'ein:@connect 'ein:%connect% "0.1.2")

(defclass ein:connect ()
  ((notebook :initarg :notebook :type ein:$notebook)
   (buffer :initarg :buffer :type buffer)
   (autoexec :initarg :autoexec :initform nil :type boolean
             :document "Auto-execution mode flag.

See also the document of the `autoexec' slot of `ein:codecell'
class.")))

(defun ein:connect-setup (notebook buffer)
  (with-current-buffer buffer
    (setq ein:%connect%
          (ein:connect :notebook notebook :buffer buffer))
    ein:%connect%))


;;; Methods

;; FIXME: Clarify names of these `connect-to-*' functions:

;;;###autoload
(defun ein:connect-to-notebook-command (&optional not-yet-opened)
  "Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks."
  (interactive "P")
  (call-interactively (if not-yet-opened
                          #'ein:connect-to-notebook
                        #'ein:connect-to-notebook-buffer)))

;;;###autoload
(defun ein:connect-to-notebook (nbpath &optional buffer no-reconnection)
  "Connect any buffer to notebook and its kernel."
  (interactive
   (list
    (completing-read
     "Notebook to connect [URL-OR-PORT/NAME]: "
     (ein:notebooklist-list-notebooks))))
  (ein:notebooklist-open-notebook-global
   nbpath
   (lambda (notebook -ignore- buffer no-reconnection)
     (ein:connect-buffer-to-notebook notebook buffer no-reconnection))
   (list (or buffer (current-buffer)) no-reconnection)))

;;;###autoload
(defun ein:connect-to-notebook-buffer (buffer-or-name)
  "Connect any buffer to opened notebook and its kernel."
  (interactive (list (completing-read "Notebook buffer to connect: "
                                      (ein:notebook-opened-buffer-names))))
  (let ((notebook
         (buffer-local-value 'ein:%notebook% (get-buffer buffer-or-name))))
    (ein:connect-buffer-to-notebook notebook)))

;;;###autoload
(defun ein:connect-buffer-to-notebook (notebook &optional buffer
                                                no-reconnection)
  "Connect BUFFER to NOTEBOOK."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (if (or (not no-reconnection)
            (not ein:%connect%))
        (let ((connection (ein:connect-setup notebook buffer)))
          (when (ein:eval-if-bound 'ac-sources)
            (push 'ac-source-ein-async ac-sources))
          (ein:connect-mode)
          (ein:log 'info "Connected to %s"
                   (ein:$notebook-notebook-name notebook))
          connection)
      (ein:log 'info "Buffer is already connected to notebook."))))

(defun ein:connect-get-notebook ()
  (oref ein:%connect% :notebook))

(defun ein:connect-get-kernel ()
  (ein:$notebook-kernel (ein:connect-get-notebook)))

(defun ein:connect-eval-buffer ()
  "Evaluate the whole buffer.  Note that this will run the code
inside the ``if __name__ == \"__main__\":`` block."
  (interactive)
  (ein:shared-output-eval-string (buffer-string) nil nil nil :silent t)
  (ein:connect-execute-autoexec-cells)
  (ein:log 'info "Whole buffer is sent to the kernel."))

(defun ein:connect-run-buffer (&optional ask-command)
  "Run buffer using ``%run``.  Ask for command if the prefix ``C-u`` is given.
Variable `ein:connect-run-command' sets the default command."
  (interactive "P")
  (ein:aif (ein:aand (ein:get-url-or-port)
                     (ein:filename-to-python it (buffer-file-name)))
      (let* ((default-command (ein:connect-run-command-get))
             (command (if ask-command
                          (read-from-minibuffer "Command: " default-command)
                        default-command))
             (cmd (format "%s %s" command it)))
        (if (ein:maybe-save-buffer ein:connect-save-before-run)
            (progn
              (ein:shared-output-eval-string cmd nil nil nil :silent t)
              (ein:connect-execute-autoexec-cells)
              (ein:log 'info "Command sent to the kernel: %s" cmd))
          (ein:log 'info "Buffer must be saved before %%run.")))
    (error (concat "This buffer has no associated file.  "
                   "Use `ein:connect-eval-buffer' instead."))))

(defun ein:connect-run-or-eval-buffer (&optional eval)
  "Run buffer using the ``%run`` magic command or eval whole
buffer if the prefix ``C-u`` is given.
Variable `ein:connect-run-command' sets the command to run.
You can change the command and/or set the options.
See also: `ein:connect-run-buffer', `ein:connect-eval-buffer'."
  (interactive "P")
  (if eval
      (ein:connect-eval-buffer)
    (ein:connect-run-buffer)))

(defun ein:connect-reload-buffer ()
  "Reload buffer using the command set by `ein:connect-reload-command'."
  (interactive)
  (let ((ein:connect-run-command ein:connect-reload-command))
    (call-interactively #'ein:connect-run-buffer)))

(defun ein:connect-eval-region (start end)
  (interactive "r")
  (ein:shared-output-eval-string (buffer-substring start end))
  (ein:log 'info "Selected region is sent to the kernel."))

(define-obsolete-function-alias
  'ein:connect-eval-string-internal
  'ein:shared-output-eval-string "0.1.2")

(define-obsolete-function-alias
  'ein:connect-request-tool-tip-or-help-command
  'ein:pytools-request-tooltip-or-help "0.1.2")

(defun ein:connect-pop-to-notebook ()
  (interactive)
  (ein:connect-assert-connected)
  (pop-to-buffer (ein:notebook-buffer (ein:connect-get-notebook))))


;;; Generic getter

(defun ein:get-url-or-port--connect ()
  (ein:aand (ein:get-notebook--connect) (ein:$notebook-url-or-port it)))

(defun ein:get-notebook--connect ()
  (when (ein:connect-p ein:%connect%)
    (oref ein:%connect% :notebook)))

(defun ein:get-kernel--connect ()
  (ein:aand (ein:get-notebook--connect) (ein:$notebook-kernel it)))

(defun ein:get-traceback-data--connect ()
  ;; FIXME: Check if the TB in shared-output buffer is originated from
  ;;        the current buffer.
  (ein:aand (ein:shared-output-get-cell) (ein:cell-get-tb-data it)))
(autoload 'ein:shared-output-get-cell "ein-shared-output") ; FIXME: Remove!


;;; Auto-execution

(defun ein:connect-assert-connected ()
  (assert (ein:connect-p ein:%connect%) nil
          "Current buffer (%s) is not connected to IPython notebook."
          (buffer-name))
  (assert (ein:notebook-live-p (oref ein:%connect% :notebook)) nil
          "Connected notebook is not live (probably already closed)."))

(defun ein:connect-execute-autoexec-cells ()
  "Call `ein:notebook-execute-autoexec-cells' via `after-save-hook'."
  (ein:connect-assert-connected)
  (when (oref ein:%connect% :autoexec)
    (ein:notebook-execute-autoexec-cells (ein:connect-get-notebook))))

(defun ein:connect-toggle-autoexec ()
  "Toggle auto-execution mode of the current connected buffer.

When auto-execution mode is on, cells in connected notebook will
be automatically executed whenever run, eval or reload command [#]_
is called in this buffer.

.. [#] Namely, one of

   * `ein:connect-run-buffer'
   * `ein:connect-eval-buffer'
   * `ein:connect-run-or-eval-buffer'
   * `ein:connect-reload-buffer'

Note that you need to set cells to run in the connecting buffer
or no cell will be executed.
Use the `ein:worksheet-turn-on-autoexec' command in notebook to
change the cells to run."
  (interactive)
  (ein:connect-assert-connected)
  (let ((autoexec-p (not (oref ein:%connect% :autoexec))))
    (oset ein:%connect% :autoexec autoexec-p)
    (ein:log 'info "Auto-execution mode is %s."
             (if autoexec-p "enabled" "disabled"))))


;;; Auto-connect

;;;###autoload
(defun ein:connect-to-default-notebook ()
  "Connect to the default notebook specified by
`ein:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook."
  (ein:log 'verbose "CONNECT-TO-DEFAULT-NOTEBOOK")
  (ein:and-let* ((nbpath ein:connect-default-notebook)
                 ((not (ein:worksheet-buffer-p))))
    (when (functionp nbpath)
      (setq nbpath (funcall nbpath)))
    (ein:connect-to-notebook nbpath nil t)))



;;; ein:connect-mode

(defvar ein:connect-mode-map (make-sparse-keymap))

(let ((map ein:connect-mode-map))
  (define-key map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
  (define-key map "\C-c\C-l" 'ein:connect-reload-buffer)
  (define-key map "\C-c\C-r" 'ein:connect-eval-region)
  (define-key map (kbd "C-:") 'ein:shared-output-eval-string)
  (define-key map "\C-c\C-f" 'ein:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ein:completer-complete)
  (define-key map "\C-c\C-z" 'ein:connect-pop-to-notebook)
  (define-key map "\C-c\C-a" 'ein:connect-toggle-autoexec)
  (define-key map "\C-c\C-o" 'ein:console-open)
  (define-key map "\C-c\C-x" 'ein:tb-show)
  (define-key map "\M-."          'ein:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein:pytools-jump-back-command)
  (define-key map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)
  map)

(defun ein:connect-mode-get-lighter ()
  (if (oref ein:%connect% :autoexec)
      (format " ein:c%s" (or ein:connect-aotoexec-lighter
                             ein:cell-autoexec-prompt))
    " ein:c"))

(define-minor-mode ein:connect-mode
  "Minor mode for communicating with IPython notebook.

\\{ein:connect-mode-map}"
  :lighter (:eval (ein:connect-mode-get-lighter))
  :keymap ein:connect-mode-map
  :group 'ein
  (ein:complete-on-dot-install ein:connect-mode-map))

(put 'ein:connect-mode 'permanent-local t)


(provide 'ein-connect)

;;; ein-connect.el ends here
