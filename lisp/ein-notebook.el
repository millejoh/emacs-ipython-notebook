;;; ein-notebook.el --- Notebook module   -*- lexical-binding: t -*-

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notebook.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notebook.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebook.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Coding rule about current buffer.
;; A lot of notebook and cell functions touches to current buffer and
;; it is not ideal to wrap all these functions by `with-current-buffer'.
;; Therefore, when the function takes `notebook' to the first argument
;; ("method" function), it is always assumed that the current buffer
;; is the notebook buffer.  **However**, functions called as callback
;; (via `url-retrieve', for example) must protect themselves by
;; calling from unknown buffer.

;;; Code:


(eval-when-compile (require 'auto-complete))

(require 'ewoc)
(require 'mumamo nil t)
(require 'company nil t)
(require 'px nil t)
(require 'eldoc nil t)
(require 'ein-core)
(require 'ein-classes)
(require 'ein-console)
(require 'ein-log)
(require 'ein-node)
(require 'ein-contents-api)
(require 'ein-kernel)
(require 'ein-kernelinfo)
(require 'ein-cell)
(require 'ein-cell-edit)
(require 'ein-cell-output)
(require 'ein-worksheet)
(require 'ein-iexec)
(require 'ein-scratchsheet)
(require 'ein-notification)
(require 'ein-completer)
(require 'ein-pager)
(require 'ein-pseudo-console)
(require 'ein-events)
(require 'ein-notification)
(require 'ein-kill-ring)
(require 'ein-query)
(require 'ein-pytools)
(require 'ein-traceback)
(require 'ein-inspector)
(require 'ein-shared-output)
(require 'ein-notebooklist)
(require 'ein-multilang)
(require 'ob-ein)
(require 'poly-ein)

;;; Configuration

(make-obsolete-variable 'ein:notebook-discard-output-on-save nil "0.2.0")

(declare-function ein:smartrep-config "ein-smartrep")

(defcustom ein:use-smartrep nil
  "Set to `t' to use preset smartrep configuration.

.. warning:: When used with MuMaMo (see `ein:notebook-modes'),
   keyboard macro which manipulates cell (add, remove, move,
   etc.) may start infinite loop (you need to stop it with
   ``C-g``).  Please be careful using this option if you are a
   heavy keyboard macro user.  Using keyboard macro for other
   commands is fine.

.. (Comment) I guess this infinite loop happens because the three
   modules (kmacro.el, mumamo.el and smartrep.el) touches to
   `unread-command-events' in somehow inconsistent ways."
  :type 'boolean
  :group 'ein)

(defvar *ein:notebook--pending-query* (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT . PATH) => t/nil")

(defcustom ein:notebook-autosave-frequency 300
  "Sets the frequency (in seconds) at which the notebook is
automatically saved, per IPEP15. Set to 0 to disable this feature.

Autosaves are automatically enabled when a notebook is opened,
but can be controlled manually via `ein:notebook-enable-autosave'
and `ein:notebook-disable-autosave'.

If you wish to change the autosave frequency for the current
notebook call `ein:notebook-update-autosave-freqency'.

"
  :type 'number
  :group 'ein)

(defcustom ein:notebook-create-checkpoint-on-save t
  "If non-nil a checkpoint will be created every time the
notebook is saved. Otherwise checkpoints must be created manually
via `ein:notebook-create-checkpoint'."
  :type 'boolean
  :group 'ein)

(defcustom ein:notebook-discard-output-on-save 'no
  "Configure if the output part of the cell should be saved or not.

.. warning:: This configuration is obsolete now.
   Use nbconvert (https://github.com/ipython/nbconvert) to
   strip output.

`no' : symbol
    Save output. This is the default.
`yes' : symbol
    Always discard output.
a function
    This function takes two arguments, notebook and cell.  Return
    `t' to discard output and return `nil' to save.  For example,
    if you don't want to save image output but other kind of
    output, use `ein:notebook-cell-has-image-output-p'.
"
  :type '(choice (const :tag "No" 'no)
                 (const :tag "Yes" 'yes)
                 )
  :group 'ein)

(defun ein:notebook-cell-has-image-output-p (_ignore cell)
  (ein:cell-has-image-ouput-p cell))

(defun ein:notebook-discard-output-p (notebook cell)
  "Return non-`nil' if the output must be discarded, otherwise save."
  (cl-case ein:notebook-discard-output-on-save
    (no nil)
    (yes t)
    (t (funcall ein:notebook-discard-output-on-save notebook cell))))

;; As opening/saving notebook treats possibly huge data, define these
;; timeouts separately:

(defcustom ein:notebook-querty-timeout-save (* 60 1000) ; 1 min
  "Query timeout for saving notebook.
Similar to `ein:notebook-querty-timeout-open', but for saving
notebook.  For global setting and more information, see
`ein:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defcustom ein:helm-kernel-history-search-key nil
  "Bind `helm-ein-kernel-history' to this key in notebook mode.

Example::

    (setq ein:helm-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ein:notebook-mode-map'."
  :type 'boolean
  :group 'ein)

(defcustom ein:anything-kernel-history-search-key nil
  "Bind `anything-ein-kernel-history' to this key in notebook mode.

Example::

    (setq ein:anything-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ein:notebook-mode-map'."
  :type 'boolean
  :group 'ein)

(defcustom ein:notebook-set-buffer-file-name nil
  "[DEPRECATED] Set `buffer-file-name' of notebook buffer. Currently does nothing."
  :type 'boolean
  :group 'ein)

(defvar ein:notebook-after-rename-hook nil
  "Hooks to run after notebook is renamed successfully.
Current buffer for these functions is set to the notebook buffer.")


;;; Class and variable

(defvar ein:base-kernel-url "/api/")
(defvar ein:create-session-url "/api/sessions")
;; Currently there is no way to know this setting.  Maybe I should ask IPython
;; developers for an API to get this from notebook server.
;;
;; 10April2014 (JMM) - The most recent documentation for the RESTful interface
;; is at:
;; https://github.com/ipython/ipython/wiki/IPEP-16%3A-Notebook-multi-directory-dashboard-and-URL-mapping


(defvar ein:notebook-pager-buffer-name-template "*ein:pager %s/%s*")
(defvar ein:notebook-buffer-name-template "*ein: %s/%s*")

(ein:deflocal ein:%notebook% nil
  "Buffer local variable to store an instance of `ein:$notebook'.")

(ein:deflocal ein:%notebook-latex-p% nil
  "Is latex preview toggled")

(define-obsolete-variable-alias 'ein:notebook 'ein:%notebook% "0.1.2")

;;; Constructor

(defun ein:notebook-new (url-or-port notebook-path pre-kernelspec &rest args)
  (let ((kernelspec
         (cond ((ein:$kernelspec-p pre-kernelspec) pre-kernelspec)
               ((consp pre-kernelspec)
                (cl-loop for (_name ks) on (ein:need-kernelspecs url-or-port) by 'cddr
                  when (and (ein:$kernelspec-p ks)
                            (string= (cdr pre-kernelspec)
                                     (cl-struct-slot-value
                                      'ein:$kernelspec (car pre-kernelspec) ks)))
                  return ks))
               (t (ein:get-kernelspec url-or-port pre-kernelspec)))))
    (apply #'make-ein:$notebook
           :url-or-port url-or-port
           :kernelspec kernelspec
           :notebook-path notebook-path
           args)))

;;; Destructor

(defun ein:notebook-del (notebook)
  "Destructor for `ein:$notebook'."
  (ein:log-ignore-errors
    (ein:kernel-del (ein:$notebook-kernel notebook))))

(defun ein:notebook-close-worksheet (notebook ws)
  "Close worksheet WS in NOTEBOOK.  If WS is the last worksheet,
call notebook destructor `ein:notebook-del'."
  (cl-symbol-macrolet ((worksheets (ein:$notebook-worksheets notebook))
                       (scratchsheets (ein:$notebook-scratchsheets notebook)))
    (cond
     ((ein:worksheet-p ws) (ein:worksheet-save-cells ws t))
     (t (setq scratchsheets (delq ws scratchsheets))))
    (unless (or (seq-filter (lambda (x)
                              (and (not (eq x ws))
                                   (ein:worksheet-has-buffer-p x)))
                            worksheets)
                scratchsheets)
      (ein:notebook-del notebook))))


;;; Notebook utility functions

(defun ein:notebook-update-url-or-port (new-url-or-port notebook)
  "Change the url and port the notebook is saved to. Calling
this will propagate the change to the kernel, trying to restart
the kernel in the process. Use case for this command is when
the jupyter server dies and restarted on a different port.

If you have enabled token or password security on server running
at the new url/port, then please be aware that this new url-port
combo must match exactly these url/port you used format
`ein:notebooklist-login'."
  (interactive (list
                (ein:notebooklist-ask-url-or-port)
                (ein:notebook--get-nb-or-error)))
  (message "Updating server info and restarting kernel for notebooklist %s"
           (ein:$notebook-notebook-name notebook))
  (setf (ein:$notebook-url-or-port notebook) new-url-or-port)
  (with-current-buffer (ein:notebook-buffer notebook)
    (ein:kernel-retrieve-session (ein:$notebook-kernel notebook))
    (rename-buffer (format ein:notebook-buffer-name-template
                           (ein:$notebook-url-or-port notebook)
                           (ein:$notebook-notebook-name notebook)))))

(defun ein:notebook-buffer (notebook)
  "Return first buffer in NOTEBOOK's worksheets."
  (cl-loop for ws in (append (ein:$notebook-worksheets notebook)
                             (ein:$notebook-scratchsheets notebook))
    if (ein:worksheet-buffer ws)
    return it))

(defun ein:notebook-buffer-list (notebook)
  "Return the buffers associated with NOTEBOOK's kernel.
The buffer local variable `default-directory' of these buffers
will be updated with kernel's cwd."
  (delete nil
          (mapcar #'ein:worksheet-buffer
                  (append (ein:$notebook-worksheets notebook)
                          (ein:$notebook-scratchsheets notebook)))))

(defun ein:notebook--get-nb-or-error ()
  (or ein:%notebook% (error "Not in notebook buffer.")))

;;;###autoload
(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

(defun ein:notebook-name-getter (notebook)
  (cons #'ein:notebook-name notebook))

;;; Open notebook

(defun ein:notebook-url (notebook)
  (ein:notebook-url-from-url-and-id (ein:$notebook-url-or-port notebook)
                                    (ein:$notebook-api-version notebook)
                                    (ein:$notebook-notebook-path notebook)))

(defun ein:notebook-url-from-url-and-id (url-or-port api-version path)
  (cond ((= api-version 2)
         (ein:url url-or-port "api/notebooks" path))
        ((>= api-version 3)
         (ein:url url-or-port "api/contents" path))))

(defun ein:notebook-open--decorate-callback (notebook existing pending-clear callback no-pop)
  "In addition to CALLBACK, also clear the pending semaphore, pop-to-buffer the new notebook, save to disk the kernelspec metadata, and put last warning in minibuffer."
  (apply-partially
   (lambda (notebook* created callback* pending-clear* no-pop*)
     (funcall pending-clear*)
     (with-current-buffer (ein:notebook-buffer notebook*)
       (ein:worksheet-focus-cell))
     (unless no-pop*
       (with-current-buffer (ein:notebook-buffer notebook*)
         (if ein:polymode
             (progn
               (pm-select-buffer (pm-innermost-span))
               (pop-to-buffer (pm-span-buffer (pm-innermost-span))))
           (pop-to-buffer (ein:notebook-buffer notebook*)))))
     (when (and (not noninteractive)
                (null (plist-member (ein:$notebook-metadata notebook*) :kernelspec)))
       (ein:aif (ein:$notebook-kernelspec notebook*)
           (progn
             (setf (ein:$notebook-metadata notebook*)
                   (plist-put (ein:$notebook-metadata notebook*)
                              :kernelspec (ein:notebook--spec-insert-name
                                           (ein:$kernelspec-name it) (ein:$kernelspec-spec it))))
             (ein:notebook-save-notebook notebook*))))
     (when callback*
       (funcall callback* notebook* created))
     (ein:and-let* ((created)
                    (buffer (get-buffer "*Warnings*"))
                    (last-warning (with-current-buffer buffer
                                    (thing-at-point 'line t))))
       (message "%s" last-warning)))
   notebook (not existing) callback pending-clear no-pop))

(defun ein:notebook-open-or-create (url-or-port path &optional kernelspec callback no-pop)
  "Same as `ein:notebook-open' but create PATH if not found."
  (let ((if-not-found (lambda (_contents _status-code) )))
    (ein:notebook-open url-or-port path kernelspec callback if-not-found no-pop)))

;;;###autoload
(defun ein:notebook-jump-to-opened-notebook (notebook)
  "List all opened notebook buffers and switch to one that the user selects."
  (interactive
   (list  (completing-read "Jump to notebook:" (ein:notebook-opened-buffer-names) nil t)))
  (switch-to-buffer notebook))

;;;###autoload
(defun ein:notebook-open (url-or-port path &optional kernelspec callback errback no-pop)
  "Returns notebook at URL-OR-PORT/PATH.

Note that notebook sends for its contents and won't have them right away.

After the notebook is opened, CALLBACK is called as::

  \(funcall CALLBACK notebook created)

where `created' indicates a new notebook or an existing one.
"
  (interactive
   (ein:notebooklist-parse-nbpath (ein:notebooklist-ask-path "notebook")))
  (unless errback (setq errback #'ignore))
  (let* ((pending-key (cons url-or-port path))
         (pending-p (gethash pending-key *ein:notebook--pending-query*))
         (pending-clear (apply-partially (lambda (pending-key* &rest _args)
                                           (remhash pending-key*
                                                    *ein:notebook--pending-query*))
                                         pending-key))
         (existing (ein:notebook-get-opened-notebook url-or-port path))
         (notebook (ein:aif existing it
                     (ein:notebook-new url-or-port path kernelspec)))
         (callback0 (ein:notebook-open--decorate-callback notebook existing pending-clear
                                                          callback no-pop)))
    (if existing
        (progn
          (ein:log 'info "Notebook %s is already open"
                   (ein:$notebook-notebook-name notebook))
          (funcall callback0))
      (if (and pending-p noninteractive)
          (ein:log 'error "Notebook %s pending open!" path)
        (when (or (not pending-p)
                  (y-or-n-p (format "Notebook %s pending open!  Retry? " path)))
          (setf (gethash pending-key *ein:notebook--pending-query*) t)
          (add-function :before (var errback) pending-clear)
          (ein:content-query-contents url-or-port path
                                      (apply-partially #'ein:notebook-open--callback
                                                       notebook callback0 (not no-pop))
                                      errback))))
    notebook))

(defun ein:notebook-open--callback (notebook callback0 q-checkpoints content)
  (ein:log 'verbose "Opened notebook %s" (ein:$notebook-notebook-path notebook))
  (let ((_notebook-path (ein:$notebook-notebook-path notebook)))
    (ein:gc-prepare-operation)
    (setf (ein:$notebook-api-version notebook) (ein:$content-notebook-version content)
          (ein:$notebook-notebook-name notebook) (ein:$content-name content))
    (ein:notebook-bind-events notebook (ein:events-new))
    (ein:notebook-maybe-set-kernelspec notebook (plist-get (ein:$content-raw-content content) :metadata))
    (ein:notebook-install-kernel notebook)
    (ein:notebook-from-json notebook (ein:$content-raw-content content))
    (if (not (with-current-buffer (ein:notebook-buffer notebook)
               (ein:get-notebook)))
        (error "ein:notebook-open--callback: notebook instantiation failed")
      ;; Start websocket only after worksheet is rendered
      ;; because ein:notification-bind-events only gets called after worksheet's
      ;; buffer local notification widget is instantiated
      (ein:kernel-retrieve-session (ein:$notebook-kernel notebook) nil
                                   (apply-partially (lambda (callback0* name* _kernel)
                                                      (funcall callback0*)
                                                      (ein:log 'info "Notebook %s is ready" name*))
                                                    callback0
                                                    (ein:$notebook-notebook-name notebook)))
      (setf (ein:$notebook-kernelinfo notebook)
            (ein:kernelinfo-new (ein:$notebook-kernel notebook)
                                (cons #'ein:notebook-buffer-list notebook)
                                (symbol-name (ein:get-mode-for-kernel (ein:$notebook-kernelspec notebook)))))
      (ein:notebook-put-opened-notebook notebook)
      (ein:notebook--check-nbformat (ein:$content-raw-content content))
      (setf (ein:$notebook-q-checkpoints notebook) q-checkpoints)
      (ein:notebook-enable-autosaves notebook)
      (ein:gc-complete-operation))))

(defun ein:notebook-maybe-set-kernelspec (notebook content-metadata)
  (ein:aif (plist-get content-metadata :kernelspec)
      (let ((kernelspec (ein:get-kernelspec (ein:$notebook-url-or-port notebook)
                                            (plist-get it :name))))
        (setf (ein:$notebook-kernelspec notebook) kernelspec))))


(defun ein:notebook--different-number (n1 n2)
  (and (numberp n1) (numberp n2) (not (= n1 n2))))

(defun ein:notebook--check-nbformat (data)
  "Warn user when nbformat is changed on server side.
See https://github.com/ipython/ipython/pull/1934 for the purpose
of minor mode."
  ;; See `Notebook.prototype.load_notebook_success'
  ;; at IPython/frontend/html/notebook/static/js/notebook.js
  (cl-destructuring-bind (&key nbformat orig_nbformat
                               nbformat_minor orig_nbformat_minor
                               &allow-other-keys)
      data
    (cond
     ((ein:notebook--different-number nbformat orig_nbformat)
      (ein:display-warning
       (format "Notebook major version updated (v%d -> v%d).
  To not update version, do not save this notebook."
               orig_nbformat nbformat)))
     ((ein:notebook--different-number nbformat_minor orig_nbformat_minor)
      (ein:display-warning
       (format "This notebook is version v%s.%s, but IPython
  server you are using only fully support up to v%s.%s.
  Some features may not be available."
               orig_nbformat orig_nbformat_minor
               nbformat nbformat_minor))))))


;;; Initialization.

(defun ein:notebook-enable-autosaves (notebook)
  "Enable automatic, periodic saving for notebook."
  (interactive
   (list (or (ein:get-notebook)
             (ein:aand (ein:notebook-opened-buffer-names)
                       (with-current-buffer (ein:completing-read
                                             "Notebook: " it nil t)
                         (ein:get-notebook))))))
  (when (and (ein:$notebook-q-checkpoints notebook)
             (> ein:notebook-autosave-frequency 0))
      (setf (ein:$notebook-autosave-timer notebook)
            (run-at-time ein:notebook-autosave-frequency
                         ein:notebook-autosave-frequency
                         #'ein:notebook-maybe-save-notebook
                         notebook))
      (ein:log 'verbose "Enabling autosaves for %s with frequency %s seconds."
               (ein:$notebook-notebook-name notebook)
               ein:notebook-autosave-frequency)))

(defun ein:notebook-disable-autosaves (notebook)
  "Disable automatic, periodic saving for current notebook."
  (interactive
   (list (or (ein:get-notebook)
             (ein:aand (ein:notebook-opened-buffer-names)
                       (with-current-buffer (ein:completing-read
                                             "Notebook: " it nil t)
                         (ein:get-notebook))))))
  (if (and notebook (ein:$notebook-autosave-timer notebook))
      (progn
        (ein:log 'verbose "Disabling auto checkpoints for notebook %s" (ein:$notebook-notebook-name notebook))
        (cancel-timer (ein:$notebook-autosave-timer notebook)))))

(defun ein:notebook-update-autosave-frequency (new-frequency notebook)
  "Change the autosaves frequency for the current notebook, or
for a notebook selected by the user if not currently inside a
notebook buffer."
  (interactive
   (list (read-number "New autosaves frequency (0 to disable): ")
         (or (ein:get-notebook)
             (ein:aand (ein:notebook-opened-buffer-names)
                       (with-current-buffer (ein:completing-read
                                             "Notebook: " it nil t)
                         (ein:get-notebook))))))
  (if notebook
      (progn
        (setq ein:notebook-autosave-frequency new-frequency)
        (ein:notebook-disable-autosaves notebook)
        (ein:notebook-enable-autosaves notebook))
    (message "Open notebook first")))

(defun ein:notebook-bind-events (notebook events)
  "Bind events related to PAGER to the event handler EVENTS."
  (setf (ein:$notebook-events notebook) events)
  (ein:worksheet-class-bind-events events)
  ;; Bind events for sub components:
  (setf (ein:$notebook-pager notebook)
        (ein:pager-new
         (format ein:notebook-pager-buffer-name-template
                 (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-name notebook))
         (ein:$notebook-events notebook))))

(defalias 'ein:notebook-reconnect-kernel 'ein:notebook-reconnect-session-command "The distinction between kernel and session is a bit mysterious, all the action is now occurring in `ein:notebook-reconnect-session-command' these days, for which this function is now an alias.")

(define-obsolete-function-alias
  'ein:notebook-show-in-shared-output
  'ein:shared-output-show-code-cell-at-point "0.1.2")

(eval-when-compile (defvar outline-regexp))
(declare-function px-preview "px" ())
(defsubst ein:notebook-toggle-latex-fragment ()
  (interactive)
  (cond (ein:polymode (ein:display-warning "ein:notebook-toggle-latex-fragment: delegate to markdown-mode"))
        ((featurep 'px)
         (let ((outline-regexp "$a")
               (kill-buffer-hook nil)) ;; outline-regexp never matches to avoid headline
           (cl-letf (((symbol-function 'px-remove) #'ignore))
             (if ein:%notebook-latex-p%
                 (progn
                   (ein:worksheet-render (ein:worksheet--get-ws-or-error))
                   (setq ein:%notebook-latex-p% nil))
               (px-preview)
               (setq ein:%notebook-latex-p% t)))))
        (t (ein:display-warning "px package not found"))))

;;; Kernel related things

(defun ein:list-available-kernels (url-or-port)
  (let ((kernelspecs (ein:need-kernelspecs url-or-port)))
    (if kernelspecs
        (cl-sort (cl-loop for (_key spec) on (ein:plist-exclude kernelspecs '(:default)) by 'cddr
                   collecting (cons (ein:$kernelspec-name spec)
                                    (ein:$kernelspec-display-name spec)))
                 #'string< :key #'cdr))))

(defun ein:notebook-switch-kernel (notebook kernel-name)
  "Change the kernel for a running notebook. If not called from a
notebook buffer then the user will be prompted to select an opened notebook."
  (interactive
   (let* ((notebook (or (ein:get-notebook)
                        (ein:completing-read
                         "Select notebook: "
                         (ein:notebook-opened-buffer-names))))
          (kernel-name (ein:completing-read
                        "Select kernel: "
                        (ein:list-available-kernels (ein:$notebook-url-or-port notebook)))))
     (list notebook kernel-name)))
  (let* ((kernelspec (ein:get-kernelspec
                      (ein:$notebook-url-or-port notebook) kernel-name)))
    (setf (ein:$notebook-kernelspec notebook) kernelspec)
    (setf (ein:$notebook-metadata notebook)
          (plist-put (ein:$notebook-metadata notebook)
                     :kernelspec (ein:notebook--spec-insert-name
                                  (ein:$kernelspec-name kernelspec) (ein:$kernelspec-spec kernelspec))))
    (ein:notebook-save-notebook notebook #'ein:notebook-kill-kernel-then-close-command
                                (list notebook))
    (cl-loop repeat 10
      until (null (ein:$kernel-websocket (ein:$notebook-kernel notebook)))
      do (sleep-for 0 500)
      finally return (ein:notebook-open (ein:$notebook-url-or-port notebook)
                                        (ein:$notebook-notebook-path notebook)))))

(defun ein:notebook-install-kernel (notebook)
  (let* ((base-url (concat ein:base-kernel-url "kernels"))
         (kernelspec (ein:$notebook-kernelspec notebook))
         (kernel (ein:kernel-new (ein:$notebook-url-or-port notebook)
                                 (ein:$notebook-notebook-path notebook)
                                 kernelspec
                                 base-url
                                 (ein:$notebook-events notebook)
                                 (ein:$notebook-api-version notebook))))
    (setf (ein:$notebook-kernel notebook) kernel)
    (when (eq (ein:get-mode-for-kernel (ein:$notebook-kernelspec notebook)) 'python)
      (ein:pytools-setup-hooks kernel notebook))))

(defun ein:notebook-reconnect-session-command ()
   "It seems convenient but undisciplined to blithely create a new session if the original one no longer exists."
   (interactive)
   (ein:kernel-reconnect-session (ein:$notebook-kernel ein:%notebook%)))

(defun ein:notebook-restart-session-command ()
   "Delete session on server side.  Start new session."
  (interactive)
  (ein:aif ein:%notebook%
      (when (y-or-n-p "Are you sure you want to restart this session? ")
        (ein:kernel-restart-session (ein:$notebook-kernel it)))
    (message "Not in notebook buffer!")))

(define-obsolete-function-alias
  'ein:notebook-request-tool-tip-or-help-command
  'ein:pytools-request-tooltip-or-help "0.1.2")

(defun ein:notebook-ac-dot-complete ()
  "Insert dot and request completion."
  (interactive)
  (if (and (ein:get-notebook)
           (ein:codecell-p (ein:get-cell-at-point)))
      (call-interactively #'ein:ac-dot-complete)
    (insert ".")))

(defun ein:notebook-kernel-interrupt-command ()
  "Interrupt the kernel.
This is equivalent to do ``C-c`` in the console program."
  (interactive)
  (ein:kernel-interrupt (ein:$notebook-kernel ein:%notebook%)))

;; autoexec

(defun ein:notebook-execute-autoexec-cells (notebook)
  "Execute cells of which auto-execution flag is on."
  (interactive (list (or ein:%notebook% (error "Not in notebook buffer!"))))
  (mapc #'ein:worksheet-execute-autoexec-cells
        (ein:$notebook-worksheets notebook)))

(define-obsolete-function-alias
  'ein:notebook-eval-string
  'ein:shared-output-eval-string "0.1.2")


;;; Persistence and loading

(defun ein:notebook-set-notebook-name (notebook name)
  "Check NAME and change the name of NOTEBOOK to it."
  (if (ein:notebook-test-notebook-name name)
      (setf (ein:$notebook-notebook-name notebook) name
            (ein:$notebook-notebook-id notebook) name)
    (ein:log 'error "%S is not a good notebook name." name)
    (error "%S is not a good notebook name." name)))

(defun ein:notebook-test-notebook-name (name)
  (and (stringp name)
       (> (length name) 0)
       (not (string-match "[\\/\\\\:]" name))))

(cl-defun ein:notebook--worksheet-new (notebook &optional (func #'ein:worksheet-new))
  (funcall func
           (ein:$notebook-nbformat notebook)
           (ein:notebook-name-getter notebook)
           (cons (lambda (notebook cell)
                   (ein:notebook-discard-output-p notebook cell))
                 notebook)
           (ein:$notebook-kernel notebook)
           (ein:$notebook-events notebook)))

(defun ein:notebook--worksheet-render (notebook ws)
  (ein:worksheet-render ws)
  (with-current-buffer (ein:worksheet-buffer ws)
    (let (multilang-failed)
      (if ein:polymode
          (poly-ein-mode)
        ;; Changing major mode here is super dangerous as it
        ;; kill-all-local-variables.
        ;; Our saviour has been `ein:deflocal' which applies 'permanent-local
        ;; to variables assigned up to this point, but we ought not rely on it
        (funcall (ein:notebook-choose-mode))
        (ein:worksheet-reinstall-undo-hooks ws)
        (condition-case err
            (ein:aif (ein:$notebook-kernelspec notebook)
                (ein:ml-lang-setup it))
          (error (ein:log 'error (error-message-string err))
                 (setq multilang-failed t))))
      (unless multilang-failed
        (ein:notebook-mode)
        (ein:notebook--notification-setup notebook)
        (ein:notebook-setup-kill-buffer-hook)
        (setq ein:%notebook% notebook)
        (when ein:polymode
          (poly-ein-fontify-buffer (ein:notebook-buffer notebook)))))))

(defun ein:notebook--notification-setup (notebook)
  (ein:notification-setup
   (current-buffer)
   (ein:$notebook-events notebook)
   :get-list
   (lambda () (ein:$notebook-worksheets ein:%notebook%))
   :get-current
   (lambda () ein:%worksheet%)
   :get-name
   #'ein:worksheet-name
   :get-buffer
   (lambda (ws)
     (ein:notebook-worksheet--render-maybe ein:%notebook% ws "clicked")
     (ein:worksheet-buffer ws))
   :delete
   (lambda (ws)
     (ein:notebook-worksheet-delete ein:%notebook% ws t))
   :insert-prev
   (lambda (ws) (ein:notebook-worksheet-insert-prev ein:%notebook% ws))
   :insert-next
   (lambda (ws) (ein:notebook-worksheet-insert-next ein:%notebook% ws))
   :move-prev
   (lambda (ws) (ein:notebook-worksheet-move-prev ein:%notebook% ws))
   :move-next
   (lambda (ws) (ein:notebook-worksheet-move-next ein:%notebook% ws))
   ))

(defun ein:notebook-set-buffer-file-name-maybe (_notebook)
  (ein:log 'warn "This function is deprecated. Who could be calling me?"))

;; (defun ein:notebook-set-buffer-file-name-maybe (notebook)
;;   "Set `buffer-file-name' of the current buffer to ipynb file
;; of NOTEBOOK."
;;   (when ein:notebook-set-buffer-file-name
;;     (ein:notebook-fetch-data
;;      notebook
;;      (lambda (data notebook buffer)
;;        (with-current-buffer buffer
;;          (cl-destructuring-bind (&key project &allow-other-keys)
;;              data
;;            (setq buffer-file-name
;;                  (expand-file-name
;;                   (format "%s.ipynb"
;;                           (ein:$notebook-notebook-name notebook))
;;                   project)))))
;;      (list notebook (current-buffer)))))

(defun ein:notebook-from-json (notebook data)
  (cl-destructuring-bind (&key metadata nbformat nbformat_minor
                               &allow-other-keys)
      data
    (setf (ein:$notebook-metadata notebook) metadata)
    (setf (ein:$notebook-nbformat notebook) nbformat)
    (setf (ein:$notebook-nbformat-minor notebook) nbformat_minor))
  (setf (ein:$notebook-worksheets notebook)
        (cl-case (ein:$notebook-nbformat notebook)
          (3 (ein:read-nbformat3-worksheets notebook data))
          (4 (ein:read-nbformat4-worksheets notebook data))
          (t (ein:log 'error "nbformat version %s unsupported"
                      (ein:$notebook-nbformat notebook)))))
  (ein:notebook--worksheet-render notebook (car (ein:$notebook-worksheets notebook)))
  notebook)

(defun ein:read-nbformat3-worksheets (notebook data)
  (mapcar (lambda (ws-data)
                    (ein:worksheet-from-json
                     (ein:notebook--worksheet-new notebook)
                     ws-data))
          (or (plist-get data :worksheets)
              (list nil))))

;; nbformat4 gets rid of the concept of worksheets. That means, for the moment,
;; ein will no longer support worksheets. There may be a path forward for
;; reimplementing this feature, however.  The nbformat 4 json definition says
;; that cells are allowed to have tags. Clever use of this feature may lead to
;; good things.

(defun ein:read-nbformat4-worksheets (notebook data)
  "Convert a notebook in nbformat4 to a list of worksheet-like
  objects suitable for processing in ein:notebook-from-json."
  (let* ((cells (plist-get data :cells))
         (ws-cells (mapcar (lambda (data) (ein:cell-from-json data)) cells))
         (worksheet (ein:notebook--worksheet-new notebook)))
    (oset worksheet :saved-cells ws-cells)
    ;(mapcar (lambda (data) (message "test %s" (slot-value data 'metadata))) ws-cells)
    (list worksheet)))

(defun ein:notebook-to-json (notebook)
  "Return json-ready alist."
  (let ((data
         (cl-case (ein:$notebook-nbformat notebook)
           (3 (ein:write-nbformat3-worksheets notebook))
           (4 (ein:write-nbformat4-worksheets notebook))
           (t (ein:log 'error "nbformat version %s unsupported"
                       (ein:$notebook-nbformat notebook))))))
    ;; Apparently metadata can be either a hashtable or a plist...
    (let ((metadata (cdr (assq 'metadata data))))
      (if (hash-table-p metadata)
          (setf (gethash 'name metadata) (ein:$notebook-notebook-name notebook))
        (plist-put metadata
                   :name (ein:$notebook-notebook-name notebook)))
      (ein:aif (ein:$notebook-nbformat-minor notebook)
          ;; Do not set nbformat when it is not given from server.
          (push `(nbformat_minor . ,it) data))
      (push `(nbformat . ,(ein:$notebook-nbformat notebook)) data)
      data)))


(defun ein:write-nbformat3-worksheets (notebook)
  (let ((worksheets (mapcar #'ein:worksheet-to-json
                            (ein:$notebook-worksheets notebook))))
    `((worksheets . ,(apply #'vector worksheets))
      (metadata . ,(ein:$notebook-metadata notebook))
      )))

(defsubst ein:notebook--spec-insert-name (name spec)
  "Add kernel NAME, e.g., 'python2', to the kernelspec member of ipynb metadata."
  (if (plist-member spec :name)
      spec
    (plist-put spec :name name)))

(defun ein:write-nbformat4-worksheets (notebook)
  (let ((all-cells (cl-loop for ws in (ein:$notebook-worksheets notebook)
                     for i from 0
                     append (ein:worksheet-to-nb4-json ws i))))
    ;; should be in notebook constructor, not here
    (ein:aif (ein:$notebook-kernelspec notebook)
        (setf (ein:$notebook-metadata notebook)
              (plist-put (ein:$notebook-metadata notebook)
                         :kernelspec (ein:notebook--spec-insert-name
                                      (ein:$kernelspec-name it) (ein:$kernelspec-spec it)))))
    `((metadata . ,(ein:aif (ein:$notebook-metadata notebook)
                       it
                     (make-hash-table)))
      (cells . ,(apply #'vector all-cells)))))

(defun ein:notebook-maybe-save-notebook (notebook &optional callback cbargs)
  (if (cl-some #'(lambda (ws)
                   (buffer-modified-p
                    (ein:worksheet-buffer ws)))
               (ein:$notebook-worksheets notebook))
      (ein:notebook-save-notebook notebook callback cbargs)))

(defun ein:notebook-save-notebook (notebook &optional callback cbargs errback)
  (unless (ein:notebook-buffer notebook)
    (let ((buf (format ein:notebook-buffer-name-template
                       (ein:$notebook-url-or-port notebook)
                       (ein:$notebook-notebook-name notebook))))
      (ein:log 'error "ein:notebook-save-notebook: notebook %s has no buffer!" buf)
      (setf (ewoc--buffer (ein:worksheet--ewoc
                           (car (ein:$notebook-worksheets notebook))))
            (get-buffer buf))))
  (condition-case err
      (with-current-buffer (ein:notebook-buffer notebook)
        (cl-letf (((symbol-function 'delete-trailing-whitespace) #'ignore))
          (run-hooks 'before-save-hook)))
    (error (ein:log 'warn "ein:notebook-save-notebook: Saving despite '%s'."
                    (error-message-string err))))
  (let ((content (ein:content-from-notebook notebook)))
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (ein:content-save content
                      #'ein:notebook-save-notebook-success
                      (list notebook callback cbargs)
                      #'ein:notebook-save-notebook-error
                      (list notebook errback))))

(defun ein:notebook-save-notebook-command ()
  "Save the notebook."
  (interactive)
  (ein:notebook-save-notebook ein:%notebook%))

(defun ein:notebook-save-notebook-success (notebook &optional callback cbargs)
  (ein:log 'verbose "Notebook is saved.")
  (setf (ein:$notebook-dirty notebook) nil)
  (mapc (lambda (ws)
          (ein:worksheet-save-cells ws) ; [#]_
          (ein:worksheet-set-modified-p ws nil))
        (ein:$notebook-worksheets notebook))
  (ein:events-trigger (ein:$notebook-events notebook)
                      'notebook_saved.Notebook)
  (when ein:notebook-create-checkpoint-on-save
    (ein:notebook-create-checkpoint notebook))
  (when callback
    (apply callback cbargs)))

;; .. [#] Consider the following case.
;;    (1) Open worksheet WS0 and other worksheets.
;;    (2) Edit worksheet WS0 then save the notebook.
;;    (3) Edit worksheet WS0.
;;    (4) Kill WS0 buffer by discarding the edit.
;;    (5) Save the notebook.
;;    This should save the latest WS0.  To do so, WS0 at the point (2)
;;    must be cached in the worksheet slot `:saved-cells'.

(cl-defun ein:notebook-save-notebook-error (notebook &key symbol-status &allow-other-keys)
  (if (eq symbol-status 'user-cancel)
      (ein:log 'info "Cancel saving notebook.")
    (ein:log 'info "Failed to save notebook!")
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_save_failed.Notebook)))

(defun ein:notebook-rename-command (path)
  "Rename current notebook and save it immediately.

NAME is any non-empty string that does not contain '/' or '\\'."
  (interactive
   (list (read-string "Rename to: "
                      (ein:$notebook-notebook-path ein:%notebook%))))
  (unless (and (string-match "\\.ipynb" path) (= (match-end 0) (length path)))
    (setq path (format "%s.ipynb" path)))
  (let* ((notebook (ein:notebook--get-nb-or-error))
         (content (ein:content-from-notebook notebook)))
    (ein:log 'verbose "Renaming notebook %s to '%s'" (ein:notebook-url notebook) path)
    (ein:content-rename content path #'ein:notebook-rename-success
                        (list notebook content))))

(defun ein:notebook-save-to-command (path)
  "Make a copy of the notebook and save it to a new path specified by NAME.
NAME is any non-empty string that does not contain '/' or '\\'.
"
  (interactive
   (list (read-string "Save copy to: " (ein:$notebook-notebook-path ein:%notebook%))))
  (unless (and (string-match ".ipynb" path) (= (match-end 0) (length path)))
    (setq path (format "%s.ipynb" path)))
  (let* ((content (ein:content-from-notebook ein:%notebook%))
         (name (substring path (or (cl-position ?/ path :from-end t) 0))))
    (setf (ein:$content-path content) path
          (ein:$content-name content) name)
    (ein:content-save content #'ein:notebook-open
                      (list (ein:$notebook-url-or-port ein:%notebook%)
                            path))))

(cl-defun ein:notebook-rename-success (notebook content)
  (ein:notebook-remove-opened-notebook notebook)
  (ein:notebook-set-notebook-name notebook (ein:$content-name content))
  (setf (ein:$notebook-notebook-path notebook) (ein:$content-path content))
  (ein:notebook-put-opened-notebook notebook)
  (mapc #'ein:worksheet-set-buffer-name
        (append (ein:$notebook-worksheets notebook)
                (ein:$notebook-scratchsheets notebook)))
  (ein:and-let* ((kernel (ein:$notebook-kernel notebook)))
    (ein:session-rename (ein:$kernel-url-or-port kernel)
                        (ein:$kernel-session-id kernel)
                        (ein:$content-path content))
    (setf (ein:$kernel-path kernel) (ein:$content-path content)))
  (ein:log 'info "Notebook renamed to %s." (ein:$content-name content)))

(defmacro ein:notebook-avoid-recursion (&rest body)
  `(let ((kill-buffer-query-functions
          (remove 'ein:notebook-kill-buffer-query kill-buffer-query-functions)))
     ,@body))

(defun ein:notebook-kill-notebook-buffers (notebook)
  "Kill all of NOTEBOOK's buffers"
  (mapc #'ein:notebook-kill-current-buffer (ein:notebook-buffer-list notebook)))

(defun ein:notebook-kill-current-buffer (buf)
  "Kill BUF and avoid recursion in kill-buffer-query-functions"
  (ein:notebook-avoid-recursion
   (let ((notebook (buffer-local-value 'ein:%notebook% buf)))
     (when (kill-buffer buf)
       (ein:notebook-tidy-opened-notebooks notebook)))))

(defsubst ein:notebook-kill-buffer-query ()
  (ein:aif (ein:get-notebook--notebook)
      (let ((buf (or (buffer-base-buffer (current-buffer))
                     (current-buffer))))
        (ein:notebook-ask-save it (apply-partially
                                   #'ein:notebook-kill-current-buffer
                                   buf))
        ;; don't kill buffer!
        nil)
    ;; kill buffer!
    t))

(defun ein:notebook-ask-save (notebook callback0)
  (unless callback0
    (setq callback0 #'ignore))
  (if (and (ein:notebook-modified-p notebook)
           (not (ob-ein-anonymous-p (ein:$notebook-notebook-path notebook))))
      (if (y-or-n-p (format "Save %s?" (ein:$notebook-notebook-name notebook)))
          (let ((success-positive 0))
            (add-function :before (var callback0) (lambda () (setq success-positive 1)))
            (ein:notebook-save-notebook notebook callback0 nil
                                        (lambda () (setq success-positive -1)))
            (cl-loop repeat 10
              until (not (zerop success-positive))
              do (sleep-for 0 200)
              finally return (> success-positive 0)))
        (when (ein:worksheet-p ein:%worksheet%)
          (ein:worksheet-dont-save-cells ein:%worksheet%)) ;; TODO de-obfuscate
        (funcall callback0)
        t)
    (funcall callback0)
    t))

(defun ein:notebook-close (notebook &optional callback &rest cbargs)
  (interactive (list (ein:notebook--get-nb-or-error)))
  (let* ((notebook (or notebook (ein:notebook--get-nb-or-error)))
         (callback0 (apply-partially #'ein:notebook-kill-notebook-buffers notebook)))
    (when callback
      (add-function :after (var callback0)
                    (apply #'apply-partially callback cbargs)))
    (ein:notebook-ask-save notebook callback0)))

(defun ein:notebook-kill-kernel-then-close-command (notebook)
  "Kill kernel and then kill notebook buffer.
To close notebook without killing kernel, just close the buffer
as usual."
  (interactive (list (ein:notebook--get-nb-or-error)))
  (let ((kernel (ein:$notebook-kernel notebook))
        (callback1 (apply-partially
                    (lambda (notebook* _kernel)
                      (ein:notebook-close notebook*))
                    notebook)))
    (if (ein:kernel-live-p kernel)
        (ein:message-whir "Ending session"
                          (add-function :before (var callback1) done-callback)
                          (ein:kernel-delete-session kernel callback1))
      (funcall callback1 nil))))

(defun ein:fast-content-from-notebook (notebook)
  "Quickly generate a basic content structure from notebook. This
function does not generate the full json representation of the
notebook worksheets."
  (make-ein:$content :name (ein:$notebook-notebook-name notebook)
                     :path (ein:$notebook-notebook-path notebook)
                     :url-or-port (ein:$notebook-url-or-port notebook)
                     :type "notebook"
                     :notebook-version (ein:$notebook-api-version notebook)))

(defun ein:notebook-create-checkpoint (notebook)
  "Create checkpoint for current notebook based on most recent save."
  (interactive (list (ein:get-notebook)))
  (if (ein:$notebook-q-checkpoints notebook)
      (ein:content-create-checkpoint
       (ein:fast-content-from-notebook notebook)
       (let ((notebook notebook))
         #'(lambda (content)
             (ein:log 'verbose "Checkpoint %s for %s generated."
                      (plist-get (car (ein:$content-checkpoints content)) :id)
                      (ein:$notebook-notebook-name notebook))
             (setf (ein:$notebook-checkpoints notebook)
                   (ein:$content-checkpoints content)))))))

(defun ein:notebook-list-checkpoint-ids (notebook)
  (unless (ein:$notebook-checkpoints notebook)
    (ein:content-query-checkpoints (ein:fast-content-from-notebook notebook)
                                   (let ((notebook notebook))
                                     #'(lambda (content)
                                         (setf (ein:$notebook-checkpoints notebook)
                                               (ein:$content-checkpoints content)))))
    (sleep-for 0.5))
  (cl-loop for cp in (ein:$notebook-checkpoints notebook)
    collecting (plist-get cp :last_modified)))

(defun ein:notebook-restore-to-checkpoint (notebook checkpoint)
  "Restore notebook to previous checkpoint saved on the Jupyter
server. Note that if there are multiple checkpoints the user will
be prompted on which one to use."
  (interactive
   (let* ((notebook (ein:get-notebook))
          (checkpoint (ein:completing-read
                       "Select checkpoint: "
                       (ein:notebook-list-checkpoint-ids notebook))))
     (list notebook checkpoint)))
  (ein:content-restore-checkpoint (ein:fast-content-from-notebook notebook)
                                  checkpoint)
  (ein:notebook-close notebook)
  (ein:notebook-open (ein:$notebook-url-or-port notebook)
                     (ein:$notebook-notebook-path notebook)))


;;; Worksheet

(defmacro ein:notebook--worksheet-render-new (notebook type)
  "Create new worksheet of TYPE in NOTEBOOK."
  (let ((func (intern (format "ein:%s-new" type)))
        (slot (list (intern (format "ein:$notebook-%ss" type)) notebook)))
    `(let ((ws (ein:notebook--worksheet-new ,notebook #',func)))
       (setf ,slot (append ,slot (list ws)))
       (ein:notebook--worksheet-render ,notebook ws)
       ws)))

(defun ein:notebook-worksheet-render-new (notebook)
  "Create new worksheet in NOTEBOOK."
  (ein:notebook--worksheet-render-new notebook worksheet))

(defun ein:notebook-worksheet-open-next-or-new (notebook ws &optional show)
  "Open next worksheet.  Create new if none.

Try to open the worksheet to the worksheet WS using the function
`ein:notebook-worksheet-open-next', open a new worksheet if not
found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ein:notebook-worksheet-open-next notebook ws)))
    (unless next
      (ein:log 'info "Creating new worksheet...")
      (setq next (ein:notebook-worksheet-render-new notebook))
      (ein:log 'info "Creating new worksheet... Done."))
    (when show
      (funcall show (ein:worksheet-buffer next)))))

(defun ein:notebook-worksheet-open-next-or-first (notebook ws &optional show)
  "Open next or first worksheet.

Try to open the worksheet to the worksheet WS using the function
`ein:notebook-worksheet-open-next', open the first worksheet if
not found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ein:notebook-worksheet-open-next notebook ws)))
    (unless next
      (setq next (car (ein:$notebook-worksheets notebook))))
    (when show
      (funcall show (ein:worksheet-buffer next)))))

(defun ein:notebook-worksheet-open-prev-or-last (notebook ws &optional show)
  "Open previous or last worksheet.
See also `ein:notebook-worksheet-open-next-or-first' and
`ein:notebook-worksheet-open-prev'."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (ein:notebook-worksheet-open-prev notebook ws)))
    (unless prev
      (setq prev (car (last (ein:$notebook-worksheets notebook)))))
    (when show
      (funcall show (ein:worksheet-buffer prev)))))

(cl-defun ein:notebook-worksheet--render-maybe
    (notebook ws &optional (adj "next"))
  "Render worksheet WS of NOTEBOOK if it does not have buffer.
ADJ is a adjective to describe worksheet to be rendered."
  (if (ein:worksheet-has-buffer-p ws)
      (ein:log 'verbose "The worksheet already has a buffer.")
    (ein:log 'verbose "Rendering %s worksheet..." adj)
    (ein:notebook--worksheet-render notebook ws)
    (ein:log 'verbose "Rendering %s worksheet... Done." adj)))

(cl-defun ein:notebook-worksheet--open-new
    (notebook new &optional (adj "next") show)
  "Open (possibly new) worksheet NEW of NOTEBOOK with SHOW function.
ADJ is a adjective to describe worksheet to be opened.
SHOW is a function to be called with worksheet buffer if given."
  (when new
    (ein:notebook-worksheet--render-maybe notebook new adj))
  (when show
    (cl-assert (ein:worksheet-p new) nil "No %s worksheet." adj)
    (funcall show (ein:worksheet-buffer new))))

(defun ein:notebook-worksheet-open-next (notebook ws &optional show)
  "Open next worksheet.

Search the worksheet after the worksheet WS, render it if it is
not yet, then return the worksheet.  If there is no such
worksheet, return nil.  Open the first worksheet if the worksheet
WS is an instance of `ein:scratchsheet'.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (if (ein:scratchsheet-p ws)
                  (car (ein:$notebook-worksheets notebook))
                (cl-loop with worksheets = (ein:$notebook-worksheets notebook)
                  for current in worksheets
                  for next in (cdr worksheets)
                  when (eq current ws) return next))))
    (ein:notebook-worksheet--open-new notebook next "next" show)
    next))

(defun ein:notebook-worksheet-open-prev (notebook ws &optional show)
  "Open previous worksheet.
See also `ein:notebook-worksheet-open-next'."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (if (ein:scratchsheet-p ws)
                  (car (last (ein:$notebook-worksheets notebook)))
                (cl-loop for (prev current) on (ein:$notebook-worksheets notebook)
                  when (eq current ws) return prev))))
    (ein:notebook-worksheet--open-new notebook prev "previous" show)
    prev))

(defun ein:notebook-worksheet-open-ith (notebook i &optional show)
  "Open I-th (zero-origin) worksheet."
  (let ((ws (nth i (ein:$notebook-worksheets notebook))))
    (unless ws (error "No %s-th worksheet" (1+ i)))
    (ein:notebook-worksheet--open-new notebook ws (format "%s-th" i) show)))

(defmacro ein:notebook-worksheet--defun-open-nth (n)
  "Define a command to open N-th (one-origin) worksheet."
  (cl-assert (and (integerp n) (> n 0)) t "Bad nth worksheet n=%s" n)
  (let ((func (intern (format "ein:notebook-worksheet-open-%sth" n))))
    `(defun ,func (notebook &optional show)
       ,(format "Open %d-th worksheet." n)
       (interactive (list (ein:notebook--get-nb-or-error)
                          #'switch-to-buffer))
       (ein:notebook-worksheet-open-ith notebook ,(1- n) show))))

(defmacro ein:notebook-worksheet--defun-all-open-nth (min max)
  `(progn
     ,@(cl-loop for n from min to max
         collect `(ein:notebook-worksheet--defun-open-nth ,n))))

(ein:notebook-worksheet--defun-all-open-nth 1 8)

(defun ein:notebook-worksheet-open-last (notebook &optional show)
  "Open the last worksheet."
  (interactive (list (ein:notebook--get-nb-or-error)
                     #'switch-to-buffer))
  (let ((last (car (last (ein:$notebook-worksheets notebook)))))
    (ein:notebook-worksheet--open-new notebook last "last" show)
    last))

(defun ein:notebook-worksheet-insert-new (notebook ws &optional render show
                                                   inserter)
  (let ((new (ein:notebook--worksheet-new notebook)))
    (setf (ein:$notebook-worksheets notebook)
          (funcall inserter (ein:$notebook-worksheets notebook) ws new))
    (when (or render show)
      (ein:notebook--worksheet-render notebook new))
    (when show
      (funcall show (ein:worksheet-buffer new)))
    new))

(cl-defun ein:notebook-worksheet-insert-next
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet after this worksheet and open it.
See also `ein:notebook-worksheet-insert-prev'.

.. The worksheet WS is searched in the worksheets slot of
   NOTEBOOK and a newly created worksheet is inserted after WS.
   Worksheet buffer is created when RENDER or SHOW is non-`nil'.
   SHOW is a function which take a buffer."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (ein:notebook-worksheet-insert-new notebook ws render show
                                     #'ein:list-insert-after))

(cl-defun ein:notebook-worksheet-insert-prev
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet before this worksheet and open it.
See also `ein:notebook-worksheet-insert-next'."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (ein:notebook-worksheet-insert-new notebook ws render show
                                     #'ein:list-insert-before))

(defun ein:notebook-worksheet-delete (notebook ws &optional confirm)
  "Delete the current worksheet.
When used as a lisp function, delete worksheet WS from NOTEBOOk."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)
                     t))
  (when (and confirm
             (not (y-or-n-p "Really remove this worksheet? There is no undo.")))
    (error "Quit deleting the current worksheet."))
  (setf (ein:$notebook-worksheets notebook)
        (delq ws (ein:$notebook-worksheets notebook)))
  (setf (ein:$notebook-dirty notebook) t)
  (kill-buffer (ein:worksheet-buffer ws)))

(defun ein:notebook-worksheet-move-prev (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (cl-assert (ein:worksheet-p ws) nil "Not worksheet.")
  (setf (ein:$notebook-worksheets notebook)
        (ein:list-move-left (ein:$notebook-worksheets notebook) ws)))

(defun ein:notebook-worksheet-move-next (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ein:notebook--get-nb-or-error)
                     (ein:worksheet--get-ws-or-error)))
  (cl-assert (ein:worksheet-p ws) nil "Not worksheet.")
  (setf (ein:$notebook-worksheets notebook)
        (ein:list-move-right (ein:$notebook-worksheets notebook) ws)))

(cl-defun ein:notebook-worksheet-index
    (&optional (notebook ein:%notebook%)
               (ws ein:%worksheet%))
  "Return an index of the worksheet WS in NOTEBOOK."
  (cl-loop for i from 0
    for ith-ws in (ein:$notebook-worksheets notebook)
    when (eq ith-ws ws)
    return i))


;;; Scratch sheet

(defun ein:notebook-scratchsheet-render-new (notebook)
  "Create new scratchsheet in NOTEBOOK."
  (ein:notebook--worksheet-render-new notebook scratchsheet))

(defun ein:notebook-scratchsheet-open (notebook &optional new popup)
  "Open \"scratch sheet\".
Open a new one when prefix argument is given.
Scratch sheet is almost identical to worksheet.  However, EIN
will not save the buffer.  Use this buffer like of normal IPython
console.  Note that you can always copy cells into the normal
worksheet to save result."
  (interactive (list (ein:notebook--get-nb-or-error)
                     current-prefix-arg
                     t))
  (let ((ss (or (unless new
                  (car (ein:$notebook-scratchsheets notebook)))
                (ein:notebook-scratchsheet-render-new notebook))))
    (when popup
      (pop-to-buffer (ein:worksheet-buffer ss)))
    ss))


;;; Opened notebooks

(defvar ein:notebook--opened-map (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT NOTEBOOK-ID) => notebook instance.")

(defun ein:notebook-get-opened-notebook (url-or-port path)
  (gethash (list url-or-port path) ein:notebook--opened-map))

(defun ein:notebook-get-opened-buffer (url-or-port path)
  (ein:aand (ein:notebook-get-opened-notebook url-or-port path)
            (ein:notebook-buffer it)))

(defun ein:notebook-put-opened-notebook (notebook)
  (puthash (list (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-path notebook))
           notebook
           ein:notebook--opened-map))

(defun ein:notebook-tidy-opened-notebooks (notebook)
  "Remove NOTEBOOK from ein:notebook--opened-map if it's not ein:notebook-live-p"
  (unless (ein:notebook-live-p notebook)
    (ein:notebook-remove-opened-notebook notebook)))

(defun ein:notebook-remove-opened-notebook (notebook)
  (remhash (list (ein:$notebook-url-or-port notebook)
                 (ein:$notebook-notebook-path notebook))
           ein:notebook--opened-map))

(defun ein:notebook-opened-notebooks (&optional predicate)
  "Return list of opened notebook instances.
If PREDICATE is given, notebooks are filtered by PREDICATE.
PREDICATE is called with each notebook and notebook is included
in the returned list only when PREDICATE returns non-nil value."
  (let ((notebooks (hash-table-values ein:notebook--opened-map)))
    (if predicate
        (seq-filter predicate notebooks)
      notebooks)))

(defun ein:notebook-opened-buffers (&optional predicate)
  "Return list of opened notebook buffers."
  (mapcar #'ein:notebook-buffer (ein:notebook-opened-notebooks predicate)))

(defun ein:notebook-opened-buffer-names (&optional predicate)
  "Return list of opened notebook buffer names.
If PREDICATE is given, the list is filtered by PREDICATE.
PREDICATE is called with the buffer name for each opened notebook."
  (let ((notebooks (mapcar #'buffer-name (ein:notebook-opened-buffers))))
    (if predicate
        (seq-filter predicate notebooks)
      notebooks)))


;;; Generic getter

(defun ein:get-url-or-port--notebook ()
  (when ein:%notebook% (ein:$notebook-url-or-port ein:%notebook%)))

(defun ein:get-notebook--notebook ()
  ein:%notebook%)

(defun ein:get-kernel--notebook ()
  (when (ein:$notebook-p ein:%notebook%)
    (ein:$notebook-kernel ein:%notebook%)))


;;; Predicate

(defun ein:notebook-live-p (notebook)
  "Return non-`nil' if NOTEBOOK has live buffer."
  (buffer-live-p (ein:notebook-buffer notebook)))

(defun ein:notebook-modified-p (&optional notebook)
  "Return non-nil if NOTEBOOK is modified.
If NOTEBOOK is not given or nil then consider the notebook
associated with current buffer (if any)."
  (unless notebook (setq notebook ein:%notebook%))
  (and (ein:$notebook-p notebook)
       (ein:notebook-live-p notebook)
       (or (ein:$notebook-dirty notebook)
           (cl-loop for ws in (ein:$notebook-worksheets notebook)
             when (ein:worksheet-modified-p ws)
             return t))))


;;; Notebook mode

(defcustom ein:notebook-modes
  '(ein:notebook-multilang-mode)
  "Notebook modes to use \(in order of preference).

When the notebook is opened, mode in this value is checked one by one
and the first usable mode is used.

Available modes:

* `ein:notebook-multilang-mode'
* `ein:notebook-mumamo-mode'
* `ein:notebook-python-mode'
* `ein:notebook-plain-mode'

Examples:

Use MuMaMo if it is installed.  Otherwise, use plain mode.
This is the old default setting::

  (setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-plain-mode))

Avoid using MuMaMo even when it is installed::

  (setq ein:notebook-modes '(ein:notebook-plain-mode))

Use simple `python-mode' based notebook mode when MuMaMo is not installed::

  (setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-python-mode))
"
  :type '(repeat (choice (const :tag "Multi-lang" ein:notebook-multilang-mode)
                         (const :tag "MuMaMo" ein:notebook-mumamo-mode)
                         (const :tag "Only Python" ein:notebook-python-mode)
                         (const :tag "Plain" ein:notebook-plain-mode)))
  :group 'ein)


(defun ein:notebook-choose-mode ()
  "Return usable (defined) notebook mode."
  (autoload 'ein:notebook-multilang-mode "ein-multilang")
  (cl-loop for mode in ein:notebook-modes
    if (functionp mode)
    return mode))

(defvar ein:notebook-mode-map (make-sparse-keymap))

(with-eval-after-load "ein-smartrep"
  (ein:smartrep-config ein:notebook-mode-map))


(defmacro ein:notebook--define-key (keymap key defn)
  "Ideally we could override just the keymap binding with a (string . wrapped) cons pair (as opposed to messing with the DEFN itself), but then describe-minor-mode unhelpfully shows ?? for the keymap commands."
  `(progn
     (when (functionp ,defn)
       (add-function :around (symbol-function ,defn)
                     (lambda (f &rest args)
                       (poly-ein-base (apply f args)))))
     (define-key ,keymap ,key ,defn)))

(let ((map ein:notebook-mode-map))
  (ein:notebook--define-key map "\C-ci" 'ein:inspect-object)
  (ein:notebook--define-key map "\C-c'" 'ein:edit-cell-contents)
  (ein:notebook--define-key map "\C-c\C-c" 'ein:worksheet-execute-cell)
  (ein:notebook--define-key map (kbd "M-RET") 'ein:worksheet-execute-cell-and-goto-next)
  (ein:notebook--define-key map (kbd "<M-S-return>")
    'ein:worksheet-execute-cell-and-insert-below)
  (ein:notebook--define-key map (kbd "C-c C-'") 'ein:worksheet-turn-on-autoexec)
  (ein:notebook--define-key map "\C-c\C-e" 'ein:worksheet-toggle-output)
  (ein:notebook--define-key map "\C-c\C-v" 'ein:worksheet-set-output-visibility-all)
  (ein:notebook--define-key map "\C-c\C-l" 'ein:worksheet-clear-output)
  (ein:notebook--define-key map (kbd "C-c C-S-l") 'ein:worksheet-clear-all-output)
  (ein:notebook--define-key map (kbd "C-c C-;") 'ein:shared-output-show-code-cell-at-point)
  (ein:notebook--define-key map "\C-c\C-k" 'ein:worksheet-kill-cell)
  (ein:notebook--define-key map "\C-c\M-w" 'ein:worksheet-copy-cell)
  (ein:notebook--define-key map "\C-c\C-w" 'ein:worksheet-copy-cell)
  (ein:notebook--define-key map "\C-c\C-y" 'ein:worksheet-yank-cell)
  (ein:notebook--define-key map "\C-c\C-a" 'ein:worksheet-insert-cell-above)
  (ein:notebook--define-key map "\C-c\C-b" 'ein:worksheet-insert-cell-below)
  (ein:notebook--define-key map "\C-c\C-t" 'ein:worksheet-toggle-cell-type)
  (ein:notebook--define-key map "\C-cS" 'ein:worksheet-toggle-slide-type)
  (ein:notebook--define-key map "\C-c\C-u" 'ein:worksheet-change-cell-type)
  (ein:notebook--define-key map "\C-c\C-s" 'ein:worksheet-split-cell-at-point)
  (ein:notebook--define-key map "\C-c\C-m" 'ein:worksheet-merge-cell)
  (ein:notebook--define-key map "\C-c\C-n" 'ein:worksheet-goto-next-input)
  (ein:notebook--define-key map "\C-c\C-p" 'ein:worksheet-goto-prev-input)
  (ein:notebook--define-key map (kbd "C-<up>") 'ein:worksheet-goto-prev-input)
  (ein:notebook--define-key map (kbd "C-<down>") 'ein:worksheet-goto-next-input)
  (ein:notebook--define-key map (kbd "C-c <up>") 'ein:worksheet-move-cell-up)
  (ein:notebook--define-key map (kbd "C-c <down>") 'ein:worksheet-move-cell-down)
  (ein:notebook--define-key map (kbd "M-<up>") 'ein:worksheet-move-cell-up)
  (ein:notebook--define-key map (kbd "M-<down>") 'ein:worksheet-move-cell-down)
  (ein:notebook--define-key map "\C-c\C-h" 'ein:pytools-request-tooltip-or-help)
  (ein:notebook--define-key map "\C-c\C-i" 'ein:completer-complete)
  (ein:notebook--define-key map (kbd "C-c C-$") 'ein:tb-show)
  (ein:notebook--define-key map "\C-c\C-x" nil)
  (ein:notebook--define-key map "\C-c\C-x\C-l" 'ein:notebook-toggle-latex-fragment)
  (ein:notebook--define-key map "\C-c\C-x\C-r" 'ein:notebook-restart-session-command)
  (ein:notebook--define-key map "\C-c\C-r" 'ein:notebook-reconnect-session-command)
  (ein:notebook--define-key map "\C-c\C-z" 'ein:notebook-kernel-interrupt-command)
  (ein:notebook--define-key map "\C-c\C-q" 'ein:notebook-kill-kernel-then-close-command)
  (ein:notebook--define-key map (kbd "C-c C-#") 'ein:notebook-close)
  (ein:notebook--define-key map (kbd "C-:") 'ein:shared-output-eval-string)
  (ein:notebook--define-key map "\C-c\C-f" 'ein:file-open)
  (ein:notebook--define-key map "\C-c\C-o" 'ein:notebook-open)
  (ein:notebook--define-key map "\C-x\C-s" 'ein:notebook-save-notebook-command)
  (ein:notebook--define-key map "\C-x\C-w" 'ein:notebook-rename-command)
  (define-key map "\M-."          'ein:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein:pytools-jump-back-command)
  (ein:notebook--define-key map "\M-p"          'ein:worksheet-previous-input-history)
  (ein:notebook--define-key map "\M-n"          'ein:worksheet-next-input-history)
  (ein:notebook--define-key map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)
  ;; Worksheets
  (ein:notebook--define-key map (kbd "C-c !")     'ein:worksheet-rename-sheet)
  (ein:notebook--define-key map (kbd "C-c {")     'ein:notebook-worksheet-open-prev-or-last)
  (ein:notebook--define-key map (kbd "C-c }")     'ein:notebook-worksheet-open-next-or-first)
  (ein:notebook--define-key map (kbd "C-c M-{")   'ein:notebook-worksheet-move-prev)
  (ein:notebook--define-key map (kbd "C-c M-}")   'ein:notebook-worksheet-move-next)
  (ein:notebook--define-key map (kbd "C-c +")     'ein:notebook-worksheet-insert-next)
  (ein:notebook--define-key map (kbd "C-c M-+")   'ein:notebook-worksheet-insert-prev)
  (ein:notebook--define-key map (kbd "C-c -")     'ein:notebook-worksheet-delete)
  (ein:notebook--define-key map "\C-c1" 'ein:notebook-worksheet-open-1th)
  (ein:notebook--define-key map "\C-c2" 'ein:notebook-worksheet-open-2th)
  (ein:notebook--define-key map "\C-c3" 'ein:notebook-worksheet-open-3th)
  (ein:notebook--define-key map "\C-c4" 'ein:notebook-worksheet-open-4th)
  (ein:notebook--define-key map "\C-c5" 'ein:notebook-worksheet-open-5th)
  (ein:notebook--define-key map "\C-c6" 'ein:notebook-worksheet-open-6th)
  (ein:notebook--define-key map "\C-c7" 'ein:notebook-worksheet-open-7th)
  (ein:notebook--define-key map "\C-c8" 'ein:notebook-worksheet-open-8th)
  (ein:notebook--define-key map "\C-c9" 'ein:notebook-worksheet-open-last)
  ;; Menu
  (easy-menu-define ein:notebook-menu map "EIN Notebook Mode Menu"
    `("EIN Notebook"
      ("File"
       ,@(ein:generate-menu
          '(("Save notebook" ein:notebook-save-notebook-command)
            ("Copy and rename notebook" ein:notebook-save-to-command)
            ("Rename notebook" ein:notebook-rename-command)
            ("Close notebook" ein:notebook-close)
            ("Kill kernel then close notebook"
             ein:notebook-kill-kernel-then-close-command))))
      ("Edit"
       ,@(ein:generate-menu
          '(("Kill cell" ein:worksheet-kill-cell)
            ("Copy cell" ein:worksheet-copy-cell)
            ("Yank cell" ein:worksheet-yank-cell)
            ("Insert cell above" ein:worksheet-insert-cell-above)
            ("Insert cell below" ein:worksheet-insert-cell-below)
            ("Toggle cell type" ein:worksheet-toggle-cell-type)
            ("Toggle slide type" ein:worksheet-toggle-slide-type)
            ("Change cell type" ein:worksheet-change-cell-type)
            ("Split cell at point" ein:worksheet-split-cell-at-point)
            ("Merge cell" ein:worksheet-merge-cell)
            ("Go to next cell" ein:worksheet-goto-next-input)
            ("Go to previous cell" ein:worksheet-goto-prev-input)
            ("Move cell up" ein:worksheet-move-cell-up)
            ("Move cell down" ein:worksheet-move-cell-down)
            ("Dedent text in CELL" ein:worksheet-dedent-cell-text)
            )))
      ("Cell/Code"
       ,@(ein:generate-menu
          '(("Edit cell contents in dedicated buffer" ein:edit-cell-contents)
            ("Execute cell" ein:worksheet-execute-cell
             :active (ein:worksheet-at-codecell-p))
            ("Execute cell and go to next"
             ein:worksheet-execute-cell-and-goto-next
             :active (ein:worksheet-at-codecell-p))
            ("Execute cell and insert below"
             ein:worksheet-execute-cell-and-insert-below
             :active (ein:worksheet-at-codecell-p))
            ("Execute all cells"
             ein:worksheet-execute-all-cells)
	    ("Execute all cells above"
             ein:worksheet-execute-all-cells-above)
	    ("Execute all cells below"
             ein:worksheet-execute-all-cells-below)
            ("Turn on auto execution flag" ein:worksheet-turn-on-autoexec
             :active (ein:worksheet-at-codecell-p))
            ("Evaluate code in minibuffer" ein:shared-output-eval-string)
            ("Toggle instant cell execution mode" ein:iexec-mode)
            ))
       "---"
       ,@(ein:generate-menu
          '(("Toggle output visibility" ein:worksheet-toggle-output
             :active (ein:worksheet-at-codecell-p))
            ("Show all output"
             ein:worksheet-set-output-visibility-all)
            ("Discard output" ein:worksheet-clear-output
             :active (ein:worksheet-at-codecell-p))
            ("Discard all output" ein:worksheet-clear-all-output)
            ("Show full output" ein:shared-output-show-code-cell-at-point
             :active (ein:worksheet-at-codecell-p))
            ("Traceback viewer" ein:tb-show)
            ))
       "---"
       ,@(ein:generate-menu
          '(("Show object help"
             ein:pytools-request-tooltip-or-help)
            ("Complete code" ein:completer-complete
             :active (ein:worksheet-at-codecell-p))
            ("Jump to definition" ein:pytools-jump-to-source-command)
            ("Go back to the previous jump point"
             ein:pytools-jump-back-command)
            ("Previous input history"
             ein:worksheet-previous-input-history)
            ("Next input history"
             ein:worksheet-next-input-history))))
      ("Kernel"
       ,@(ein:generate-menu
          '(("Restart session" ein:notebook-restart-session-command)
            ("Reconnect session" ein:notebook-reconnect-session-command)
            ("Switch kernel" ein:notebook-switch-kernel)
            ("Interrupt kernel" ein:notebook-kernel-interrupt-command))))
      ("Worksheets [Experimental]"
       ,@(ein:generate-menu
          '(("Rename worksheet" ein:worksheet-rename-sheet)
            ("Insert next worksheet"
             ein:notebook-worksheet-insert-next)
            ("Insert previous worksheet"
             ein:notebook-worksheet-insert-prev)
            ("Delete worksheet" ein:notebook-worksheet-delete)
            ("Move worksheet left"  ein:notebook-worksheet-move-prev)
            ("Move worksheet right" ein:notebook-worksheet-move-next)
            ))
       "---"
       ,@(ein:generate-menu
          '(("Open previous worksheet"
             ein:notebook-worksheet-open-prev)
            ("Open previous or last worksheet"
             ein:notebook-worksheet-open-prev-or-last)
            ("Open next worksheet"
             ein:notebook-worksheet-open-next)
            ("Open next or first worksheet"
             ein:notebook-worksheet-open-next-or-first)
            ("Open next or new worksheet"
             ein:notebook-worksheet-open-next-or-new)
            ))
       "---"
       ,@(ein:generate-menu
          (append
           (cl-loop for n from 1 to 8
             collect
               (list
                (format "Open %d-th worksheet" n)
                (intern (format "ein:notebook-worksheet-open-%sth" n))))
           '(("Open last worksheet" ein:notebook-worksheet-open-last)))))
      ;; Misc:
      ,@(ein:generate-menu
         '(("Open regular IPython console" ein:console-open)
           ("Open scratch sheet" ein:notebook-scratchsheet-open)
           ("Toggle pseudo console mode" ein:pseudo-console-mode)
           ))
      ))
  map)

(defcustom ein:enable-eldoc-support nil
  "Enable experimental support for eldoc in notebook buffers.

Disabled by default, but if you want to help debug this feature set it to T and
watch the fireworks!"
  :type 'boolean
  :group 'ein)

(define-minor-mode ein:notebook-mode
  "A mode for jupyter notebooks.

\\{ein:notebook-mode-map}
"
  :init-value nil
  :lighter " Notebook"
  :keymap ein:notebook-mode-map
  :group 'ein

  ;; BODY contains code to execute each time the mode is enabled or disabled.
  ;; It is executed after toggling the mode, and before running MODE-hook.

  (when ein:notebook-mode
    (cl-case ein:completion-backend
      (ein:use-ac-backend
       (ein:ac-install-backend)
       (ein:notebook--define-key ein:notebook-mode-map "." 'ein:notebook-ac-dot-complete)
       (auto-complete-mode))
      (ein:use-company-backend
       (add-to-list 'company-backends 'ein:company-backend)
       (ein:notebook--define-key ein:notebook-mode-map "." nil)
       (company-mode)))
    (ein:aif ein:helm-kernel-history-search-key
        (ein:notebook--define-key ein:notebook-mode-map it 'helm-ein-kernel-history))
    (ein:aif ein:anything-kernel-history-search-key
        (ein:notebook--define-key ein:notebook-mode-map it 'anything-ein-kernel-history))
    (setq indent-tabs-mode nil) ;; Being T causes problems with Python code.
    (when (and (featurep 'eldoc) ein:enable-eldoc-support)
        (add-function :before-until (local 'eldoc-documentation-function)
                      #'ein:completer--get-eldoc-signature)
        (eldoc-mode))
    (ein:worksheet-imenu-setup)
    (when ein:use-smartrep
      (require 'ein-smartrep))))

;; To avoid MuMaMo to discard `ein:notebook-mode', make it
;; permanent local.
(put 'ein:notebook-mode 'permanent-local t)

(define-derived-mode ein:notebook-plain-mode fundamental-mode "EIN[plain]"
  "IPython notebook mode without fancy coloring."
  (font-lock-mode))

(define-derived-mode ein:notebook-python-mode python-mode "EIN[python]"
  "Use `python-mode' for whole notebook buffer.")

(defun ein:notebook-open-in-browser (&optional print)
  "Open current notebook in web browser.
When the prefix argument (``C-u``) is given, print page is opened.
Note that print page is not supported in IPython 0.12.1."
  (interactive "P")
  (let ((url (apply #'ein:url
                    (ein:$notebook-url-or-port ein:%notebook%)
                    (if (>= (ein:$notebook-api-version ein:%notebook%) 3)
                        "notebooks")
                    (ein:$notebook-notebook-path ein:%notebook%)
                    (if print (list "print")))))
    (message "Opening %s in browser" url)
    (browse-url url)))

(defun ein:notebook-fetch-data (notebook callback &optional cbargs)
  "Fetch data in body tag of NOTEBOOK html page.
CALLBACK is called with a plist with data in the body tag as
the first argument and CBARGS as the rest of arguments."
  (let ((url-or-port (ein:$notebook-url-or-port notebook))
        (notebook-id (ein:$notebook-notebook-id notebook)))
    (ein:query-singleton-ajax
     (list 'notebook-fetch-data url-or-port notebook-id)
     (ein:url url-or-port notebook-id)
     :parser
     (lambda ()
       (list
        :project
        (ein:html-get-data-in-body-tag "data-project")
        :base-project-url
        (ein:html-get-data-in-body-tag "data-base-project-url")
        :base-kernel-url
        (ein:html-get-data-in-body-tag "data-base-kernel-url")
        :read-only
        (ein:html-get-data-in-body-tag "data-read-only")
        :notebook-id
        (ein:html-get-data-in-body-tag "data-notebook-id")))
     :success
     (apply-partially (cl-function
                       (lambda (callback cbargs &key data &allow-other-keys)
                         (apply callback data cbargs)))
                      callback cbargs))))

;;; Buffer and kill hooks
(add-hook 'kill-buffer-query-functions 'ein:notebook-kill-buffer-query)

(defun ein:notebook-close-notebooks (&optional blithely)
  "Used in `ein:jupyter-server-stop' and `kill-emacs-query-functions' hook."
  (ein:aif (ein:notebook-opened-notebooks)
      (if (and (cl-notevery #'identity (mapcar #'ein:notebook-close it))
               (not blithely))
          (y-or-n-p "Some notebooks could not be saved.  Exit anyway?")
        t)
    t))

(add-hook 'kill-emacs-query-functions 'ein:notebook-close-notebooks t)

;;;###autoload
(defalias 'ein:exit 'ein:quit)

;;;###autoload
(defun ein:quit (&optional force)
  "Close all notebooks and servers."
  (interactive "P")
  (ein:notebook-close-notebooks force)
  (when (featurep 'ein-jupyter)
    (ein:jupyter-server-stop force))) ; autoloaded


(defun ein:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  ;; TODO - it remains a bug that neither `ein:notebook-kill-buffer-callback'
  ;; nor `ein:notebook-close' updates ein:notebook--opened-map
  (ein:log 'debug "ein:notebook-kill-buffer-callback called")
  (when (ein:$notebook-p ein:%notebook%)
    (ein:notebook-disable-autosaves ein:%notebook%)
    (ein:notebook-close-worksheet ein:%notebook% ein:%worksheet%)))

(defun ein:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ein:notebook-kill-buffer-callback nil t))

(let* ((the-mode (ein:notebook-choose-mode))
       (incompatible-func (lambda ()
                            (when (boundp 'undo-tree-incompatible-major-modes)
                              (nconc undo-tree-incompatible-major-modes
                                     (list the-mode))))))
  (unless (funcall incompatible-func)
    (with-eval-after-load 'undo-tree
      (funcall incompatible-func))))

(provide 'ein-notebook)

;;; ein-notebook.el ends here
