;;; ein-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ein-company" "ein-company.el" (23493 60533
;;;;;;  798254 445000))
;;; Generated autoloads from ein-company.el

(autoload 'ein:company-backend "ein-company" "\


\(fn COMMAND &optional ARG &rest _)" t nil)

;;;***

;;;### (autoloads nil "ein-connect" "ein-connect.el" (23493 60533
;;;;;;  798254 445000))
;;; Generated autoloads from ein-connect.el

(autoload 'ein:connect-to-notebook-command "ein-connect" "\
Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks.

\(fn &optional NOT-YET-OPENED)" t nil)

(autoload 'ein:connect-to-notebook "ein-connect" "\
Connect any buffer to notebook and its kernel.

\(fn NBPATH &optional BUFFER NO-RECONNECTION)" t nil)

(autoload 'ein:connect-to-notebook-buffer "ein-connect" "\
Connect any buffer to opened notebook and its kernel.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'ein:connect-buffer-to-notebook "ein-connect" "\
Connect BUFFER to NOTEBOOK.

\(fn NOTEBOOK &optional BUFFER NO-RECONNECTION)" nil nil)

(autoload 'ein:connect-to-default-notebook "ein-connect" "\
Connect to the default notebook specified by
`ein:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ein-console" "ein-console.el" (23468 61365
;;;;;;  135112 242000))
;;; Generated autoloads from ein-console.el

(autoload 'ein:console-open "ein-console" "\
Open IPython console.
To use this function, `ein:console-security-dir' and
`ein:console-args' must be set properly.
This function works best with the new python.el_ which is shipped
with Emacs 24.2 or later.  If you don't have it, this function
opens a \"plain\" command line interpreter (comint) buffer where
you cannot use fancy stuff such as TAB completion.
It should be possible to support python-mode.el.  Patches are welcome!

.. _python.el: https://github.com/fgallina/python.el

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-dev" "ein-dev.el" (23495 30384 720586
;;;;;;  181000))
;;; Generated autoloads from ein-dev.el

(autoload 'ein:dev-insert-mode-map "ein-dev" "\
Insert mode-map into rst document.  For README.rst.

\(fn MAP-STRING)" nil nil)

(autoload 'ein:dev-start-debug "ein-dev" "\
Enable EIN debugging support.
When the prefix argument is given, debugging support for websocket
callback (`websocket-callback-debug-on-error') is enabled.

\(fn &optional WS-CALLBACK)" t nil)

(autoload 'ein:dev-stop-debug "ein-dev" "\
Inverse of `ein:dev-start-debug'.  Hard to maintain because it needs to match start

\(fn)" t nil)

(autoload 'ein:dev-bug-report-template "ein-dev" "\
Open a buffer with bug report template.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-helm" "ein-helm.el" (23064 59027 190301
;;;;;;  971000))
;;; Generated autoloads from ein-helm.el

(autoload 'anything-ein-kernel-history "ein-helm" "\
Search kernel execution history then insert the selected one.

\(fn)" t nil)

(autoload 'helm-ein-kernel-history "ein-helm" "\
Search kernel execution history then insert the selected one.

\(fn)" t nil)

(autoload 'anything-ein-notebook-buffers "ein-helm" "\
Choose opened notebook using anything.el interface.

\(fn)" t nil)

(autoload 'helm-ein-notebook-buffers "ein-helm" "\
Choose opened notebook using helm interface.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-iexec" "ein-iexec.el" (23064 59027 190301
;;;;;;  971000))
;;; Generated autoloads from ein-iexec.el

(autoload 'ein:iexec-mode "ein-iexec" "\
Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ein-inspector" "ein-inspector.el" (23493 60533
;;;;;;  802254 466000))
;;; Generated autoloads from ein-inspector.el

(autoload 'ein:inspect-object "ein-inspector" "\


\(fn KERNEL OBJECT)" t nil)

;;;***

;;;### (autoloads nil "ein-ipynb-mode" "ein-ipynb-mode.el" (23495
;;;;;;  30384 720586 181000))
;;; Generated autoloads from ein-ipynb-mode.el

(autoload 'ein:ipynb-mode "ein-ipynb-mode" "\
A simple mode for ipynb file.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ein:ipynb-mode))

;;;***

;;;### (autoloads nil "ein-jedi" "ein-jedi.el" (23493 60533 802254
;;;;;;  466000))
;;; Generated autoloads from ein-jedi.el

(autoload 'ein:jedi-complete "ein-jedi" "\
Run completion using candidates calculated by EIN and Jedi.

\(fn &key (expand ac-expand-on-auto-complete))" t nil)

(autoload 'ein:jedi-dot-complete "ein-jedi" "\
Insert \".\" and run `ein:jedi-complete'.

\(fn)" t nil)

(autoload 'ein:jedi-setup "ein-jedi" "\
Setup auto-completion using EIN and Jedi.el_ together.

Jedi.el_ is a Python auto-completion library for Emacs.
To use EIN and Jedi together, add the following in your Emacs setup before loading EIN.::

  (setq ein:completion-backend 'ein:use-ac-jedi-backend)

.. _Jedi.el: https://github.com/tkf/emacs-jedi

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ein-junk" "ein-junk.el" (23495 30384 720586
;;;;;;  181000))
;;; Generated autoloads from ein-junk.el

(autoload 'ein:junk-new "ein-junk" "\
Open a notebook to try random thing.
Notebook name is determined based on
`ein:junk-notebook-name-template'.

When prefix argument is given, it asks URL or port to use.

\(fn NAME KERNELSPEC URL-OR-PORT)" t nil)

(autoload 'ein:junk-rename "ein-junk" "\
Rename the current notebook based on `ein:junk-notebook-name-template'
and save it immediately.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "ein-jupyter" "ein-jupyter.el" (23495 30384
;;;;;;  724586 204000))
;;; Generated autoloads from ein-jupyter.el

(autoload 'ein:jupyter-server-login-and-open "ein-jupyter" "\
Log in and open a notebooklist buffer for a running jupyter notebook server.

Determine if there is a running jupyter server (started via a
call to `ein:jupyter-server-start') and then try to guess if
token authentication is enabled. If a token is found use it to generate a
call to `ein:notebooklist-login' and once authenticated open the notebooklist buffer
via a call to `ein:notebooklist-open'.

\(fn &optional CALLBACK)" t nil)

(autoload 'ein:jupyter-server-start "ein-jupyter" "\
Start SERVER-CMD_PATH with `--notebook-dir' NOTEBOOK-DIRECTORY.  Login after connection established unless NO-LOGIN-P is set.  LOGIN-CALLBACK taking single argument, the buffer created by ein:notebooklist-open--finish.

This command opens an asynchronous process running the jupyter
notebook server and then tries to detect the url and password to
generate automatic calls to `ein:notebooklist-login' and
`ein:notebooklist-open'.

With \\[universal-argument] prefix arg, it will prompt the user for the path to
the jupyter executable first. Else, it will try to use the
value of `*ein:last-jupyter-command*' or the value of the
customizable variable `ein:jupyter-default-server-command'.

Then it prompts the user for the path of the root directory
containing the notebooks the user wants to access.

The buffer named by `ein:jupyter-server-buffer-name' will contain
the log of the running jupyter server.

\(fn SERVER-CMD-PATH NOTEBOOK-DIRECTORY &optional NO-LOGIN-P LOGIN-CALLBACK)" t nil)

(autoload 'ein:jupyter-server-stop "ein-jupyter" "\
Stop a running jupyter notebook server.

Use this command to stop a running jupyter notebook server. If
there is no running server then no action will be taken.

\(fn &optional FORCE LOG)" t nil)

;;;***

;;;### (autoloads nil "ein-jupyterhub" "ein-jupyterhub.el" (23495
;;;;;;  30384 724586 204000))
;;; Generated autoloads from ein-jupyterhub.el

(autoload 'ein:jupyterhub-connect "ein-jupyterhub" "\
Log on to a jupyterhub server using PAM authentication. Requires jupyterhub version 0.8 or greater.

\(fn URL USER PASSWORD)" t nil)

;;;***

;;;### (autoloads nil "ein-kernel" "ein-kernel.el" (23493 60533 802254
;;;;;;  466000))
;;; Generated autoloads from ein-kernel.el

(defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port)

(defalias 'ein:kernel-id 'ein:$kernel-kernel-id)

;;;***

;;;### (autoloads nil "ein-multilang" "ein-multilang.el" (23493 60533
;;;;;;  802254 466000))
;;; Generated autoloads from ein-multilang.el

(autoload 'ein:notebook-multilang-mode "ein-multilang" "\
Notebook mode with multiple language fontification.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-notebook" "ein-notebook.el" (23495 30384
;;;;;;  724586 204000))
;;; Generated autoloads from ein-notebook.el

(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

;;;***

;;;### (autoloads nil "ein-notebooklist" "ein-notebooklist.el" (23495
;;;;;;  39319 328691 982000))
;;; Generated autoloads from ein-notebooklist.el

(autoload 'ein:notebooklist-enable-keepalive "ein-notebooklist" "\
Enable periodic calls to the notebook server to keep long running sessions from expiring.
By long running we mean sessions to last days, or weeks. The
frequency of the refresh (which is very similar to a call to
`ein:notebooklist-open`) is controlled by
`ein:notebooklist-keepalive-refresh-time`, and is measured in
terms of hours. If `ein:enable-keepalive' is non-nil this will
automatically be called during calls to `ein:notebooklist-open`.

\(fn &optional URL-OR-PORT)" t nil)

(autoload 'ein:notebooklist-disable-keepalive "ein-notebooklist" "\
Disable the notebooklist keepalive calls to the jupyter notebook server.

\(fn)" t nil)

(autoload 'ein:notebooklist-reload "ein-notebooklist" "\
Reload current Notebook list.

\(fn NOTEBOOKLIST &optional RESYNC)" t nil)

(autoload 'ein:notebooklist-upload-file "ein-notebooklist" "\


\(fn UPLOAD-PATH)" t nil)

(autoload 'ein:notebooklist-new-notebook "ein-notebooklist" "\
Ask server to create a new notebook and open it in a new buffer.

TODO - New and open should be separate, and we should flag an exception if we try to new an existing.

\(fn &optional URL-OR-PORT KERNELSPEC PATH CALLBACK CBARGS)" t nil)

(autoload 'ein:notebooklist-new-notebook-with-name "ein-notebooklist" "\
Open new notebook and rename the notebook.

\(fn NAME KERNELSPEC URL-OR-PORT &optional PATH)" t nil)

(autoload 'ein:notebooklist-list-notebooks "ein-notebooklist" "\
Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/PATH\".

\(fn)" nil nil)

(autoload 'ein:notebooklist-open-notebook-global "ein-notebooklist" "\
Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'.

\(fn NBPATH &optional CALLBACK CBARGS)" t nil)

(autoload 'ein:notebooklist-load "ein-notebooklist" "\
Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein:notebooklist-load)

You should setup `ein:url-or-port' or `ein:default-url-or-port'
in order to make this code work.

See also:
`ein:connect-to-default-notebook', `ein:connect-default-notebook'.

\(fn &optional URL-OR-PORT)" nil nil)

(defalias 'ein:login 'ein:notebooklist-login)

(defalias 'ein-login 'ein:notebooklist-login)

(autoload 'ein:notebooklist-login "ein-notebooklist" "\
Deal with security before main entry of ein:notebooklist-open.

CALLBACK takes one argument, the buffer created by ein:notebooklist-open--success.

\(fn URL-OR-PORT CALLBACK)" t nil)

(autoload 'ein:notebooklist-change-url-port "ein-notebooklist" "\
Update the ipython/jupyter notebook server URL for all the
notebooks currently opened from the current notebooklist buffer.

This function works by calling `ein:notebook-update-url-or-port'
on all the notebooks opened from the current notebooklist.

\(fn NEW-URL-OR-PORT)" t nil)

;;;***

;;;### (autoloads nil "ein-org" "ein-org.el" (23492 45645 196314
;;;;;;  883000))
;;; Generated autoloads from ein-org.el

(autoload 'ein:org-open "ein-org" "\
Open IPython notebook specified by LINK-PATH.
This function is to be used for FOLLOW function of
`org-add-link-type'.

\(fn LINK-PATH)" nil nil)

(autoload 'ein:org-store-link "ein-org" "\
Call `org-store-link-props' when in notebook buffer.
This function is to be used for `org-store-link-functions'.

Examples::

  ipynb:(:url-or-port 8888 :name \"My_Notebook\")
  ipynb:(:url-or-port \"http://notebook-server\" :name \"My_Notebook\")

Note that spaces will be escaped in org files.

As how IPython development team supports multiple directory in
IPython notebook server is unclear, it is not easy to decide the
format for notebook links.  Current approach is to use
S-expression based (rather verbose) serialization, so that
extending link spec without loosing backward compatibility is
easier.  For the examples of link format in general, see Info
node `(org) External links' and Info node `(org) Search options'

\(fn)" nil nil)

(eval-after-load "org" '(if (fboundp 'org-link-set-parameters) (org-link-set-parameters "ipynb" :follow 'ein:org-open :help-echo "Open ipython notebook." :store 'ein:org-store-link) (org-add-link-type "ipynb" :follow 'ein:org-open) (add-hook 'org-store-link-functions 'ein:org-store-link)))

;;;***

;;;### (autoloads nil "ein-pseudo-console" "ein-pseudo-console.el"
;;;;;;  (23064 59027 198301 987000))
;;; Generated autoloads from ein-pseudo-console.el

(autoload 'ein:pseudo-console-mode "ein-pseudo-console" "\
Pseudo console mode.  Hit RET to execute code.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ein-shared-output" "ein-shared-output.el"
;;;;;;  (23468 61365 139112 317000))
;;; Generated autoloads from ein-shared-output.el

(autoload 'ein:shared-output-pop-to-buffer "ein-shared-output" "\
Open shared output buffer.

\(fn)" t nil)

(autoload 'ein:shared-output-show-code-cell-at-point "ein-shared-output" "\
Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein:cell-max-num-outputs'.

\(fn)" t nil)

(autoload 'ein:shared-output-eval-string "ein-shared-output" "\
Evaluate a piece of code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ein:shared-output-pop-to-buffer'.

.. ARGS is passed to `ein:kernel-execute'.  Unlike `ein:kernel-execute',
   `:silent' is `nil' by default.

\(fn CODE &optional POPUP VERBOSE KERNEL &rest ARGS)" t nil)

;;;***

;;;### (autoloads nil "ein-traceback" "ein-traceback.el" (23493 60533
;;;;;;  806254 487000))
;;; Generated autoloads from ein-traceback.el

(autoload 'ein:tb-show "ein-traceback" "\
Show full traceback in traceback viewer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("debug-ein.el" "ein-ac.el" "ein-cell-edit.el"
;;;;;;  "ein-cell-output.el" "ein-cell.el" "ein-classes.el" "ein-completer.el"
;;;;;;  "ein-contents-api.el" "ein-core.el" "ein-events.el" "ein-file.el"
;;;;;;  "ein-hy.el" "ein-ipdb.el" "ein-kernelinfo.el" "ein-kill-ring.el"
;;;;;;  "ein-log.el" "ein-multilang-fontify.el" "ein-node.el" "ein-notification.el"
;;;;;;  "ein-output-area.el" "ein-pager.el" "ein-pkg.el" "ein-process.el"
;;;;;;  "ein-python.el" "ein-pytools.el" "ein-query.el" "ein-scratchsheet.el"
;;;;;;  "ein-skewer.el" "ein-smartrep.el" "ein-subpackages.el" "ein-timestamp.el"
;;;;;;  "ein-utils.el" "ein-websocket.el" "ein-worksheet.el" "ein.el"
;;;;;;  "ob-ein.el" "zeroein.el") (23495 30384 724586 204000))

;;;***

(provide 'ein-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ein-loaddefs.el ends here
