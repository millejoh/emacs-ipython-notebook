;;; ein-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ein-connect" "ein-connect.el" (21715 54032
;;;;;;  0 0))
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

;;;### (autoloads nil "ein-console" "ein-console.el" (21715 54032
;;;;;;  0 0))
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

;;;### (autoloads nil "ein-dev" "ein-dev.el" (21720 44286 0 0))
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
Disable debugging support enabled by `ein:dev-start-debug'.

\(fn)" t nil)

(autoload 'ein:dev-bug-report-template "ein-dev" "\
Open a buffer with bug report template.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-helm" "ein-helm.el" (21715 54032 0 0))
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

;;;### (autoloads nil "ein-iexec" "ein-iexec.el" (21715 54032 0 0))
;;; Generated autoloads from ein-iexec.el

(autoload 'ein:iexec-mode "ein-iexec" "\
Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ein-ipynb-mode" "ein-ipynb-mode.el" (21715
;;;;;;  54032 0 0))
;;; Generated autoloads from ein-ipynb-mode.el

(autoload 'ein:ipynb-mode "ein-ipynb-mode" "\
A simple mode for ipynb file.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ein:ipynb-mode))

;;;***

;;;### (autoloads nil "ein-jedi" "ein-jedi.el" (21715 54032 0 0))
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
To use EIN and Jedi together, add the following in your Emacs setup.::

  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

.. _Jedi.el: https://github.com/tkf/emacs-jedi

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ein-junk" "ein-junk.el" (21715 54032 0 0))
;;; Generated autoloads from ein-junk.el

(autoload 'ein:junk-new "ein-junk" "\
Open a notebook to try random thing.
Notebook name is determined based on
`ein:junk-notebook-name-template'.

When prefix argument is given, it asks URL or port to use.

\(fn NAME URL-OR-PORT)" t nil)

(autoload 'ein:junk-rename "ein-junk" "\
Rename the current notebook based on `ein:junk-notebook-name-template'
and save it immediately.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "ein-kernel" "ein-kernel.el" (21715 54089 0
;;;;;;  0))
;;; Generated autoloads from ein-kernel.el

(defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port)

(defalias 'ein:kernel-id 'ein:$kernel-kernel-id)

;;;***

;;;### (autoloads nil "ein-multilang" "ein-multilang.el" (21715 54032
;;;;;;  0 0))
;;; Generated autoloads from ein-multilang.el

(autoload 'ein:notebook-multilang-mode "ein-multilang" "\
Notebook mode with multiple language fontification.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-notebook" "ein-notebook.el" (21720 44286
;;;;;;  0 0))
;;; Generated autoloads from ein-notebook.el

(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

;;;***

;;;### (autoloads nil "ein-notebooklist" "ein-notebooklist.el" (21720
;;;;;;  45483 0 0))
;;; Generated autoloads from ein-notebooklist.el

(autoload 'ein:notebooklist-open "ein-notebooklist" "\
Open notebook list buffer.

\(fn &optional URL-OR-PORT PATH NO-POPUP)" t nil)

(autoload 'ein:notebooklist-reload "ein-notebooklist" "\
Reload current Notebook list.

\(fn &optional NOTEBOOKLIST)" t nil)

(autoload 'ein:notebooklist-new-notebook "ein-notebooklist" "\
Ask server to create a new notebook and open it in a new buffer.

\(fn &optional URL-OR-PORT PATH CALLBACK CBARGS)" t nil)

(autoload 'ein:notebooklist-new-notebook-with-name "ein-notebooklist" "\
Open new notebook and rename the notebook.

\(fn NAME &optional URL-OR-PORT PATH)" t nil)

(autoload 'ein:notebooklist-list-notebooks "ein-notebooklist" "\
Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\".

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

;;;***

;;;### (autoloads nil "ein-org" "ein-org.el" (21715 54032 0 0))
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

(eval-after-load "org" '(progn (org-add-link-type "ipynb" 'ein:org-open) (add-hook 'org-store-link-functions 'ein:org-store-link)))

;;;***

;;;### (autoloads nil "ein-pseudo-console" "ein-pseudo-console.el"
;;;;;;  (21715 54032 0 0))
;;; Generated autoloads from ein-pseudo-console.el

(autoload 'ein:pseudo-console-mode "ein-pseudo-console" "\
Pseudo console mode.  Hit RET to execute code.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ein-shared-output" "ein-shared-output.el"
;;;;;;  (21715 54032 0 0))
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

;;;### (autoloads nil "ein-traceback" "ein-traceback.el" (21715 54032
;;;;;;  0 0))
;;; Generated autoloads from ein-traceback.el

(autoload 'ein:tb-show "ein-traceback" "\
Show full traceback in traceback viewer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("debug-ein.el" "ein-ac.el" "ein-cell.el"
;;;;;;  "ein-completer.el" "ein-contents-api.el" "ein-core.el" "ein-events.el"
;;;;;;  "ein-kernelinfo.el" "ein-kill-ring.el" "ein-log.el" "ein-multilang-fontify.el"
;;;;;;  "ein-mumamo.el" "ein-node.el" "ein-notification.el" "ein-output-area.el"
;;;;;;  "ein-pager.el" "ein-pkg.el" "ein-python.el" "ein-pytools.el"
;;;;;;  "ein-query.el" "ein-scratchsheet.el" "ein-smartrep.el" "ein-subpackages.el"
;;;;;;  "ein-utils.el" "ein-websocket.el" "ein-worksheet.el" "ein.el"
;;;;;;  "zeroein.el") (21720 45618 650000 0))

;;;***

(provide 'ein-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ein-loaddefs.el ends here
