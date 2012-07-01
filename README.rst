===================================
 Emacs IPython Notebook (and more)
===================================

.. note:: It is stable enough for my day to day work, but I can't
          grantee the safety for your notebook data.  So please make
          sure you have backup.

Screenshot
==========

.. figure:: https://github.com/tkf/emacs-ipython-notebook/raw/data/screenshots/notebook_simple_plot.png
   :alt: Plotting in Emacs IPython Notebook

See `more <https://github.com/tkf/emacs-ipython-notebook/wiki/Screenshots>`_!

Features
========

Emacs IPython Notebook (EIN) provides fully featured IPython Notebook
client and integrated REPL (like SLIME_).  While EIN makes notebook
editing very powerful by allowing you to use any Emacs features, it
also expose IPython features such as code evaluation, object
inspection and code completion to the Emacs side.  These features can
be accessed anywhere in Emacs and improve Python code editing and
reading in Emacs.

.. _SLIME: http://common-lisp.net/project/slime/

Highlighted features:

* Copy/paste cells, even to/from different notebooks.
* Console integration: You can easily connect to kernel via console
  application.  This enables you to start debugging in the same
  kernel.  It is even possible to connect console over ssh.
* IPython kernel can be "connected" to any buffers.  This enables you
  to evaluate buffer/region using same kernel as notebook.  Notebook
  goodies such as tooltip help, help browser and code completion are
  available in these buffers.
* Jump to definition (go to the definition by hitting ``M-.`` over an
  object).

Other notebook features:

* Inline images
* Auto/manual-completion
* Popup (tooltip) help
* Syntax highlighting in each cell types (Python/Markdown)
* Help browser (opens when executing ``function?``)
* Traceback viewer

Links:

* `Online Documentation
  <http://tkf.github.com/emacs-ipython-notebook/>`_
* `Screenshots
  <https://github.com/tkf/emacs-ipython-notebook/wiki/Screenshots>`_
* `Repository at GitHub
  <https://github.com/tkf/emacs-ipython-notebook>`_
* `Issue Tracker at GitHub
  <https://github.com/tkf/emacs-ipython-notebook/issues>`_


Quick try
=========

This is a quick and clean way to try EIN separately from your Emacs
setting.  If you want to try EIN but think preparing all the
requirements is too much, try this!::

   git clone git://github.com/tkf/zeroein.git
   zeroein/zeroein.py

This will launch a new Emacs instance.  For more information, see::

   zeroein/zeroein.py --help


Requirements
============

* IPython_ 0.12 or higher.
* `websocket.el`_
* (optional) mumamo_:
  It will be automatically loaded when it is on the path.
  The official way to setup path is to load nXhtml_.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or python-mode.el.
  Fabian Gallina's `python.el`_ is required to use
  ``ein:notebook-console-open`` command.
* (optional) `auto-complete.el`_
  You need to configure subpackage ``ein-ac`` to enable
  this feature.
* (optional) `smartrep.el`_:
  This package enables you to omit typing prefix keys (e.g.,
  ``C-c C-n C-n C-n ...`` instead of ``C-c C-n C-c C-n C-c C-n ...``).
  You need to configure subpackage ``ein-smartrep`` to enable
  this feature.

Also, EIN heavily relies on standard Emacs libraries including EWOC,
EIEIO and json.el.  EIN is currently tested in Emacs 24.1.

.. _IPython: http://ipython.org/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _mumamo: http://www.emacswiki.org/emacs/MuMaMo
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _python.el: https://github.com/fgallina/python.el
.. _auto-complete.el: http://cx4a.org/software/auto-complete/
.. _smartrep.el: https://github.com/myuhe/smartrep.el


Usage
=====

1. Install module.
   Put Emacs lisp ``ein*.el`` files and Python file ``ein.py`` in your
   load path.

2. Require module::

     (require 'ein)

3. Start `IPython notebook server`_.

4. Hit ``M-x ein:notebooklist-open`` to open notebook list.

.. _`IPython notebook server`:
   http://ipython.org/ipython-doc/stable/interactive/htmlnotebook.html


Subpackages
-----------

Enable `auto-complete.el`_::

   (setq ein:use-auto-complete t)
   ;; Or, to enable "superpack" (a little bit hacky improvements):
   ;; (setq ein:use-auto-complete-superpack t)

Enable `smartrep.el`_::

   (setq ein:use-smartrep t)


Keybinds - Notebook
-------------------

.. (ein:dev-insert-mode-map "\\{ein:notebook-mode-map}")

::

   key             binding
   ---             -------

   C-c             Prefix Command
   C-x             Prefix Command
   ESC             Prefix Command
   C-:             ein:notebook-eval-string
   <C-down>        ein:notebook-goto-next-input-command
   <C-up>          ein:notebook-goto-prev-input-command
   <M-down>        ein:notebook-move-cell-down-command
   <M-up>          ein:notebook-move-cell-up-command

   C-x C-s         ein:notebook-save-notebook-command
   C-x C-w         ein:notebook-rename-command

   M-RET           ein:notebook-execute-current-cell-and-goto-next
   M-,             ein:pytools-jump-back-command
   M-.             ein:pytools-jump-to-source-command

   C-c C-a         ein:notebook-insert-cell-above-command
   C-c C-b         ein:notebook-insert-cell-below-command
   C-c C-c         ein:notebook-execute-current-cell
   C-c C-e         ein:notebook-toggle-output-command
   C-c C-f         ein:notebook-request-tool-tip-or-help-command
   C-c TAB         ein:notebook-complete-command
   C-c C-k         ein:notebook-kill-cell-command
   C-c C-l         ein:notebook-clear-output-command
   C-c RET         ein:notebook-merge-cell-command
   C-c C-n         ein:notebook-goto-next-input-command
   C-c C-o         ein:notebook-console-open
   C-c C-p         ein:notebook-goto-prev-input-command
   C-c C-q         ein:notebook-kill-kernel-then-close-command
   C-c C-r         ein:notebook-restart-kernel-command
   C-c C-s         ein:notebook-split-cell-at-point
   C-c C-t         ein:notebook-toggle-cell-type
   C-c C-u         ein:notebook-change-cell-type
   C-c C-v         ein:notebook-set-collapsed-all-command
   C-c C-w         ein:notebook-copy-cell-command
   C-c C-x         ein:notebook-view-traceback
   C-c C-y         ein:notebook-yank-cell-command
   C-c C-z         ein:notebook-kernel-interrupt-command
   C-c ESC         Prefix Command
   C-c C-S-l       ein:notebook-clear-all-output-command
   C-c C-,         ein:pytools-jump-back-command
   C-c C-.         ein:pytools-jump-to-source-command
   C-c <down>      ein:notebook-move-cell-down-command
   C-c <up>        ein:notebook-move-cell-up-command

   C-c M-w         ein:notebook-copy-cell-command

.. // KEYS END //


Keybinds - Connect
------------------

In Python (or any other) buffer, you can connect to any open notebook
by ``M-x ein:connect-to-notebook`` then choose appropriate notebook.
After connecting to the notebook (and hence its kernel), the following
commands are available.

.. (ein:dev-insert-mode-map "\\{ein:connect-mode-map}")

::

   key             binding
   ---             -------

   C-c             Prefix Command
   ESC             Prefix Command
   C-:             ein:connect-eval-string

   M-,             ein:pytools-jump-back-command
   M-.             ein:pytools-jump-to-source-command

   C-c C-c         ein:connect-run-or-eval-buffer
   C-c C-f         ein:connect-request-tool-tip-or-help-command
   C-c TAB         ein:connect-complete-command
   C-c C-r         ein:connect-eval-region
   C-c C-z         ein:connect-pop-to-notebook
   C-c C-,         ein:pytools-jump-back-command
   C-c C-.         ein:pytools-jump-to-source-command

.. // KEYS END //
