==============================================
 EIN -- Emacs IPython Notebook |build-status|
==============================================

  --- or **E**\ IN **I**\ s not only for **N**\ otebooks.


.. note:: It is stable enough for my day to day work, but I can't
          guarantee the safety for your notebook data.  So please make
          sure you have backup.

.. |build-status|
   image:: https://secure.travis-ci.org/tkf/emacs-ipython-notebook.png
           ?branch=master
   :target: http://travis-ci.org/tkf/emacs-ipython-notebook
   :alt: Build Status


Screenshot
==========

.. figure:: https://github.com/tkf/emacs-ipython-notebook/raw/data/screenshots/notebook_simple_plot.png
   :alt: Plotting in Emacs IPython Notebook

See `more <https://github.com/tkf/emacs-ipython-notebook/wiki/Screenshots>`_!

Features
========

Emacs IPython Notebook (EIN) provides a IPython Notebook client and
integrated REPL (like SLIME_) in Emacs.  While EIN makes notebook
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

* `Wiki
  <https://github.com/tkf/emacs-ipython-notebook/wiki>`_

  + `Screenshots
    <https://github.com/tkf/emacs-ipython-notebook/wiki/Screenshots>`_
  + `Tips
    <https://github.com/tkf/emacs-ipython-notebook/wiki/Tips>`_

* `Downloads
  <https://github.com/tkf/emacs-ipython-notebook/tags>`_
* `Repository at GitHub
  <https://github.com/tkf/emacs-ipython-notebook>`_
* `Issue Tracker at GitHub
  <https://github.com/tkf/emacs-ipython-notebook/issues>`_


Quick try
=========

This is a quick and clean way to try EIN separately from your Emacs
setting.  If you want to try EIN but think preparing all the
requirements is too much, try this!::

   git clone git://github.com/tkf/emacs-ipython-notebook.git
   cd emacs-ipython-notebook/
   lisp/zeroein.el

This will launch a new Emacs instance.

You can use environment variable ``EMACS`` to control Emacs executable
to use.::

   EMACS=emacs-snapshot lisp/zeroein.el

The above command requires /bin/sh.  If the above command does not work
(e.g., you are using MS Windows), try the following command::

  emacs -Q -l lisp/zeroein.el


Requirements
============

* IPython_ 0.12 or higher.
* `websocket.el`_ 0.9
* `request.el`_ >= 0.2
* (optional) mumamo_ developmental version:
  It will be automatically loaded when it is on the path.
  The official way to setup path is to load nXhtml_.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or python-mode.el.
  Fabian Gallina's `python.el`_ is required to use
  ``ein:console-open`` command.
* (optional) `auto-complete.el`_
  You need to configure subpackage ``ein-ac`` to enable
  this feature.
* (optional) `smartrep.el`_:
  This package enables you to omit typing prefix keys (e.g.,
  ``C-c C-n C-n C-n ...`` instead of ``C-c C-n C-c C-n C-c C-n ...``).
  You need to configure subpackage ``ein-smartrep`` to enable
  this feature.

Also, EIN heavily relies on standard Emacs libraries including EWOC,
EIEIO and json.el.  EIN is currently tested against Emacs 23.3 and 24.3.
It is known to work in Emacs 23.2, 24.1 and 24.2.

.. _IPython: http://ipython.org/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _request.el: https://github.com/tkf/emacs-request
.. _mumamo: http://www.emacswiki.org/emacs/MuMaMo
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _python.el: https://github.com/fgallina/python.el
.. _auto-complete.el: http://cx4a.org/software/auto-complete/
.. _smartrep.el: https://github.com/myuhe/smartrep.el


Usage
=====

1. Install module.
   Put Emacs lisp ``ein*.el`` files and Python file ``ein.py`` in your
   load path.  See `online documentation`_ for more information.

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
   .               ein:notebook-complete-dot
   C-:             ein:shared-output-eval-string
   <C-down>        ein:worksheet-goto-next-input
   <C-up>          ein:worksheet-goto-prev-input
   <M-S-return>    ein:worksheet-execute-cell-and-insert-below
   <M-down>        ein:worksheet-move-cell-down
   <M-up>          ein:worksheet-move-cell-up

   C-x C-s         ein:notebook-save-notebook-command
   C-x C-w         ein:notebook-rename-command

   M-RET           ein:worksheet-execute-cell-and-goto-next
   M-,             ein:pytools-jump-back-command
   M-.             ein:pytools-jump-to-source-command
   M-n             ein:worksheet-next-input-history
   M-p             ein:worksheet-previous-input-history

   C-c C-a         ein:worksheet-insert-cell-above
   C-c C-b         ein:worksheet-insert-cell-below
   C-c C-c         ein:worksheet-execute-cell
   C-c C-e         ein:worksheet-toggle-output
   C-c C-f         ein:pytools-request-tooltip-or-help
   C-c TAB         ein:completer-complete
   C-c C-k         ein:worksheet-kill-cell
   C-c C-l         ein:worksheet-clear-output
   C-c RET         ein:worksheet-merge-cell
   C-c C-n         ein:worksheet-goto-next-input
   C-c C-o         ein:console-open
   C-c C-p         ein:worksheet-goto-prev-input
   C-c C-q         ein:notebook-kill-kernel-then-close-command
   C-c C-r         ein:notebook-restart-kernel-command
   C-c C-s         ein:worksheet-split-cell-at-point
   C-c C-t         ein:worksheet-toggle-cell-type
   C-c C-u         ein:worksheet-change-cell-type
   C-c C-v         ein:worksheet-set-output-visibility-all
   C-c C-w         ein:worksheet-copy-cell
   C-c C-x         ein:tb-show
   C-c C-y         ein:worksheet-yank-cell
   C-c C-z         ein:notebook-kernel-interrupt-command
   C-c ESC         Prefix Command
   C-c !           ein:worksheet-rename-sheet
   C-c +           ein:notebook-worksheet-insert-next
   C-c -           ein:notebook-worksheet-delete
   C-c 1           ein:notebook-worksheet-open-1th
   C-c 2           ein:notebook-worksheet-open-2th
   C-c 3           ein:notebook-worksheet-open-3th
   C-c 4           ein:notebook-worksheet-open-4th
   C-c 5           ein:notebook-worksheet-open-5th
   C-c 6           ein:notebook-worksheet-open-6th
   C-c 7           ein:notebook-worksheet-open-7th
   C-c 8           ein:notebook-worksheet-open-8th
   C-c 9           ein:notebook-worksheet-open-last
   C-c {           ein:notebook-worksheet-open-prev-or-last
   C-c }           ein:notebook-worksheet-open-next-or-first
   C-c C-S-l       ein:worksheet-clear-all-output
   C-c C-#         ein:notebook-close
   C-c C-'         ein:worksheet-turn-on-autoexec
   C-c C-,         ein:pytools-jump-back-command
   C-c C-.         ein:pytools-jump-to-source-command
   C-c C-/         ein:notebook-scratchsheet-open
   C-c C-;         ein:shared-output-show-code-cell-at-point
   C-c <down>      ein:worksheet-move-cell-down
   C-c <up>        ein:worksheet-move-cell-up

   C-c M-+         ein:notebook-worksheet-insert-prev
   C-c M-w         ein:worksheet-copy-cell
   C-c M-{         ein:notebook-worksheet-move-prev
   C-c M-}         ein:notebook-worksheet-move-next

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
   .               ein:completer-dot-complete
   C-:             ein:shared-output-eval-string

   M-,             ein:pytools-jump-back-command
   M-.             ein:pytools-jump-to-source-command

   C-c C-a         ein:connect-toggle-autoexec
   C-c C-c         ein:connect-run-or-eval-buffer
   C-c C-f         ein:pytools-request-tooltip-or-help
   C-c TAB         ein:completer-complete
   C-c C-l         ein:connect-reload-buffer
   C-c C-o         ein:console-open
   C-c C-r         ein:connect-eval-region
   C-c C-x         ein:tb-show
   C-c C-z         ein:connect-pop-to-notebook
   C-c C-,         ein:pytools-jump-back-command
   C-c C-.         ein:pytools-jump-to-source-command
   C-c C-/         ein:notebook-scratchsheet-open

.. // KEYS END //


License
=======

Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.
