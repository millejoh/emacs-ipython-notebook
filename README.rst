========================================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev| |melpa-stable|
========================================================================

  --- or **E**\ IN **I**\ s not only for **N**\ otebooks.

EIN works with IPython 2.x_, 3.x_, and Jupyter_! Note that remote and password
protected logins are working with IPython 3.x, but have not been tested with
Jupyter.

.. note:: The code has been stable enough for my day to day work, but there are
          no guarantees for the safety for your notebook data.  Please make sure
          that you back-up and back-up often!

.. note:: The code for testing EIN is horribly broken, but I regularly hand
          check the code running against IPython's suite of sample
          notebooks. It's a worse-is-better solution to problem requiring a
          time-consuming solution.

.. |build-status|
   image:: https://secure.travis-ci.org/millejoh/emacs-ipython-notebook.png?branch=master
   :target: http://travis-ci.org/millejoh/emacs-ipython-notebook
   :alt: Build Status
.. |melpa-dev|
   image:: http://melpa.milkbox.net/packages/ein-badge.svg
   :target: http://melpa.milkbox.net/#/ein
   :alt: MELPA development version
.. |melpa-stable|
   image:: http://melpa-stable.milkbox.net/packages/ein-badge.svg
   :target: http://melpa-stable.milkbox.net/#/ein
   :alt: MELPA stable version
.. _2.x: http://ipython.org/ipython-doc/2/index.html
.. _3.x: http://ipython.org/ipython-doc/3/index.html


Org-mode Integration
====================

EIN now integrates_ with org-mode! The code was heavily inspired by ob-ipython_
which is another project very much worth checking out. Find it on MELPA_.

.. _integrates: http://millejoh.github.io/emacs-ipython-notebook/#org-mode-integration
.. _ob-ipython: https://github.com/gregsexton/ob-ipython/

Screenshots
===========

.. figure:: https://github.com/millejoh/emacs-ipython-notebook/wiki/images/demo_plotnormal.PNG
   :alt: Plotting in Emacs IPython Notebook

.. figure:: https://github.com/millejoh/emacs-ipython-notebook/wiki/images/R-kernel-example.PNG
   :alt: EIN connecting to an R kernel

See `more <https://github.com/millejoh/emacs-ipython-notebook/wiki/Screenshots>`_!

Features
========

The Emacs IPython Notebook (EIN) provides a client for IPython v2.x and 3.x and
Jupyter notebooks via an integrated REPL (like SLIME_) in Emacs. EIN makes
notebook editing very powerful by allowing you to use Emacs features; it also
expose IPython features such as code evaluation, object inspection and code
completion to the Emacs side. These features can be accessed anywhere in Emacs
and improve Python code editing and reading in Emacs.

.. _SLIME: http://common-lisp.net/project/slime/

Highlighted features:

* Copy/paste cells, even to/from different notebooks.
* Console integration: You can easily connect to a kernel via the console
  application.  This enables you to start debugging in the same kernel.  It is
  even possible to connect to a console over ssh.
* An IPython kernel can be "connected" to any buffer.  This enables you to
  evaluate a buffer or buffer region using the same kernel as the notebook.
  Notebook goodies such as tooltip help, help browser and code completion are
  available in these buffers.
* Jump to definition (go to the definition by hitting ``M-.`` over an object).

Other notebook features:

* Inline images
* Auto/manual-completion
* Popup (tooltip) help
* Syntax highlighting in each cell type (Python/Markdown)
* Help browser (opens when executing ``function?``)
* Traceback viewer
* Integration with org-mode
* Support for debugging via %debug
* Multiple python kernels
* EXPERIMENTAL: Non-python kernels! R runs in EIN, see the screenshot above!

Links:

* `Online Documentation
  <http://millejoh.github.io/emacs-ipython-notebook/>`_

* `Wiki
  <https://github.com/millejoh/emacs-ipython-notebook/wiki>`_

  + `Screenshots
    <https://github.com/millejoh/emacs-ipython-notebook/wiki/Screenshots>`_
  + `Tips
    <https://github.com/millejoh/emacs-ipython-notebook/wiki/Tips>`_

* `Downloads
  <https://github.com/millejoh/emacs-ipython-notebook/tags>`_
* `Repository at GitHub
  <https://github.com/millejoh/emacs-ipython-notebook>`_
* `Issue Tracker at GitHub
  <https://github.com/millejoh/emacs-ipython-notebook/issues>`_


Quick try
=========

Install from MELPA_! Seriously, installing from MELPA is the easiest way to
ensure all of ein's dependencies are correctly installed. `zeroein.el` is from a
bygone era and does not quite work the way it used to. If you are determined,
and slightly masochistic, you can attempt the following::

   git clone git://github.com/millejoh/emacs-ipython-notebook.git
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

* EMACS 24.5, 25.2, or 26
* IPython_ 2.0 or higher.
* Tornado_ 4.0.2 or higher.
* `websocket.el`_ >= 1.7
* `request.el`_ >= 0.3
* `request-deferred.el`_ >= 0.2
* `dash`_ >= 2.13
* `s`_ >= 1.11
* `auto-complete.el`_ >= 1.4:
  You need to configure subpackage ``ein-ac`` to enable
  this feature.
* `skewer-mode`_ >= 1.6.2:
  Skewer mode gives EIN the ability to execute dynamic javascript in the
  note book.
* (optional) Jupyterhub_ 0.8 or higher:
  EIN supports logging in to Jupyterhub servers using PAM authentication,
  though this only works with v0.8, which currently is the development version
  of Jupyterhub.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or python-mode.el. `python.el`_ is
  required to use ``ein:console-open`` command.
* (optional) `smartrep.el`_:
  This package enables you to omit typing prefix keys (e.g.,
  ``C-c C-n C-n C-n ...`` instead of ``C-c C-n C-c C-n C-c C-n ...``).
  You need to configure subpackage ``ein-smartrep`` to enable
  this feature.
* (optional) `jedi.el`_:
  Python auto-completion for emacs using `jedi`_. In your
  emacs initialization file add

  ``(setq ein:completion-backend 'ein:use-ac-jedi-backend)``

Also, EIN heavily relies on standard Emacs libraries including EWOC,
EIEIO and json.el.

.. _IPython: http://ipython.org/
.. _Tornado: http://www.tornadoweb.org/en/stable/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _request.el: https://github.com/tkf/emacs-request
.. _request-deferred.el: https://github.com/tkf/emacs-request/blob/master/request-deferred.el
.. _dash: https://github.com/magnars/dash.el
.. _skewer-mode: https://github.com/skeeto/skewer-mode
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _python.el: https://github.com/fgallina/python.el
.. _auto-complete.el: http://cx4a.org/software/auto-complete/
.. _smartrep.el: https://github.com/myuhe/smartrep.el
.. _jedi.el: https://github.com/tkf/emacs-jedi
.. _jedi: https://github.com/davidhalter/jedi
.. _s: https://github.com/magnars/s.el

Usage
=====

1. Install from MELPA_.
   For manual install, put Emacs lisp ``ein*.el`` files and Python file
   ``ein.py`` in your load path. See `online documentation`_ for more
   information.

2. Configure the variables ``ein:jupyter-default-server-command`` and
   ``ein:jupyter-default-notebook-directory``, then call
   ``ein:jupyter-server-start``. This should start the jupyter notebook server,
   log in, then automatically open the notebook list.

-OR-

2. Start the `Jupyter notebook server`_.

3. (Optional) Newer versions of Jupyter have token authentication_ enabled by
   default so you will need to call ``M-x ein:notebooklist-login`` and enter the
   token as the password.

4. Execute ``M-x ein:notebooklist-open`` to open notebook list.

.. _`Jupyter notebook server`:
   https://jupyter.readthedocs.io/en/latest/content-quickstart.html

.. _MELPA: http://melpa.org/#/

.. _authentication: http://blog.jupyter.org/2016/12/21/jupyter-notebook-4-3-1/

Subpackages
-----------

Enable `auto-complete.el`_::

   (setq ein:use-auto-complete t)
   ;; Or, to enable "superpack" (a little bit hacky improvements):
   ;; (setq ein:use-auto-complete-superpack t)

Enable `smartrep.el`_::

   (setq ein:use-smartrep t)


Keybindings - Notebook
----------------------

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


Keybindings - Connect
---------------------

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
