========================================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev| |melpa-stable|
========================================================================

  --- or **E**\ IN **I**\ s not only for **N**\ otebooks.

Emacs IPython Notebook (EIN) lets you edit and run Jupyter_ (formerly IPython) notebooks within Emacs.  It channels all the power of Emacs without the idiosyncrasies of in-browser editing.

EIN was originally written by tkf_.  More `complete documentation`_ is available.

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
.. _Jupyter: http://jupyter.org
.. _tkf: https://tkf.github.io/emacs-ipython-notebook
.. _`complete documentation`: http://millejoh.github.io/emacs-ipython-notebook

Install
=======
Install from MELPA_ (recommended) or ``make install`` from github source.  You will need to install Cask_ for the latter.  See the `online documentation`__ for more information.

Usage
=====
Launch a local session
----------------------
``M-x ein:jupyter-server-start`` (aliased ``M-x ein:run``) launches a jupyter process from emacs.

Login to a local or remote session
----------------------------------
``M-x ein:notebooklist-login`` (aliased ``M-x ein:login``) to a running jupyter server.

Jupyter services relayed over HTTP such as ``mybinder.org`` and ``coursera.org`` require cookie authentication.  In these instances you need to issue ``C-u M-x ein:login`` to be prompted for cookie information.  See the `Wiki`_ for more information.

Open a notebook file
--------------------
Open an ``.ipynb`` file and press ``C-c C-z``.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _MELPA: http://melpa.org/#/
__ `complete documentation`_

Highlighted Features
====================

* Copy/paste cells, even to/from different notebooks.
* Console integration: You can easily connect to a kernel via the console
  application.  This enables you to start debugging in the same kernel.  It is
  even possible to connect to a console over ssh.
* An IPython kernel can be "connected" to any buffer.  This enables you to
  evaluate a buffer or buffer region using the same kernel as the notebook.
  Notebook goodies such as tooltip help, help browser and code completion are
  available in these buffers.
* Jump to definition (go to the definition by hitting ``M-.`` over an object).
* Projectile-like file navigation via ``C-c C-o``.
* Limited JupyterHub support.

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

Keybindings - Notebook
----------------------

::

   key             binding
   ---             -------
   
   C-c		Prefix Command
   C-x		Prefix Command
   ESC		Prefix Command
   C-:		ein:shared-output-eval-string
   <C-down>	ein:worksheet-goto-next-input
   <C-up>		ein:worksheet-goto-prev-input
   <M-S-return>	ein:worksheet-execute-cell-and-insert-below
   <M-down>	ein:worksheet-move-cell-down
   <M-up>		ein:worksheet-move-cell-up
   
   C-x C-s		ein:notebook-save-notebook-command
   C-x C-w		ein:notebook-rename-command
   
   M-RET		ein:worksheet-execute-cell-and-goto-next
   M-,		ein:pytools-jump-back-command
   M-.		ein:pytools-jump-to-source-command
   M-n		ein:worksheet-next-input-history
   M-p		ein:worksheet-previous-input-history
   
   C-c C-a		ein:worksheet-insert-cell-above
   C-c C-b		ein:worksheet-insert-cell-below
   C-c C-c		ein:worksheet-execute-cell
   C-c C-d		ein:worksheet-toggle-slide-type
   C-c C-e		ein:worksheet-toggle-output
   C-c C-f		ein:file-open
   C-c C-h		ein:pytools-request-tooltip-or-help
   C-c TAB		ein:completer-complete
   C-c C-k		ein:worksheet-kill-cell
   C-c C-l		ein:worksheet-clear-output
   C-c RET		ein:worksheet-merge-cell
   C-c C-n		ein:worksheet-goto-next-input
   C-c C-o		ein:notebook-open
   C-c C-p		ein:worksheet-goto-prev-input
   C-c C-q		ein:notebook-kill-kernel-then-close-command
   C-c C-r		ein:notebook-reconnect-session-command
   C-c C-s		ein:worksheet-split-cell-at-point
   C-c C-t		ein:worksheet-toggle-cell-type
   C-c C-u		ein:worksheet-change-cell-type
   C-c C-v		ein:worksheet-set-output-visibility-all
   C-c C-w		ein:worksheet-copy-cell
   C-c C-x		Prefix Command
   C-c C-y		ein:worksheet-yank-cell
   C-c C-z		ein:notebook-kernel-interrupt-command
   C-c ESC		Prefix Command
   C-c !		ein:worksheet-rename-sheet
   C-c '		ein:edit-cell-contents
   C-c +		ein:notebook-worksheet-insert-next
   C-c -		ein:notebook-worksheet-delete
   C-c 1		ein:notebook-worksheet-open-1th
   C-c 2		ein:notebook-worksheet-open-2th
   C-c 3		ein:notebook-worksheet-open-3th
   C-c 4		ein:notebook-worksheet-open-4th
   C-c 5		ein:notebook-worksheet-open-5th
   C-c 6		ein:notebook-worksheet-open-6th
   C-c 7		ein:notebook-worksheet-open-7th
   C-c 8		ein:notebook-worksheet-open-8th
   C-c 9		ein:notebook-worksheet-open-last
   C-c S		ein:worksheet-toggle-slideshow-view
   C-c i		ein:inspect-object
   C-c {		ein:notebook-worksheet-open-prev-or-last
   C-c }		ein:notebook-worksheet-open-next-or-first
   C-c C-S-l	ein:worksheet-clear-all-output
   C-c C-#		ein:notebook-close
   C-c C-$		ein:tb-show
   C-c C-'		ein:worksheet-turn-on-autoexec
   C-c C-,		ein:pytools-jump-back-command
   C-c C-.		ein:pytools-jump-to-source-command
   C-c C-/		ein:notebook-scratchsheet-open
   C-c C-;		ein:shared-output-show-code-cell-at-point
   C-c <down>	ein:worksheet-move-cell-down
   C-c <up>	ein:worksheet-move-cell-up
   
   C-c C-x C-l	ein:notebook-toggle-latex-fragment
   C-c C-x C-r	ein:notebook-restart-session-command
   
   C-c M-+		ein:notebook-worksheet-insert-prev
   C-c M-w		ein:worksheet-copy-cell
   C-c M-{		ein:notebook-worksheet-move-prev
   C-c M-}		ein:notebook-worksheet-move-next

Keybindings - Connect
---------------------

In Python (or any other) buffer, you can connect to any open notebook
by ``M-x ein:connect-to-notebook`` then choose appropriate notebook.
After connecting to the notebook (and hence its kernel), the following
commands are available.

::

   key             binding
   ---             -------
   
   C-c		Prefix Command
   ESC		Prefix Command
   C-:		ein:shared-output-eval-string
   
   M-,		ein:pytools-jump-back-command
   M-.		ein:pytools-jump-to-source-command
   
   C-c C-a		ein:connect-toggle-autoexec
   C-c C-c		ein:connect-run-or-eval-buffer
   C-c C-h		ein:pytools-request-tooltip-or-help
   C-c TAB		ein:completer-complete
   C-c C-l		ein:connect-reload-buffer
   C-c C-o		ein:console-open
   C-c C-r		ein:connect-eval-region
   C-c C-x		ein:tb-show
   C-c C-z		ein:connect-pop-to-notebook
   C-c C-,		ein:pytools-jump-back-command
   C-c C-.		ein:pytools-jump-to-source-command
   C-c C-/		ein:notebook-scratchsheet-open

Links
=====

* `Online Documentation
  <http://millejoh.github.io/emacs-ipython-notebook/>`_

* `Wiki
  <https://github.com/millejoh/emacs-ipython-notebook/wiki>`_

  + `Screenshots
    <https://github.com/millejoh/emacs-ipython-notebook/wiki/Screenshots>`_
  + `Tips
    <https://github.com/millejoh/emacs-ipython-notebook/wiki/Tips>`_

License
=======

Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.
