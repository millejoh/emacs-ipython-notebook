========================================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev| |melpa-stable|
========================================================================

  --- or **E**\ IN **I**\ s not only for pytho\ **N**\ .

Emacs IPython Notebook (EIN) lets you edit and run Jupyter_ (formerly IPython)
notebooks within Emacs.  It channels all the power of Emacs without the
idiosyncrasies of in-browser editing.

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

Install
=======
Install from MELPA_ (recommended) or ``make install`` from github source.

Usage
=====
Start EIN using **one** of the following:

- Open an ``.ipynb`` file normally in emacs and press ``C-c C-o``, or,
- ``M-x ein:run`` launches a jupyter process from emacs, or,
- ``M-x ein:login`` to a running jupyter server

Use ``C-u M-x ein:login`` for services such as ``mybinder.org`` requiring cookie authentication.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _MELPA: http://melpa.org/#/

It doesn't work
---------------

EIN is tested on GNU Emacs versions
25.1
and later. Your mileage may vary with the `spacemacs layer`_ and other *emacsen*.

You may also try to self-diagnose:

First invoke ``M-x ein:dev-start-debug``.  Then reproduce the error.

High level diagnostics appear in ``M-x ein:log-pop-to-all-buffer``.

Low level diagnostics appear in ``M-x ein:log-pop-to-request-buffer``.

If you cannot resolve the problem, file an issue using ``M-x ein:dev-bug-report-template``.

.. _spacemacs layer: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/ipython-notebook
.. _auto-complete: https://github.com/auto-complete/auto-complete
.. _company-mode: https://github.com/company-mode/company-mode
.. _jupyterhub: https://github.com/jupyterhub/jupyterhub

I want to use Elpy, ESS, LSP, julia-mode
========================================

Enable `polymode`_ via::

   M-x customize-group RET ein
   Toggle Ein:Polymode
  
Org-mode Integration
====================

EIN provides org-babel functionality similar to ob-ipython_ and scimax_.

*Language* is ``ein``.  The ``:session`` header argument is the notebook url, e.g., ``https://localhost:8888/my.ipynb``, or simply ``localhost``, in which case EIN will evaluate org blocks in an anonymous notebook::

   #BEGIN_SRC ein :session localhost :results raw drawer :image output.png
   import matplotlib.pyplot as plt
   import numpy as np

   %matplotlib inline
   x = np.linspace(0, 1, 100)
   y = np.random.rand(100,1)
   plt.plot(x,y)
   #+END_SRC

You may also specify the port, i.e., ``localhost:8889``.  See `ob-ein details`_.

.. _polymode: https://github.com/polymode/polymode
.. _ob-ipython: https://github.com/gregsexton/ob-ipython
.. _scimax: https://github.com/jkitchin/scimax
.. _ob-ein details: http://millejoh.github.io/emacs-ipython-notebook/#org-mode-integration

Connected Buffers
=================

Use ``M-x ein:connect-to-notebook`` to submit code from an arbitrary buffer to a running jupyter kernel.  See `connected buffer details`_.

.. _connected buffer details: http://millejoh.github.io/emacs-ipython-notebook/#connected-buffer

Keymap (C-h m)
==============

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
   C-c S		ein:worksheet-toggle-slide-type
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

Links
=====
* `Complete documentation <http://millejoh.github.io/emacs-ipython-notebook/>`_

* `Wiki <https://github.com/millejoh/emacs-ipython-notebook/wiki>`_

  + `Screenshots <https://github.com/millejoh/emacs-ipython-notebook/wiki/Screenshots>`_
  + `Tips <https://github.com/millejoh/emacs-ipython-notebook/wiki/Tips>`_

License
=======

Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.
