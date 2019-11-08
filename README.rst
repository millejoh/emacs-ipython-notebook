========================================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev| |melpa-stable|
========================================================================

  --- or **E**\ IN **I**\ s not only for pytho\ **N**\ .

Emacs IPython Notebook (EIN) lets you run Jupyter (formerly IPython)
notebooks within Emacs.  It channels all the power of Emacs without the
idiosyncrasies of in-browser editing.

Org_ users please find ob-ein_, a jupyter Babel_ backend.

EIN was originally written by `[tkf]`_.  A jupyter Babel_ backend was first
introduced by `[gregsexton]`_.

.. |build-status|
   image:: https://secure.travis-ci.org/dickmao/emacs-ipython-notebook.png?branch=master
   :target: http://travis-ci.org/dickmao/emacs-ipython-notebook
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
.. _Babel: https://orgmode.org/worg/org-contrib/babel/intro.html
.. _Org: https://orgmode.org
.. _[tkf]: http://tkf.github.io
.. _[gregsexton]: https://github.com/gregsexton/ob-ipython

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

Alternatively, ob-ein_.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _MELPA: http://melpa.org/#/

Reporting bugs
--------------
EIN is tested on GNU Emacs versions
25
and later.  We presently do not recommend the `spacemacs layer`_.

**Please file issues using** ``M-x ein:dev-bug-report-template``.

You may also try to self-diagnose.

First invoke ``M-x ein:dev-start-debug``.  Then reproduce the error.

General logging ``M-x ein:log-pop-to-all-buffer``.

Notebook server ``M-x ein:log-pop-to-request-buffer``.

Kernel messaging (must be run from notebook buffer) ``M-x ein:dev-pop-to-debug-channels``.

.. _spacemacs layer: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/ipython-notebook
.. _auto-complete: https://github.com/auto-complete/auto-complete
.. _company-mode: https://github.com/company-mode/company-mode
.. _jupyterhub: https://github.com/jupyterhub/jupyterhub

I want to use Elpy, ESS, julia-mode
========================================
Enable `polymode`_ via::

   M-x customize-group RET ein
   Toggle Ein:Polymode

ob-ein
======
Configuration:

::

   M-x customize-group RET org-babel
   Org Babel Load Languages:
     Insert (ein . t)
     For example, '((emacs-lisp . t) (ein . t))

Snippet:

::

   #BEGIN_SRC ein-python :session localhost :results raw drawer
     import numpy, math, matplotlib.pyplot as plt
     %matplotlib inline
     x = numpy.linspace(0, 2*math.pi)
     plt.plot(x, numpy.sin(x))
   #+END_SRC

The ``:session`` is the notebook url, e.g., ``http://localhost:8888/my.ipynb``, or simply ``localhost``, in which case org evaluates anonymously.  A port may also be specified, e.g., ``localhost:8889``.

*Language* can be ``ein-python``, ``ein-r``, or ``ein-julia``.  **The relevant** `jupyter kernel`_ **must be installed before use**.  Additional languages can be configured via::

   M-x customize-group RET ein
   Ob Ein Languages

.. _polymode: https://github.com/polymode/polymode
.. _ob-ipython: https://github.com/gregsexton/ob-ipython
.. _scimax: https://github.com/jkitchin/scimax
.. _jupyter kernel: https://github.com/jupyter/jupyter/wiki/Jupyter-kernels

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

License
=======
Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.
