==========================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev|
==========================================================

.. image:: https://github.com/dickmao/emacs-ipython-notebook/blob/master/thumbnail.png
   :target: https://youtu.be/8VzWc9QeOxE
   :alt: Kaggle Notebooks in AWS

Emacs IPython Notebook (EIN), despite its name, is a jupyter client for all
languages.  It does not work under non-WSL Windows environments.

As of 2023, EIN has been sunset for a number of years having been
unable to keep up with jupyter's web-first ecosystem.  Even during
its heyday EIN never fully reconciled emac's monolithic buffer
architecture to the notebook's by-cell discretization, leaving
gaping functional holes like crippled undo.

Certainly in 2012 when jupyter was much smaller, an emacs client
made perfect sense.  With many years of hindsight, it's now clear
the json-driven, git-averse notebook format is anathema to emacs's
plain text ethos.

.. |build-status|
   image:: https://github.com/millejoh/emacs-ipython-notebook/workflows/CI/badge.svg
   :target: https://github.com/millejoh/emacs-ipython-notebook/actions
   :alt: Build Status
.. |melpa-dev|
   image:: https://melpa.org/packages/ein-badge.svg
   :target: http://melpa.org/#/ein
   :alt: MELPA current version
.. _Jupyter: http://jupyter.org
.. _Babel: https://orgmode.org/worg/org-contrib/babel/intro.html
.. _Org: https://orgmode.org
.. _[tkf]: http://tkf.github.io
.. _[gregsexton]: https://github.com/gregsexton/ob-ipython

Install
=======
As described in `Getting started`_, ensure melpa's whereabouts in ``init.el`` or ``.emacs``::

   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

Then

::

   M-x package-install RET ein RET

Alternatively, directly clone this repo and ``make install``.

For jupyterlab 3.0+, reconfigure the subcommand from "notebook" to "server".

::

   M-x customize-option RET ein:jupyter-server-use-subcommand RET

Usage
=====
Start EIN using **ONE** of the following:

* Open an ``.ipynb`` file, press ``C-c C-o``, or,
* ``M-x ein:run`` launches a jupyter process from emacs, or,
* ``M-x ein:login`` to a running jupyter server, or,

``M-x ein:stop`` prompts to halt local and remote jupyter services.

Alternatively, ob-ein_.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _Getting started: http://melpa.org/#/getting-started

FAQ
===

How do I...
-----------

... report a bug?
   Note EIN is tested only for *released* GNU Emacs versions
   26.3
   and later.  Pre-release versions will not work.

   First try ``emacs -Q -f package-initialize -f ein:dev-start-debug`` and reproduce the bug.  The ``-Q`` skips any user configuration that might interfere with EIN.

   Then file an issue using ``M-x ein:dev-bug-report-template``.

... display images inline?
   We find inserting images into emacs disruptive, and so default to spawning an external viewer.  To override this,
   ::

      M-x customize-group RET ein
      Ein:Output Area Inlined Images

... configure the external image viewer?
   ::

      M-x customize-group RET mailcap
      Mailcap User Mime Data

   On a typical Linux system, one might configure a viewer for MIME Type ``image/png`` as a shell command ``convert %s -background white -alpha remove -alpha off - | display -immutable``.

... get IDE-like behavior?
   You can't.  EIN's architecture is fundamentally incompatible with LSP.

.. _Issues: https://github.com/millejoh/emacs-ipython-notebook/issues
.. _prevailing documentation: http://millejoh.github.io/emacs-ipython-notebook
.. _spacemacs layer: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/ipython-notebook
.. _company-mode: https://github.com/company-mode/company-mode
.. _jupyterhub: https://github.com/jupyterhub/jupyterhub
.. _elpy: https://melpa.org/#/elpy
.. _math-preview: https://gitlab.com/matsievskiysv/math-preview
.. _program modes: https://www.gnu.org/software/emacs/manual/html_node/emacs/Program-Modes.html
.. _undo boundaries: https://www.gnu.org/software/emacs/manual/html_node/elisp/Undo.html

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

   #+BEGIN_SRC ein-python :session localhost
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

.. _gat utility: https://dickmaogat.readthedocs.io/en/latest/install.html
.. _gat usage: https://dickmaogat.readthedocs.io/en/latest/usage.html
.. _batch mode: https://nbconvert.readthedocs.io/en/latest/execute_api.html
.. _dickmao/Kaggler: https://github.com/dickmao/Kaggler/tree/gcspath#importing-datasets

Keymap (C-h m)
==============

::

   Key             Binding
   -------------------------------------------------------------------------------
   C-<down>	ein:worksheet-goto-next-input-km
   C-<up>		ein:worksheet-goto-prev-input-km
   M-S-<return>	ein:worksheet-execute-cell-and-insert-below-km
   M-<down>	ein:worksheet-not-move-cell-down-km
   M-<up>		ein:worksheet-not-move-cell-up-km
   
   C-x C-s		ein:notebook-save-notebook-command-km
   C-x C-w		ein:notebook-rename-command-km
   
   M-RET		ein:worksheet-execute-cell-and-goto-next-km
   M-,		ein:pytools-jump-back-command
   M-.		ein:pytools-jump-to-source-command
   
   C-c C-a		ein:worksheet-insert-cell-above-km
   C-c C-b		ein:worksheet-insert-cell-below-km
   C-c C-c		ein:worksheet-execute-cell-km
   C-u C-c C-c    		ein:worksheet-execute-all-cells
   C-c C-e		ein:worksheet-toggle-output-km
   C-c C-f		ein:file-open-km
   C-c C-k		ein:worksheet-kill-cell-km
   C-c C-l		ein:worksheet-clear-output-km
   C-c RET		ein:worksheet-merge-cell-km
   C-c C-n		ein:worksheet-goto-next-input-km
   C-c C-o		ein:notebook-open-km
   C-c C-p		ein:worksheet-goto-prev-input-km
   C-c C-q		ein:notebook-kill-kernel-then-close-command-km
   C-c C-r		ein:notebook-reconnect-session-command-km
   C-c C-s		ein:worksheet-split-cell-at-point-km
   C-c C-t		ein:worksheet-toggle-cell-type-km
   C-c C-u		ein:worksheet-change-cell-type-km
   C-c C-v		ein:worksheet-set-output-visibility-all-km
   C-c C-w		ein:worksheet-copy-cell-km
   C-c C-y		ein:worksheet-yank-cell-km
   C-c C-z		ein:notebook-kernel-interrupt-command-km
   C-c C-S-l	ein:worksheet-clear-all-output-km
   C-c C-#		ein:notebook-close-km
   C-c C-$		ein:tb-show-km
   C-c C-/		ein:notebook-scratchsheet-open-km
   C-c C-;		ein:shared-output-show-code-cell-at-point-km
   C-c <down>	ein:worksheet-move-cell-down-km
   C-c <up>	ein:worksheet-move-cell-up-km
   
   C-c C-x C-r	ein:notebook-restart-session-command-km
   
   C-c M-w		ein:worksheet-copy-cell-km
   
   
   This is a minor mode.  If called interactively, toggle the
   ‘Ein:Notebook mode’ mode.  If the prefix argument is positive,
   enable the mode, and if it is zero or negative, disable the mode.
   
   If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable
   the mode if ARG is nil, omitted, or is a positive number.
   Disable the mode if ARG is a negative number.
   
   To check whether the minor mode is enabled in the current buffer,
   evaluate ‘ein:notebook-mode’.
   
   The mode’s hook is called both when the mode is enabled and when
   it is disabled.
