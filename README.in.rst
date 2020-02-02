========================================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev| |melpa-stable|
========================================================================

.. COMMENTARY (see Makefile)

.. |build-status|
   image:: https://github.com/millejoh/emacs-ipython-notebook/workflows/CI/badge.svg
   :target: https://github.com/millejoh/emacs-ipython-notebook/actions
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
As described in `Getting started`_, ensure melpa's whereabouts in ``init.el`` or ``.emacs``::

   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

Then

::

   M-x package-refresh-contents RET
   M-x package-install RET ein RET

Alternatively, directly clone this repo and ``make install``.

Usage
=====
Start EIN using **one** of the following:

- Open an ``.ipynb`` file normally in emacs and press ``C-c C-o``, or,
- ``M-x ein:run`` launches a jupyter process from emacs, or,
- ``M-x ein:login`` to a running jupyter server

Use ``C-u M-x ein:login`` for services such as ``mybinder.org`` requiring cookie authentication.

Alternatively, ob-ein_.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _Getting started: http://melpa.org/#/getting-started

Reporting bugs
--------------
EIN is tested on GNU Emacs versions
.. CI VERSION (see Makefile)
and later.

**Please file issues using** ``M-x ein:dev-bug-report-template``.

First try ``emacs -Q -f package-initialize`` and reproduce the bug.  The ``-Q`` skips any user configuration that might interfere with EIN.

Invoking ``M-x ein:dev-start-debug`` activates verbose logging.

EIN log buffer ``*ein:log-all*``.

Server log buffer (if running jupyter locally) ``*ein server*``.

HTTP requests ``M-x ein:log-pop-to-request-buffer``.

Kernel messaging (must be run from notebook buffer) ``M-x ein:dev-pop-to-debug-channels``.

.. _spacemacs layer: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/ipython-notebook
.. _company-mode: https://github.com/company-mode/company-mode
.. _jupyterhub: https://github.com/jupyterhub/jupyterhub

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

.. KEYS NOTEBOOK (see Makefile)

License
=======
Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.
