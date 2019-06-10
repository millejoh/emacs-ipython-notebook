========================================================================
 EIN -- Emacs IPython Notebook |build-status| |melpa-dev| |melpa-stable|
========================================================================

  --- or **E**\ IN **I**\ s not only for pytho\ **N**\ .

.. COMMENTARY (see Makefile)

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

It doesn't work
---------------

EIN is tested on GNU Emacs versions
.. CI VERSION (see Makefile)
and later.  We presently do not recommend the `spacemacs layer`_.

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
.. _ob-ein details: http://millejoh.github.io/emacs-ipython-notebook/#org-mode-integration
.. _jupyter kernel: https://github.com/jupyter/jupyter/wiki/Jupyter-kernels

Connected Buffers
=================

Use ``M-x ein:connect-to-notebook`` to submit code from an arbitrary buffer to a running jupyter kernel.  See `connected buffer details`_.

.. _connected buffer details: http://millejoh.github.io/emacs-ipython-notebook/#connected-buffer

Keymap (C-h m)
==============

::

.. KEYS NOTEBOOK (see Makefile)

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
