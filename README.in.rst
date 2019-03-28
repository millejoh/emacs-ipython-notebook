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

Install
=======
Install from MELPA_ (recommended) or ``make install`` from github source.

Usage
=====
There are multiple, mutually exclusive ways to launch EIN.

Launch a local session
----------------------
``M-x ein:jupyter-server-start`` (aliased ``M-x ein:run``) launches a jupyter process from emacs.

Login to a local or remote session
----------------------------------
``M-x ein:notebooklist-login`` (aliased ``M-x ein:login``) to a running jupyter server.

Jupyter services relayed over HTTP such as ``mybinder.org`` require cookie authentication.  Issuing ``C-u M-x ein:login`` prompts for cookie information.  See the `Wiki`_ for more information.

Open a notebook file
--------------------
Open an ``.ipynb`` file normally in emacs and press ``C-c C-o``.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _MELPA: http://melpa.org/#/

It doesn't work
---------------

EIN is tested and developed on GNU Emacs and works best on Emacs versions 25.3
and later. Your mileage may vary with the `spacemacs layer`_ and other
*emacsen*. For a full list of dependencies see the `documentation`_.

You may also try to self-diagnose:

First invoke ``M-x ein:dev-start-debug``.  Then reproduce the error.

Higher level diagnostics appear in ``M-x ein:log-pop-to-all-buffer``.

Lower level diagnostics (the actual ``curl`` requests) appear in ``M-x ein:log-pop-to-request-buffer``.

If you cannot resolve the problem, file an issue using ``M-x ein:dev-bug-report-template``.  Please ensure the resulting system output does not include information sensitive to your institution.

.. _`documentation`: http://millejoh.github.io/emacs-ipython-notebook/#requirements

Highlighted Features
====================

* Easily copy cells between different notebooks.
* Execute code from an arbitrary buffer in a running kernel.  See `Keybindings - Connect`_.
* Jump to definition via ``M-.``
* Completion via company-mode_.
* Limited jupyterhub_ support.

.. _spacemacs layer: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/ipython-notebook
.. _auto-complete: https://github.com/auto-complete/auto-complete
.. _company-mode: https://github.com/company-mode/company-mode
.. _jupyterhub: https://github.com/jupyterhub/jupyterhub

Org-mode Integration
====================

EIN provides org-babel functionality similar to ob-ipython_ and scimax_.  Acknowledgements to those fine packages.

*Language* is ``ein``.  The ``:session`` header argument is the notebook url, e.g., ``https://localhost:8888/my.ipynb``, or simply ``localhost``, in which case EIN will evaluate org blocks in an anonymous notebook::

   #BEGIN_SRC ein :session localhost :results raw drawer :image output.png
   import matplotlib.pyplot as plt
   import numpy as np

   %matplotlib inline
   x = np.linspace(0, 1, 100)
   y = np.random.rand(100,1)
   plt.plot(x,y)
   #+END_SRC

You may also specify the port, i.e., ``localhost:8889``.  See `complete details`_.

.. _ob-ipython: https://github.com/gregsexton/ob-ipython/
.. _scimax: https://github.com/jkitchin/scimax
.. _complete details: http://millejoh.github.io/emacs-ipython-notebook/#org-mode-integration

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

.. KEYS NOTEBOOK (see Makefile)

Keybindings - Connect
---------------------

You can execute code from an arbitrary buffer in a running kernel via 
``M-x ein:connect-to-notebook``.

::

.. KEYS CONNECT (see Makefile)

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
