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

.. KEYS NOTEBOOK (see Makefile)

Keybindings - Connect
---------------------

In Python (or any other) buffer, you can connect to any open notebook
by ``M-x ein:connect-to-notebook`` then choose appropriate notebook.
After connecting to the notebook (and hence its kernel), the following
commands are available.

::

.. KEYS CONNECT (see Makefile)

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
