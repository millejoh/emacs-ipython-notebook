========================
 Emacs IPython Notebook
========================

.. warning:: This is **very** early version.
             Do not use it in a serious situation!

Screenshot
==========

.. figure:: http://farm8.staticflickr.com/7125/7006219050_2d424b4ece_z.jpg
   :alt: Plotting in Emacs IPython Notebook


Requirements
============

* IPython_ **0.12.1**: EIN won't work with older versions.
* `websocket.el`_
* (optional) mumamo_:
  It will be automatically loaded when it is on the path.
  The official way to setup path is to load nXhtml_.
  So you will need to load nXhtml_ *before* loading EIN.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or python-mode.el.

EIN is currently tested in Emacs 24.

.. _IPython: http://ipython.org/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _mumamo: http://www.emacswiki.org/emacs/MuMaMo
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html


Usage
=====

1. Install module.
   Put Emacs lisp ``ein*.el`` files in your load path.

2. Require module::

     (require 'ein)

3. Start `IPython notebook server`_.

4. Hit ``M-x ein:notebooklist-open`` to open notebook list.

.. _`IPython notebook server`:
   http://ipython.org/ipython-doc/stable/interactive/htmlnotebook.html
