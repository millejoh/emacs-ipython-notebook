========================
 Emacs IPython Notebook
========================

.. warning:: This is **very** early version.
             Do not use it in a serious situation!

Screenshot
==========

.. figure:: http://farm8.staticflickr.com/7125/7006219050_2d424b4ece_z.jpg
   :alt: Plotting in Emacs IPython Notebook


Requirement
===========

* `websocket.el`_
* (optional) mumamo_:
  You will need to load nXhtml_ *before* loading ein.el.

.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _mumamo: http://www.emacswiki.org/emacs/MuMaMo
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html


Usage
=====

1. Install module.
   Put Emacs lisp files starts with ``ein`` in your load path.

2. Require module::

     (require 'ein)

3. Hit ``M-x ein:notebooklist-open`` to open notebook list.

   Of course, you need to start IPython notebook server before calling
   this command.
