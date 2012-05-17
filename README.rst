========================
 Emacs IPython Notebook
========================

.. warning:: This is **very** early version.
             Make sure you have backup!

Screenshot
==========

.. figure:: http://farm8.staticflickr.com/7125/7006219050_2d424b4ece_z.jpg
   :alt: Plotting in Emacs IPython Notebook


Features
========

* Copy/paste cells, even to/from different notebooks.
* Console integration: You can easily connect to kernel via console
  application.  This enables you to start debugging in the same
  kernel.

These features are currently not in the native IPython notebook, so
use Emacs IPython Notebook client (EIN) if you want them!  Of course,
EIN does not (and won't) implement rich object representation as
native IPython Notebook web client, and you should use browser also to
get full power of IPython Notebook.  EIN aims at making notebook
*editing* more effective.

Other features:

* Inline images
* Auto/manual-completion
* Popup (tooltip) help
* Syntax highlighting in each cell types (Python/Markdown)
* Help browser (opens when executing ``function?``)

More to come/ideas:

* Better pager (history, syntax highlighting, ...)
* Better messages/event handling/UI
* Highlight latex maths in markdown text
* Auto-save
* Local auto-backup
* VCS integration


Requirements
============

* IPython_ **0.12.1**: EIN won't work with older versions.
* `websocket.el`_
* (optional) mumamo_:
  It will be automatically loaded when it is on the path.
  The official way to setup path is to load nXhtml_.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or python-mode.el.
  Fabian Gallina's `python.el`_ is required to use
  ``ein:notebook-console-open`` command.
* (optional) `auto-complete.el`_
  You need to configure subpackage ``ein-ac`` to enable
  this feature.
* (optional) `smartrep.el`_:
  This package enables you to omit typing prefix keys (e.g.,
  ``C-c C-n C-n C-n ...`` instead of ``C-c C-n C-c C-n C-c C-n ...``).
  You need to configure subpackage ``ein-smartrep`` to enable
  this feature.

Also, EIN heavily relies on standard Emacs libraries including EWOC
and EIEIO.  EIN is currently tested in Emacs 24.1.

.. _IPython: http://ipython.org/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _mumamo: http://www.emacswiki.org/emacs/MuMaMo
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _python.el: https://github.com/fgallina/python.el
.. _auto-complete.el: http://cx4a.org/software/auto-complete/
.. _smartrep.el: https://github.com/myuhe/smartrep.el


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


Subpackages
-----------

Enable auto-complete::

   (require 'ein-ac)
   (ein:ac-config)
   ;; Or, to enable "superpack" (a little bit hacky improvements):
   ;; (ein:ac-config t)

Enable `smartrep.el`_::

   (require 'ein-smartrep)
   (ein:smartrep-config)


Keybinds
--------

.. (ein:dev-insert-notebook-mode-map)

::

   key             binding
   ---             -------

   C-c             Prefix Command
   C-x             Prefix Command

   C-x C-s         ein:notebook-save-notebook-command
   C-x C-w         ein:notebook-rename-command

   C-c C-a         ein:notebook-insert-cell-above-command
   C-c C-b         ein:notebook-insert-cell-below-command
   C-c C-c         ein:notebook-execute-current-cell
   C-c C-d         ein:notebook-delete-cell-command
   C-c C-f         ein:notebook-request-tool-tip-command
   C-c TAB         ein:notebook-complete-cell-command
   C-c C-k         ein:notebook-kill-cell-command
   C-c C-n         ein:notebook-goto-next-cell
   C-c C-o         ein:notebook-console-open
   C-c C-p         ein:notebook-goto-prev-cell
   C-c C-q         ein:notebook-kernel-kill-command
   C-c C-r         ein:notebook-render
   C-c C-t         ein:notebook-toggle-cell-type
   C-c C-y         ein:notebook-yank-cell-command
   C-c C-z         ein:notebook-kernel-interrupt-command
   C-c ESC         Prefix Command

   C-c M-w         ein:notebook-copy-cell-command

.. // KEYS END //
