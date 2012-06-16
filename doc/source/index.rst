Welcome to Emacs IPython Notebook's documentation!
==================================================

.. el:package:: ein

Emacs IPython Notebook (EIN) provides fully featured IPython Notebook
client and integrated REPL (like SLIME_).

.. _`Emacs IPython Notebook (EIN)`:
  https://github.com/tkf/emacs-ipython-notebook

.. _SLIME: http://common-lisp.net/project/slime/

Highlighted features:

* Copy/paste cells, even to/from different notebooks.
* Console integration: You can easily connect to kernel via console
  application.  This enables you to start debugging in the same
  kernel.  It is even possible to connect console over ssh [#]_.
* IPython kernel can be "connected" to any buffers.  This enables you
  to evaluate buffer/region using same kernel as notebook.  Notebook
  goodies such as tooltip help, help browser and code completion are
  available in these buffers. [#]_
* Jump to definition (go to the definition by hitting ``M-.`` over an
  object).

Other notebook features:

* Inline images
* Auto/manual-completion
* Popup (tooltip) help
* Syntax highlighting in each cell types (Python/Markdown/ReST/HTML)
* Help browser (opens when executing ``function?``)
* Traceback viewer

Links:

* `Repository at GitHub
  <https://github.com/tkf/emacs-ipython-notebook>`_
* `Issue Tracker at GitHub
  <https://github.com/tkf/emacs-ipython-notebook/issues>`_
* `Online Documentation
  <http://tkf.github.com/emacs-ipython-notebook/>`_

.. [#] You need to setup :el:symbol:`ein:notebook-console-args` properly
.. [#] Use the command :el:symbol:`ein:connect-to-notebook`.


Quick try
---------

This is a quick and clean way to try EIN separately from your Emacs
setting.  If you want to try EIN but think preparing all the
requirements is too much, try this!::

   git clone git://github.com/tkf/zeroein.git
   zeroein/zeroein.py

This will launch a new Emacs instance.  For more information, see::

   zeroein/zeroein.py --help


Requirements
------------

* IPython_ **0.12.1** (or developmental version):
  EIN won't work with older versions.
* `websocket.el`_
* (optional) mumamo_:
  It will be automatically loaded when it is on the path.
  The official way to setup path is to load nXhtml_.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or python-mode.el.
  Fabian Gallina's `python.el`_ is required to use
  :el:symbol:`ein:notebook-console-open` command.
* (optional) `auto-complete.el`_
  You need to configure :el:symbol:`ein:use-auto-complete` to enable
  this feature.
* (optional) `smartrep.el`_:
  This package enables you to omit typing prefix keys (e.g.,
  ``C-c C-n C-n C-n ...`` instead of ``C-c C-n C-c C-n C-c C-n ...``).
  You need to configure :el:symbol:`ein:use-smartrep` to enable
  this feature.

Also, EIN heavily relies on standard Emacs libraries including EWOC,
EIEIO and json.el.  EIN is currently tested in Emacs 24.1.

.. _IPython: http://ipython.org/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _mumamo: http://www.emacswiki.org/emacs/MuMaMo
.. _nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _python.el: https://github.com/fgallina/python.el
.. _auto-complete.el: http://cx4a.org/software/auto-complete/
.. _smartrep.el: https://github.com/myuhe/smartrep.el


Install
-------

Using el-get
^^^^^^^^^^^^

If you use developmental version of `el-get`_, you can install it
easily using it.  Emacs IPython Notebook is registered as package
``ein``.

.. _el-get: https://github.com/dimitri/el-get

Manual install
^^^^^^^^^^^^^^

Put Emacs lisp ``ein*.el`` files and Python file ``ein.py`` in
a directory defined in your :el:symbol:`load-path`.

Setup
^^^^^

Here is the minimal configuration.  See customization_ for more details.

.. sourcecode:: cl

   (require 'ein)


Usage
-----

1. Start `IPython notebook server`_.

2. Hit ``M-x ein:notebooklist-open`` to open notebook list.  This will
   open :ref:`notebook list <notebook-list-commands>` buffer.

3. In the notebook list buffer, you can open notebooks by hitting
   ``[Open]`` buttons.  See :ref:`notebook <notebook-commands>`
   section for what you can do in the notebook buffer.

.. _`IPython notebook server`:
   http://ipython.org/ipython-doc/stable/interactive/htmlnotebook.html


Commands/Keybinds
-----------------

.. _notebook-list-commands:

Notebook list
^^^^^^^^^^^^^

You can start notebook by ``M-x ein:notebooklist-open`` and enter the
port or URL of the IPython notebook server.

.. el:function:: ein:notebooklist-open
.. el:function:: ein:notebooklist-new-notebook
.. el:function:: ein:notebooklist-open-notebook-global
.. el:function:: ein:notebooklist-new-scratch-notebook

.. el:keymap:: ein:notebooklist-mode-map
   :exclude: widget-button

.. _notebook-commands:

Notebook
^^^^^^^^

The following keybinds are available in notebook buffers.

.. el:keymap:: ein:notebook-mode-map

.. el:function:: ein:notebook-delete-cell-command

Connected buffer
^^^^^^^^^^^^^^^^

You can connect any buffer (typically buffer opening Python file) to
opened notebook and use the kernel of the notebook to execute the
code, inspect objects, auto-complete code, jump to the other source,
etc.  Once the buffer is connected to the notebook, minor mode
:el:symbol:`ein:connect-mode` is enabled and the following keybinds
are available.

.. el:keymap:: ein:connect-mode-map

Other useful commands:

.. el:function:: ein:connect-to-notebook
.. el:function:: ein:connect-eval-buffer
.. el:function:: ein:connect-run-buffer

Shared output buffer
^^^^^^^^^^^^^^^^^^^^

.. el:function:: ein:shared-output-pop-to-buffer

.. el:keymap:: ein:shared-output-mode-map

Traceback viewer
^^^^^^^^^^^^^^^^

Traceback in notebook buffer is not easy to understand.  You can open
Traceback viewer by the command :el:symbol:`ein:notebook-view-traceback`.
In the Traceback viewer, following keybinds are available.

.. el:keymap:: ein:traceback-mode-map

PyTools
^^^^^^^

These commands can be used in the notebook buffer and the connected
buffer.

.. el:function:: ein:pytools-whos
.. el:function:: ein:pytools-hierarchy


Customization
-------------

You can customize EIN using the Emacs customization UI by typing
``M-x customize-group RET ein RET``.
All the configurable variables are listed below.

Subpackages
^^^^^^^^^^^

.. el:variable:: ein:use-auto-complete
.. el:variable:: ein:use-auto-complete-superpack
.. el:variable:: ein:ac-max-cache
.. el:variable:: ein:use-smartrep
.. el:variable:: ein:load-dev

Notebook list
^^^^^^^^^^^^^

.. el:variable:: ein:url-or-port
.. el:variable:: ein:scratch-notebook-name-template

Notebook
^^^^^^^^

.. el:variable:: ein:notebook-discard-output-on-save
.. el:variable:: ein:notebook-modes
.. el:variable:: ein:notebook-kill-buffer-ask
.. el:variable:: ein:notebook-console-security-dir
.. el:variable:: ein:notebook-console-executable
.. el:variable:: ein:notebook-console-args
.. el:variable:: ein:cell-traceback-level

Connect
^^^^^^^

.. el:variable:: ein:connect-run-command
.. el:variable:: ein:connect-save-before-run
.. el:variable:: ein:propagate-connect

MuMaMo
^^^^^^

.. el:variable:: ein:mumamo-codecell-mode
.. el:variable:: ein:mumamo-textcell-mode
.. el:variable:: ein:mumamo-htmlcell-mode
.. el:variable:: ein:mumamo-markdowncell-mode
.. el:variable:: ein:mumamo-rawcell-mode
.. el:variable:: ein:mumamo-headingcell-mode
.. el:variable:: ein:mumamo-fallback-mode

Misc
^^^^

.. el:variable:: ein:query-timeout


Advanced
--------

By telling IPython a little bit about Emacs Lisp, you can execute
Emacs Lisp from IPython, just like you can execute Javascript in the
web client.  See `emacslisp.py`_ for more details.

.. sourcecode:: python

   In [1]:
   %run PATH/TO/emacslisp.py

   In [2]:
   EmacsLisp('(+ 1 2 3)')
   Out [2]:
   6

.. _`emacslisp.py`:
  https://github.com/tkf/emacs-ipython-notebook/blob/master/emacslisp.py


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

