Welcome to Emacs IPython Notebook's documentation!
==================================================

.. el:package:: ein

Emacs IPython Notebook (EIN) provides fully featured IPython Notebook
client and integrated REPL (like SLIME_).  While EIN makes notebook
editing very powerful by allowing you to use any Emacs features, it
also expose IPython features such as code evaluation, object
inspection and code completion to the Emacs side.  These features can
be accessed anywhere in Emacs and improve Python code editing and
reading in Emacs.

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

* `Online Documentation
  <http://tkf.github.com/emacs-ipython-notebook/>`_
* `Screenshots
  <https://github.com/tkf/emacs-ipython-notebook/wiki/Screenshots>`_
* `Downloads
  <https://github.com/tkf/emacs-ipython-notebook/tags>`_
* `Repository at GitHub
  <https://github.com/tkf/emacs-ipython-notebook>`_
* `Issue Tracker at GitHub
  <https://github.com/tkf/emacs-ipython-notebook/issues>`_

.. [#] You need to setup :el:symbol:`ein:notebook-console-args` properly
.. [#] Use the command :el:symbol:`ein:connect-to-notebook`.

.. contents::


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

* IPython_ 0.12 or higher.
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

You should byte compile EIN, especially when using MuMaMo, otherwise
editing large notebook will be very slow.  You can use the following
command to compile EIN.  If you don't specify all the optional
packages, there will be compiler warning but that is OK as long as you
don't use that optional package.

.. sourcecode:: sh

   emacs -Q -batch -L .          \  # don't forget the dot!
       -L PATH/TO/websocket/     \
       -L PATH/TO/nxhtml/util/   \  # optional (for MuMaMo)
       -L PATH/TO/auto-complete/ \  # optional
       -L PATH/TO/popup/         \  # optional (for auto-complete)
       -L PATH/TO/fuzzy/         \  # optional (for auto-complete)
       -L PATH/TO/smartrep/      \  # optional
       -L PATH/TO/rst-mode/      \  # optional
       -f batch-byte-compile *.el

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

.. el:function:: ein:pytools-doctest
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
.. el:variable:: ein:default-url-or-port
.. el:variable:: ein:scratch-notebook-name-template

Notebook
^^^^^^^^

.. el:variable:: ein:notebook-enable-undo
.. el:variable:: ein:notebook-discard-output-on-save
.. el:variable:: ein:notebook-modes
.. el:variable:: ein:notebook-kill-buffer-ask
.. el:variable:: ein:notebook-querty-timeout-open
.. el:variable:: ein:notebook-querty-timeout-save
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


Gotchas and caveats
-------------------

Although EIN mostly works fine, there are some deficits I noticed but
have not fixed yet.  It seems that they originate from some upstream
bugs so there is little I can do in EIN (but I'm not sure -- it's
possible that I am misusing the libraries!).

If you know how to fix/workaround them, patches are very welcome.

:el:symbol:`url-retrieve`
^^^^^^^^^^^^^^^^^^^^^^^^^

While using EIN, probably most of the error messages are about server
connections.  It looks like the problem is in :el:symbol:`url-retrieve`.
But in those cases you don't loose any notebook data and your IPython
kernel is fine.  You can just type the command again and it will go
fine most of the time.  For saving notebook, I implemented code to
retry when there is an error comes from :el:symbol:`url-retrieve` to
make it even safer.

MuMaMo
^^^^^^

When using MuMaMo based notebook mode, you will notice that
highlighting outside of the cell input is turned off while you are in
the input area.  It seems there is a bug in MuMaMo [#m3bug]_.

If you are using smartrep and MuMaMo together, see also the warning in
:el:symbol:`ein:use-smartrep` document.

.. [#m3bug] See the relevant bug report I posted:
            https://bugs.launchpad.net/nxhtml/+bug/1013794


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


License
=======

Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

