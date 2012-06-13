Welcome to Emacs IPython Notebook's documentation!
==================================================

.. el:package:: ein

Emacs IPython Notebook (EIN) provides fully featured IPython Notebook
client and integrated REPL (like SLIME_).

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

.. [#] You need to setup :el:symbol:`ein:notebook-console-args` properly
.. [#] Use the command :el:symbol:`ein:connect-to-notebook`.


Keybinds
--------

Notebook list
^^^^^^^^^^^^^

You can start notebook by ``M-x ein:notebooklist-open`` and enter the
port or URL of the IPython notebook server.

.. el:function:: ein:notebooklist-open
.. el:function:: ein:notebooklist-new-notebook
.. el:function:: ein:notebooklist-open-notebook-global
.. el:function:: ein:notebooklist-new-scratch-notebook

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


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

