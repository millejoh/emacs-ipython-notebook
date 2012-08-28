Welcome to Emacs IPython Notebook's documentation!
==================================================

.. el:package:: ein

Emacs IPython Notebook (EIN) provides a IPython Notebook client and
integrated REPL (like SLIME_) in Emacs.  While EIN makes notebook
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

* `Wiki
  <https://github.com/tkf/emacs-ipython-notebook/wiki>`_

  + `Screenshots
    <https://github.com/tkf/emacs-ipython-notebook/wiki/Screenshots>`_
  + `Tips
    <https://github.com/tkf/emacs-ipython-notebook/wiki/Tips>`_

* `Downloads
  <https://github.com/tkf/emacs-ipython-notebook/tags>`_
* `Repository at GitHub
  <https://github.com/tkf/emacs-ipython-notebook>`_
* `Issue Tracker at GitHub
  <https://github.com/tkf/emacs-ipython-notebook/issues>`_

.. [#] You need to setup :el:symbol:`ein:console-args` properly
.. [#] Use the command :el:symbol:`ein:connect-to-notebook-command`.

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
* `websocket.el`_ 0.9
* (optional) mumamo_:
  It will be automatically loaded when it is on the path.
  The official way to setup path is to load nXhtml_.
* (optional) markdown-mode
* (optional) python-mode:
  It should work with either python.el or `python-mode.el`_ [#]_.
  Fabian Gallina's `python.el`_ is required to use
  :el:symbol:`ein:console-open` command.
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
.. _python-mode.el: :https://launchpad.net/python-mode
.. _auto-complete.el: http://cx4a.org/software/auto-complete/
.. _smartrep.el: https://github.com/myuhe/smartrep.el

.. [#] See
   :ref:`Gotchas and caveats > python-mode.el <gotchas-python-mode.el>`.


Install
-------

.. warning:: As EIN relies on many packages and it will not work
   properly with outdated versions, installing it using el-get or
   MELPA is highly recommended.


Using el-get
^^^^^^^^^^^^

If you use developmental version of `el-get`_, you can install it
easily using it.  Emacs IPython Notebook is registered as package
``ein``.   See `el-get`_ website for more information.

.. _el-get: https://github.com/dimitri/el-get

.. note:: If el-get complains there is no "ein/ein-notebooklist", I
   guess you are updated EIN after I reorganized the repository.  In
   this case, simply reinstalling EIN by ``M-x el-get-reinstall RET ein``
   will solve the problem.  Note that this will remove the whole
   ``ein/`` directory including ``.git`` directory before installation.


Using package.el (MELPA)
^^^^^^^^^^^^^^^^^^^^^^^^

You can install EIN using `package.el`_ when MELPA_ package repository
is added to its setting. See MELPA_ website for more information.

.. _`package.el`: http://emacswiki.org/emacs/ELPA
.. _MELPA: https://github.com/milkypostman/melpa


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

.. el:function:: ein:worksheet-delete-cell
.. el:function:: ein:notebook-rename-to-scratch-command
.. el:function:: ein:notebook-kill-all-buffers
.. el:function:: ein:iexec-mode

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

.. el:function:: ein:connect-to-notebook-command
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
.. el:function:: ein:pytools-pandas-to-ses

Misc
^^^^

.. el:package:: helm
.. el:function:: helm-ein-notebook-buffers
.. el:package:: anything
.. el:function:: anything-ein-notebook-buffers
.. el:package:: ein

.. It is better to remove el:package from eldomain??


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
.. el:function:: ein:notebooklist-load

Notebook
^^^^^^^^

.. el:variable:: ein:notebook-enable-undo
.. el:variable:: ein:notebook-discard-output-on-save
.. el:variable:: ein:notebook-modes
.. el:variable:: ein:notebook-kill-buffer-ask
.. el:variable:: ein:notebook-querty-timeout-open
.. el:variable:: ein:notebook-querty-timeout-save
.. el:variable:: ein:cell-traceback-level
.. el:variable:: ein:cell-autoexec-prompt
.. el:variable:: ein:scratch-notebook-name-template
.. el:variable:: ein:iexec-delay
.. el:variable:: ein:complete-on-dot

Console
^^^^^^^

.. el:variable:: ein:console-security-dir
.. el:variable:: ein:console-executable
.. el:variable:: ein:console-args

Connect
^^^^^^^

.. el:variable:: ein:connect-run-command
.. el:variable:: ein:connect-reload-command
.. el:variable:: ein:connect-save-before-run
.. el:variable:: ein:propagate-connect
.. el:variable:: ein:connect-aotoexec-lighter
.. el:variable:: ein:connect-default-notebook
.. el:function:: ein:connect-to-default-notebook

MuMaMo
^^^^^^

.. el:variable:: ein:mumamo-codecell-mode
.. el:variable:: ein:mumamo-textcell-mode
.. el:variable:: ein:mumamo-htmlcell-mode
.. el:variable:: ein:mumamo-markdowncell-mode
.. el:variable:: ein:mumamo-rawcell-mode
.. el:variable:: ein:mumamo-headingcell-mode
.. el:variable:: ein:mumamo-fallback-mode
.. el:variable:: ein:use-mumamo-indent-line-function-workaround

Misc
^^^^

.. el:variable:: ein:filename-translations
.. el:function:: ein:tramp-create-filename-translator
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


.. _gotchas-python-mode.el:

python-mode.el
^^^^^^^^^^^^^^

In my environment, using `python-mode.el`_ without byte-compiling it
in MuMaMo based notebook mode produces segfault.

Also, ``mumamo-idle-set-major-mode`` messages error
``(wrong-type-argument listp python-saved-check-command)``
time to time, making minibuffer bit noisy while editing notebook.


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


Reporting issue
---------------

Logging
^^^^^^^

Sometime more information that the ``*Message*`` buffer provides is
needed to debug.

1. Execute ``(ein:log-set-level 'debug)``
   (e.g., ``M-: (ein:log-set-level 'debug) RET``).
2. Then do some operation which cause the problem.
3. Go to the log buffer ``_*ein:log-all*`` (it starts with a space)
   and paste the whole buffer to the issue tracker.

   Please enclose the log with three backquotes to make the snippet as
   a code block, like this::

     ```
     [verbose] Start logging. @#<buffer *ein: 8888/NAME*>
     [info] Notebook NAME is ready @#<buffer *ein: 8888/NAME*>
     [info] Kernel started: 5e4f74d1-ce91-4e7e-9575-9646adea5172 @#<buffer *scratch*>
     ```

   See also: `GitHub Flavored Markdown - Introduction
   <http://github.github.com/github-flavored-markdown/>`_

   If it is too long, you can use paste bin service such as
   `gist <https://gist.github.com/>`_.

websocket.el
""""""""""""

websocket.el has its logging buffer.  Sometime its useful to see its
log.  This is how to do that.

1. ``(require 'ein-dev)``
2. ``(setq websocket-debug t)`` or call :el:symbol:`ein:dev-start-debug`.
3. Then do the operation which causes the problem.
4. Go to log buffer using
   :el:symbol:`ein:dev-pop-to-debug-shell` and
   :el:symbol:`ein:dev-pop-to-debug-iopub`.
   These command must be called in the notebook buffer.

Debugging
^^^^^^^^^

If you are interested in debugging EIN, you should start it with
calling the command :el:symbol:`ein:dev-start-debug`.
This command sets :el:symbol:`debug-on-error` to ``t`` and do some
patching to debugger.  This patching is required because printing EWOC
objects freezes Emacs otherwise.  It also changes log level to
log everything the log buffer.  You can reset the patch and log level
with :el:symbol:`ein:dev-stop-debug`.


Change Log
==========

v0.2
----

* Add "scratch sheet".  This acts almost as same as worksheet, but you
  don't need to save it.  You can use try any code without saving
  junks in your notebook.  Use the command
  :el:symbol:`ein:notebook-scratchsheet-open` to open scratch sheet.
* Menu support in notebook mode.
* Auto-connection support.
  The new function :el:symbol:`ein:connect-to-default-notebook` can be
  added to :el:symbol:`python-mode-hook` to automatically connect
  python-mode buffers to default notebook specified by
  :el:symbol:`ein:connect-default-notebook`.  See also
  :el:symbol:`ein:notebooklist-load`.
* Add :el:symbol:`ein:worksheet-execute-cell-and-insert-below`.
* Change the timing to trigger auto-execution in connected buffer.
  It was triggered on save before.  Now it is on run, eval or reload.
  See :el:symbol:`ein:connect-toggle-autoexec`.
* [WIP] Worksheet support.


v0.1.2
------

* Mostly refactoring for worksheet support in v0.2.
* Rename command :el:symbol:`ein:notebook-console-open` to
  :el:symbol:`ein:console-open`.  It is available from non-notebook
  buffer such as connected buffer now.
* Add :el:symbol:`ein:connect-reload-buffer`.
  Old default :el:symbol:`ein:connect-run-buffer` behavior is
  replaced by this function.  :el:symbol:`ein:connect-run-buffer`
  now actually runs buffer instead of loading it.


v0.1.1
------

* Support `auto-complete.el`_\ 's popup/quick help.
* Add :el:symbol:`ein:notebooklist-first-open-hook`.
* Handle carriage return
  (`#13 <https://github.com/tkf/emacs-ipython-notebook/issues/13>`_).
* :el:symbol:`ein:connect-to-notebook-command` is improved;
  it can connect to the notebook which is not opened yet.
* Plain text type output is favored over LaTeX type output
  (previous setting was opposite).
* Workaround indentation problem when using MuMaMo
  (`#20 <https://github.com/tkf/emacs-ipython-notebook/issues/20>`_).
  See :el:symbol:`ein:use-mumamo-indent-line-function-workaround`.
* Add :el:symbol:`ein:notebook-rename-to-scratch-command`.
* Add :el:symbol:`ein:pytools-pandas-to-ses`.
* Add Imenu support.
* Better heading cell faces.
* Add :el:symbol:`ein:iexec-mode`
* Add auto-execution mode
  (see :el:symbol:`ein:connect-toggle-autoexec` and
  :el:symbol:`ein:notebook-turn-on-autoexec`).
* Start completion when "." is inserted.
  Use :el:symbol:`ein:complete-on-dot` to disable this feature.
* Support tramp.  See :el:symbol:`ein:filename-translations`.
* Change callback API in :el:symbol:`ein:kernel-execute`
  to adapt messaging protocol change in
  `IPython (#2051) <https://github.com/ipython/ipython/pull/2051>`_.
* Add helm/anything support.
  Use :el:symbol:`helm-ein-notebook-buffers` or
  :el:symbol:`anything-ein-notebook-buffers`.


v0.1
----

* First release.


License
=======

Emacs IPython Notebook is licensed under GPL v3.
See COPYING for details.


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

