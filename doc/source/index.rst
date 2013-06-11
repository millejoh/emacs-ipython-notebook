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

   git clone git://github.com/tkf/emacs-ipython-notebook.git
   cd emacs-ipython-notebook/
   lisp/zeroein.el

This will launch a new Emacs instance.

You can use environment variable ``EMACS`` to control Emacs executable
to use.::

   EMACS=emacs-snapshot lisp/zeroein.el

The above command requires /bin/sh.  If the above command does not work
(e.g., you are using MS Windows), try the following command::

  emacs -Q -l lisp/zeroein.el


Requirements
------------

* IPython_ 0.12 or higher.
* `websocket.el`_ 0.9
* `request.el`_ >= 0.2
* (optional) mumamo_ developmental version:
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
EIEIO and json.el.  EIN is currently tested against Emacs 23.3 and 24.3.
It is known to work in Emacs 23.2, 24.1 and 24.2.

.. _IPython: http://ipython.org/
.. _websocket.el: https://github.com/ahyatt/emacs-websocket
.. _request.el: https://github.com/tkf/emacs-request
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

.. note:: If you get error "Cannot open load file: request" that means
   you have older el-get version.  You can fix this problem by
   either (1) install request.el manually, (2) use the latest recipe,
   or (3) update el-get to its master.

   You can get the latest recipe here:

   - https://github.com/dimitri/el-get/blob/master/recipes/ein.rcp
   - https://github.com/dimitri/el-get/blob/master/recipes/request.rcp

   See `issue 98 <https://github.com/tkf/emacs-ipython-notebook/issues/98>`_
   for more information.


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
.. el:function:: ein:notebooklist-login
.. el:function:: ein:junk-new

.. el:keymap:: ein:notebooklist-mode-map
   :exclude: widget-button

.. _notebook-commands:

Notebook
^^^^^^^^

The following keybinds are available in notebook buffers.

.. el:keymap:: ein:notebook-mode-map
   :replace: s/C-c TAB/C-c C-i/
             s/C-c RET/C-c C-m/

.. el:function:: ein:worksheet-execute-all-cell
.. el:function:: ein:worksheet-delete-cell
.. el:function:: ein:junk-rename
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
   :replace: s/C-c TAB/C-c C-i/

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
.. el:function:: helm-ein-kernel-history
.. el:function:: helm-ein-notebook-buffers
.. el:package:: anything
.. el:function:: anything-ein-kernel-history
.. el:function:: anything-ein-notebook-buffers
.. el:package:: ein

.. It is better to remove el:package from eldomain??


Org-mode integration
--------------------

You can link to IPython notebook from org-mode_ files.

1. Call org-mode function :el:symbol:`org-store-link`
   [#org-store-link]_ in notebook buffer.  You can select a region to
   specify a position in the notebook.

2. Go to org-mode file and type ``C-c C-l``
   (:el:symbol:`org-insert-link`).  This will insert a link to the
   notebook.

3. Type ``C-c C-o`` (:el:symbol:`org-open-at-point`) to open
   the link at the point of cursor.

.. _org-mode: http://orgmode.org/

.. [#org-store-link] See `1.3 Activation
   <http://orgmode.org/manual/Activation.html>`_ in org-mode manual.


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

.. el:variable:: ein:worksheet-enable-undo
.. el:variable:: ein:notebook-modes
.. el:variable:: ein:notebook-kill-buffer-ask
.. el:variable:: ein:notebook-querty-timeout-open
.. el:variable:: ein:notebook-querty-timeout-save
.. el:variable:: ein:cell-traceback-level
.. el:variable:: ein:cell-autoexec-prompt
.. el:variable:: ein:junk-notebook-name-template
.. el:variable:: ein:iexec-delay
.. el:variable:: ein:complete-on-dot
.. el:variable:: ein:helm-kernel-history-search-key
.. el:variable:: ein:anything-kernel-history-search-key
.. el:variable:: ein:helm-kernel-history-search-auto-pattern
.. el:variable:: ein:output-type-preference
.. el:variable:: ein:shr-env

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

Jedi.el
"""""""

.. el:function:: ein:jedi-setup

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
See Tips_ to fix this problem.


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
  https://github.com/tkf/emacs-ipython-notebook/blob/master/tools/emacslisp.py


Reporting issue
---------------

Please use ``M-x ein:dev-bug-report-template`` to write a bug report.
It pops up a buffer containing some system information and instruction
for bug report.


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
If the bug is websocket related, you may need to run it with a
prefix key like this: ``C-u M-x ein:dev-start-debug RET`` to get
a backtrace.
This command sets :el:symbol:`debug-on-error` to ``t`` and do some
patching to debugger.  This patching is required because printing EWOC
objects freezes Emacs otherwise.  It also changes log level to
log everything the log buffer.  You can reset the patch and log level
with :el:symbol:`ein:dev-stop-debug`.


Change Log
==========

v0.2.1
------

* Cached auto-complete is removed.
  :el:symbol:`ac-source-ein-cached` and :el:symbol:`ac-complete-ein-cached`
  are obsolete.
  :el:symbol:`ein:ac-max-cache` has no effect now.
* :el:symbol:`ein:query-timeout` is `nil` if `curl` backend is used
  by request.el_.
* History search interface (:el:symbol:`helm-ein-kernel-history` and
  :el:symbol:`anything-ein-kernel-history`) discards duplications.
  This functionality requires at least version 4.0 of IPython kernel.
  It is introduced by the pull request
  `ipython/ipython#2792 <https://github.com/ipython/ipython/pull/2792>`_.
  As of writing, you need IPython 0.14.dev from github.
  For older versions of kernels, it continues to work but you will
  see duplications.
* Add support for `kernel_info` request for IPython kernel protocol,
  which is introduced by
  `ipython/ipython#2649 <https://github.com/ipython/ipython/issues/2649>`_.
  This protocol is not used in EIN anywhere yet.
* Use request.el_ for smoother experience.

v0.2
----

* Preliminary login support.  See :el:symbol:`ein:notebooklist-login`.
* Code completion in notebook happens *really* automatically.
  You don't need to hit a key to start completion.
* :el:symbol:`ein:console-open` works without `python.el`_.
* Expand code cell output on execution.
  (`#88 <https://github.com/tkf/emacs-ipython-notebook/issues/88>`_).
* Improve :el:symbol:`ein:completer-dot-complete` and
  :el:symbol:`ein:jedi-dot-complete`.  Do not expand common part when
  inserting dot, to make typing code containing dots less surprising.
* Add support for Jedi.el_.  See :el:symbol:`ein:jedi-setup`.
* Add a simple org-mode link support.
* Add built-in multiple language fontification for notebook:
  :el:symbol:`ein:notebook-multilang-mode`.
  This is the new default for :el:symbol:`ein:notebook-modes`.
* Add helm/anything interface to search kernel history:
  :el:symbol:`helm-ein-kernel-history` and
  :el:symbol:`anything-ein-kernel-history`.
  See also the configurable options to use these commands:
  :el:symbol:`ein:helm-kernel-history-search-key` and
  :el:symbol:`ein:anything-kernel-history-search-key`.
* Preliminary support for multiple worksheets.
* Rename notion of "scratch notebook" to "junk notebook".
  This is to avoid confusion with newly added "scratch sheet".
  Old commands are renamed to :el:symbol:`ein:junk-new` and
  :el:symbol:`ein:junk-rename`.
* Preferred MIME types to be used can be configured using the variable
  :el:symbol:`ein:output-type-preference`.
* HTML content is rendered SHR (Simple HTML Renderer) by default.
  Use :el:symbol:`ein:shr-env` to tweak how HTML rendered.
* :el:symbol:`ein:notebook-discard-output-on-save` is obsolete now.
* Support execution history.  Commands
  :el:symbol:`ein:worksheet-previous-input-history` and
  :el:symbol:`ein:worksheet-next-input-history` can be used
  to insert previously executed code into the current cell.
* Add :el:symbol:`ein:pseudo-console-mode`.
* Add "scratch sheet".  This acts almost as same as worksheet, but you
  don't need to save it.  You can use try any code without saving
  junks in your notebook.  Use the command
  :el:symbol:`ein:notebook-scratchsheet-open` to open scratch sheet.
* Menu support in notebook mode and notebook list mode.
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

