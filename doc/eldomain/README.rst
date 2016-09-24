======================================================
 Emacs Lisp domain -- Sphinx extension for Emacs Lisp
======================================================

Example usage (Emacs IPython Notebook documentation):
http://tkf.github.com/emacs-ipython-notebook/

Setup
=====

You need to have something like this in ``conf.py``::

   # Dictionary maps package name to package prefix.
   elisp_packages = {
       'YOUR-PACKAGE': 'YOUR-PACKAGE-PREFIX-',
       'ANOTHER-PACKAGE': 'ANOTHER-PACKAGE-PREFIX:',
   }

   # These are optional:
   emacs_executable = 'emacs'
   elisp_pre_load = 'conf.el'


You need to load functions and variables you want to load in ``conf.el``::

   (add-to-list 'load-path "PATH/TO/YOUR/PACKAGE/")
   (require 'YOUR-PACKAGE)


See the setup for Emacs IPython Notebook:
https://github.com/tkf/emacs-ipython-notebook/tree/master/doc/source


Directives and roles
====================

First of all, you need to specify package to use before using any
other directives.::

   .. el:package:: PACKAGE-NAME


Then, you can automatically document function/macro/variables.::

   .. el:function:: FUNCTION-NAME
   .. el:macro:: MACRO-NAME
   .. el:variable:: VARIABLE-NAME

.. TODO: document options for these directives.


You can get well-formatted keybind list using::

   .. el:keymap:: KEYMAP-NAME


Finally, you can use ``el:symbol`` role to refer symbols.
For exmaple::

   :el:symbol:`FUNCTION-NAME`
