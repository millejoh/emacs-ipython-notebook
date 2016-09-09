===================
 Request.el manual
===================

.. note:: Entire manual is generated from docstrings.  To
   quickly check what function/variable does, use :kbd:`<f1> f`
   or :kbd:`<f1> v`, (or :kbd:`C-h` instead of :kbd:`<f1>` if you
   don't rebind it).

API
===

.. el:package:: request

.. el:function:: request
.. el:function:: request-abort

Response object
---------------

.. el:function:: request-response-status-code
.. el:function:: request-response-history
.. el:function:: request-response-data
.. el:function:: request-response-error-thrown
.. el:function:: request-response-symbol-status
.. el:function:: request-response-url
.. el:function:: request-response-done-p
.. el:function:: request-response-settings

.. el:function:: request-response-header


Cookie
------

.. el:function:: request-cookie-string
.. el:function:: request-cookie-alist


Deferred
--------

deferred.el_ is a concise way to write callback chain.
You can use :el:symbol:`request-deferred` to do requests
with deferred.el_.

.. _deferred.el: https://github.com/kiwanami/emacs-deferred

.. el:function:: request-deferred


Configuration
=============

Configuration variables are for users.
Libraries using request.el must not modify these variables.

.. el:variable:: request-storage-directory
.. el:variable:: request-curl
.. el:variable:: request-backend
.. el:variable:: request-timeout
.. el:variable:: request-log-level
.. el:variable:: request-message-level
