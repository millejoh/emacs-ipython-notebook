;;; ein.el --- IPython notebook client in Emacs

;; Copyright (C) 2012-2015 Takafumi Arakaki, John Miller

;; Author:  John Miller <millejoh at millejoh.com>, Takafumi Arakaki <aka.tkf at gmail.com>
;; URL: http://millejoh.github.io/emacs-ipython-notebook/
;; Keywords: applications, tools
;; Version: 0.13.0

;; This file is NOT part of GNU Emacs.

;; ein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ==================================
;;  EIN -- Emacs IPython Notebook
;; ==================================

;;   --- or **E**\ IN **I**\ s not only for **N**\ otebooks.

;; EIN works with IPython 2.x_, 3.x_, and Juptyer_! Note that remote and
;; password protected logins are working with IPython 3.x, but have not been
;; tested with Jupyter.

;; .. note:: The code has been stable enough for my day to day work, but there are
;;           no guarantees for the safety for your notebook data.  Please make sure
;;           that you backup and backup often!

;; .. _2.x: http://ipython.org/ipython-doc/2/index.html
;; .. _3.x: http://ipython.org/ipython-doc/3/index.html
;; .. _Jupyter: http://jupyter.org

;; Features
;; ========

;; The Emacs IPython Notebook (EIN) provides a client for the IPython v2.x and
;; 3.x notebooks and an integrated REPL (like SLIME_) in Emacs.  EIN makes
;; notebook editing very powerful by allowing you to use any Emacs features, it
;; also expose IPython features such as code evaluation, object inspection and
;; code completion to the Emacs side.  These features can be accessed anywhere
;; in Emacs and improve Python code editing and reading in Emacs.

;; .. _SLIME: http://common-lisp.net/project/slime/

;; Highlighted features:

;; * Copy/paste cells, even to/from different notebooks.
;; * Console integration: You can easily connect to a kernel via the console
;;   application.  This enables you to start debugging in the same kernel.  It is
;;   even possible to connect to a console over ssh.
;; * An IPython kernel can be "connected" to any buffer.  This enables you to
;;   evaluate a buffer or buffer region using the same kernel as the notebook.
;;   Notebook goodies such as tooltip help, help browser and code completion are
;;   available in these buffers.
;; * Jump to definition (go to the definition by hitting ``M-.`` over an object).

;; Other notebook features:

;; * Inline images
;; * Auto/manual-completion
;; * Popup (tooltip) help
;; * Syntax highlighting in each cell types (Python/Markdown)
;; * Help browser (opens when executing ``function?``)
;; * Traceback viewer


;;; Code:

;; For backward compatibility + providing easy way to load EIN for
;; users who prefer manual installation.
;(require 'ein-loaddefs)

(provide 'ein)

;;; Old commentary:

;; Development
;; ===========

;; Event vs hook vs callback
;; -------------------------
;;
;; * Use events (`ein:events') for calling (possibly multiple) functions
;;   for its side effect.
;; * Use hooks for global/configurable setting.
;; * Use callback when caller needs returned value.
;;   (e.g., `:get-buffers' slot in `ein:kernelinfo')

;; Naming
;; ------
;;
;; Variable named `ein:%VAR-NAME%' is a permanent buffer local
;; variable defined by `ein:deflocal'.  It is often an instance of a
;; class/struct named `ein:VAR-NAME'.
;;
;; Old naming rule:
;; * `ein:@VAR-NAME'/`ein:VAR-NAME' is a permanent buffer local
;;   variable.  These variables are obsolete now.
;; * `ein:$STRUCT-NAME' is a name of struct.
;;   These strcuts will be renamed to `ein:CLASS-NAME' when
;;   reimplementing them using EIEIO class instead of CL struct.
;;
;; See also:
;; `CLiki : naming conventions <http://www.cliki.net/naming%20conventions>`_

;; Integrate ein into core emacs functionality
(when (boundp 'mouse-buffer-menu-mode-groups)
  (add-to-list 'mouse-buffer-menu-mode-groups
               '("^ein:" . "ein")))

;;; ein.el ends here
