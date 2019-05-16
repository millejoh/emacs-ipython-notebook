;;; ein.el --- IPython notebook client in Emacs

;; Copyright (C) 2012-2015 Takafumi Arakaki, John Miller

;; Author:  John Miller <millejoh at millejoh.com>, Takafumi Arakaki <aka.tkf at gmail.com>
;; URL: http://millejoh.github.io/emacs-ipython-notebook/
;; Keywords: jupyter, literate programming, reproducible research

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

;; Emacs IPython Notebook (EIN) lets you run Jupyter (formerly IPython)
;; notebooks within Emacs.  It channels all the power of Emacs without the
;; idiosyncrasies of in-browser editing.
;;
;; Org_ users please find ob-ein_, a jupyter Babel_ backend.
;;
;; EIN was originally written by `[tkf]`_.  A jupyter Babel_ backend was first
;; introduced by `[gregsexton]`_.
;;

;;; Code:

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
