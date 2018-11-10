;;; ein-subpackages.el --- Subpackage management

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-subpackages.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-subpackages.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-subpackages.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (defvar ein:smartrep-config-once-called))

(declare-function ein:smartrep-config "ein-smartrep")

(defcustom ein:completion-backend 'ein:use-ac-backend
  "Determines which completion backend to use in opened EIN notebooks.

After changing the value of this variable it is recommended that
you restart Emacs. The available completion backends are::

 * ein:use-ac-backend : Use auto-complete with IPython's builtin completion engine.
 * ein:use-ac-jedi-backend : Use auto-complete with the Jedi backend.
 * ein:use-company-backend : Use company-mode with IPython's builtin completion engine.
 * ein:use-company-jedi-backends : Use company-mode with the Jedi backend (currently not implemented).
 * ein:use-none-backend: Avoid autocomplete altogether
"
  :type '(choice
          (const ein:use-ac-backend)
          (const ein:use-ac-jedi-backend)
          (const ein:use-company-backend)
          (const ein:use-company-jedi-backend)
          (const ein:use-none-backend))
  :group 'ein-completion)

(defcustom ein:use-smartrep nil
  "Set to `t' to use preset smartrep configuration.

.. warning:: When used with MuMaMo (see `ein:notebook-modes'),
   keyboard macro which manipulates cell (add, remove, move,
   etc.) may start infinite loop (you need to stop it with
   ``C-g``).  Please be careful using this option if you are a
   heavy keyboard macro user.  Using keyboard macro for other
   commands is fine.

.. (Comment) I guess this infinite loop happens because the three
   modules (kmacro.el, mumamo.el and smartrep.el) touches to
   `unread-command-events' in somehow inconsistent ways."
  :type 'boolean
  :group 'ein)

(defun ein:subpackages-load ()
  "Load sub-packages depending on configurations."
  (cl-case ein:completion-backend
    (ein:use-ac-backend (require 'ein-ac))
    (ein:use-ac-jedi-backend (require 'ein-ac))
    (ein:use-company-backend (require 'ein-company))
    (ein:use-company-jedi-backend (require 'ein-company)))
  (when ein:use-smartrep
    (with-eval-after-load "ein-smartrep"
      (ein:smartrep-config))
    (require 'ein-smartrep)))

(provide 'ein-subpackages)

;;; ein-subpackages.el ends here
