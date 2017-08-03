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

(eval-when-compile (defvar ein:ac-config-once-called)
                   (defvar ein:smartrep-config-once-called))

(declare-function ein:ac-config-once "ein-ac")
(declare-function ein:smartrep-config-once "ein-smartrep")

(defcustom ein:completion-backend 'ein:use-ac-backend
  "Determines which completion backend to use in opened EIN notebooks.

After changing the value of this variable it is recommended that
you restart Emacs. The available completion backends are::

 * ein:use-ac-backend : Use auto-complete with IPython's builtin completion engine.
 * ein:use-ac-jedi-backend : Use auto-complete with the Jedi backend.
 * ein:use-company-backend : Use company-mode with IPython's builtin completion engine.
 * ein:use-company-jedi-backends : Use company-mode with the Jedi backend (currently not implemented).
"
  :type '(radio
          (const :tag "Auto-Complete" 'ein:use-ac-backend)
          (const :tag "Auto-Complete + Jedi" 'ein:use-ac-jedi-backend)
          (const :tag "Company Mode" 'ein:use-company-backend)
          (const :tag "Company Mode + Jedi" 'ein:use-company-jedi-backend))
  :group 'ein-completion)

;; (defcustom ein:use-auto-complete nil
;;   "Set to `t' to use preset auto-complete configuration.
;; Use `ein:use-auto-complete-superpack' when you need more powerful
;; auto completion."
;;   :type 'boolean
;;   :group 'ein-completion)

(defcustom ein:use-auto-complete-superpack nil
  "Set to `t' to use preset a little bit hacky auto-complete configuration.
When this option is enabled, cached omni completion is available."
  :type 'boolean
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

(defcustom ein:load-dev nil
  "Load development helper."
  :type 'boolean
  :group 'ein)

(defun ein:subpackages-load ()
  "Load sub-packages depending on configurations."
  (cl-ecase ein:completion-backend
    ((ein:use-ac-backend ein:use-ac-jedi-backend)
     (require 'ein-ac)
     (ein:ac-config-once ein:use-auto-complete-superpack))
    ((ein:use-company-backend ein:use-company-jedi-backend)
     (warn "EIN: company backend not implemented yet.")))
  (when ein:use-smartrep
    (require 'ein-smartrep)
    (ein:smartrep-config-once))
  (when ein:load-dev
    (require 'ein-dev)))


(defun ein:subpackages-reload ()
  "Reload sub-packages."
  (interactive)
  (setq ein:ac-config-once-called nil)
  (setq ein:smartrep-config-once-called nil)
  (ein:subpackages-load))

(provide 'ein-subpackages)

;;; ein-subpackages.el ends here
