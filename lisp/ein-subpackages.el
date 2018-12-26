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
;; This module was more important when packages came from git submodules.
;; It has been gutted since.

;;

;;; Code:

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

(provide 'ein-subpackages)

;;; ein-subpackages.el ends here
