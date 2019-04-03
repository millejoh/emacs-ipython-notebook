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

(defcustom ein:completion-backend 'ein:use-none-backend
  "EIN defaults to your individual company-mode or auto-complete-mode configuration.  Change this setting to gather completions from the jupyter server::

 * ein:use-none-backend: local completions only (configured outside EIN)
 * ein:use-company-backend: company-style remote completions (elpy takes precedence)
 * ein:use-ac-backend: deprecated auto-complete remote completions
"
  :type '(choice
          (const ein:use-none-backend)
          (const ein:use-company-backend)
          (const ein:use-ac-backend))
  :group 'ein)

(provide 'ein-subpackages)

;;; ein-subpackages.el ends here
