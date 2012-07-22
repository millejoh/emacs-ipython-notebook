;;; ein.el --- IPython notebook client in Emacs

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; URL: http://tkf.github.com/emacs-ipython-notebook/
;; Keywords: applications, tools
;; Version: 0.1.1alpha0

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

;;

;;; Code:

(defvar ein:version "0.1.1alpha0"
  "Version number for Emacs IPython Notebook (EIN).")

(autoload 'ein:notebooklist-open "ein-notebooklist"
  "Open notebook list buffer." t)

(autoload 'ein:notebooklist-list-notebooks "ein-notebooklist"
  "Return a list of notebook path (NBPATH)." t)

(autoload 'ein:notebooklist-open-notebook-global "ein-notebooklist"
  "Choose notebook from all opened notebook list and open it." t)

(autoload 'ein:connect-to-notebook "ein-connect"
  "Connect any buffer to notebook and its kernel." t)

(autoload 'ein:dev-insert-mode-map "ein-dev")
(autoload 'ein:dev-start-debug "ein-dev" "Enable debugging support." t)
(autoload 'ein:dev-stop-debug "ein-dev" "Disable debugging support." t)

(provide 'ein)

;;; ein.el ends here
