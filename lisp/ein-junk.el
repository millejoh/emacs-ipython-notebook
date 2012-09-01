;;; ein-junk.el --- Open a notebook to do random things

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-junk.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-junk.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-junk.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-notebooklist)


(defcustom ein:scratch-notebook-name-template "_scratch_%Y-%m-%d-%H%M%S_"
  "Template of scratch notebook name.
This value is used from `ein:notebooklist-new-scratch-notebook'
and `ein:notebook-rename-to-scratch-command'.  This must be a
format string which can be passed to `format-time-string'."
  :type '(string :tag "Format string")
  :group 'ein)

(defun ein:scratch-notebook-name ()
  "Generate new scratch notebook name based on `current-time' and
`ein:scratch-notebook-name-template'."
  (format-time-string ein:scratch-notebook-name-template (current-time)))

(defun ein:notebooklist-new-scratch-notebook ()
  "Open a notebook to try random thing.
Notebook name is determined based on
`ein:scratch-notebook-name-template'."
  (interactive)
  (ein:notebooklist-new-notebook-with-name
   (ein:scratch-notebook-name)
   (ein:default-url-or-port)))

(defun ein:notebook-rename-to-scratch-command (name)
  "Rename notebook based on `ein:scratch-notebook-name-template'
and save it immediately."
  (interactive
   (list (read-string "Rename notebook: "
                      (ein:scratch-notebook-name))))
  (ein:notebook-rename-command name))

(provide 'ein-junk)

;;; ein-junk.el ends here
