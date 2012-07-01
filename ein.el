;;; ein.el --- IPython notebook client in Emacs

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki
;; URL: http://tkf.github.com/emacs-ipython-notebook/
;; Keywords: applications, tools

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

(autoload 'ein:notebooklist-open "ein-notebooklist"
  "Open notebook list buffer." t)

(autoload 'ein:connect-to-notebook "ein-connect"
  "Connect any buffer to notebook and its kernel." t)

(provide 'ein)

;;; ein.el ends here
