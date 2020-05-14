;;; ein-scratchsheet.el --- Worksheet without needs for saving     -*- lexical-binding:t -*-

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-scratchsheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-scratchsheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-scratchsheet.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-worksheet)

(defclass ein:scratchsheet (ein:worksheet)
  ((data :initarg :data :initform nil))
  :documentation "Worksheet without needs for saving.")

(defun ein:scratchsheet-new (nbformat notebook-path kernel events &rest args)
  (apply #'make-instance 'ein:scratchsheet
         :nbformat nbformat
	 :notebook-path notebook-path
         :kernel kernel
	 :events events
         args))

(cl-defmethod ein:worksheet--buffer-name ((ws ein:scratchsheet))
  (format "*ein:scratch %s/%s*"
	  (ein:worksheet-url-or-port ws)
	  (ein:worksheet-notebook-path ws)))

(provide 'ein-scratchsheet)

;;; ein-scratchsheet.el ends here
