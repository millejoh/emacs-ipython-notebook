;;; ein-scratchsheet.el --- Worksheet without needs for saving

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

(defvar ein:scratchsheet-buffer-name-template "*ein:scratch %s/%s*")

(defclass ein:scratchsheet (ein:worksheet)
  ;; Note that `data' slot is accessed when rendering worksheet.
  ;; So, set valid empty data (`nil') here.
  ((data :initarg :data :initform nil))
  :documentation
  "Worksheet without needs for saving.")

(defun ein:scratchsheet-new (nbformat get-notebook-name discard-output-p
                                      kernel events &rest args)
  (apply #'make-instance 'ein:scratchsheet
         :nbformat nbformat :get-notebook-name get-notebook-name
         :discard-output-p discard-output-p :kernel kernel :events events
         args))

(cl-defmethod ein:worksheet--buffer-name ((ws ein:scratchsheet))
  (format ein:scratchsheet-buffer-name-template
          (ein:worksheet-url-or-port ws)
          (ein:worksheet-full-name ws)))

(provide 'ein-scratchsheet)

;;; ein-scratchsheet.el ends here
