;;; ein-testing-cell.el --- Testing utilities for cell module  -*- lexical-binding:t -*-

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-testing-cell.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ein-testing-cell.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-testing-cell.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'json)

(defvar ein:testing-example-svg "\
<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
 \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">

<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">
  <circle cx=\"100\" cy=\"50\" r=\"40\" />
</svg>")

(defun ein:testing-codecell-pyout-data (text &optional prompt-number)
  "Create a plist representing JSON data for code-cell output.
TEXT is a string and PROMPT-NUMBER is an integer."
  (list :output_type "pyout"
        :prompt_number (or prompt-number 0)
        :text text))

(defun ein:testing-codecell-data (&optional input prompt-number outputs)
  "Create a plist representing JSON data for code-type cell.
To make OUTPUTS data, use `ein:testing-codecell-pyout-data'."
  (list :cell_type "code"
        :source (or input "")
        :language "python"
        :outputs outputs
	:metadata (list :collapsed json-false :autoscroll json-false)
        :execution_count prompt-number))

(defun ein:testing-textcell-data (&optional source cell-type)
  (list :cell_type cell-type
        :source (or source "")))

(defun ein:testing-markdowncell-data (&optional source)
  (ein:testing-textcell-data source "markdown"))

(defun ein:testing-rawcell-data (&optional source)
  (ein:testing-textcell-data source "raw"))

(defun ein:testing-htmlcell-data (&optional source)
  (ein:testing-textcell-data source "html"))

(provide 'ein-testing-cell)

;;; ein-testing-cell.el ends here
