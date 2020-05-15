;;; ein-testing-notebook.el --- Testing utilities for notebook module  -*- lexical-binding:t -*-

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-testing-notebook.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ein-testing-notebook.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-testing-notebook.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-notebook)
(require 'ein-testing-cell)

(defvar ein:testing-notebook-dummy-name "Dummy Name.ipynb")
(defvar ein:testing-notebook-dummy-url "DUMMY-URL")

(defun ein:testing-notebook-from-json (json-string)
  (let* ((data (ein:json-read-from-string json-string))
         (path (plist-get data :path))
         (kernelspec (make-ein:$kernelspec :name "python" :language "python"))
         (content (make-ein:$content :url-or-port ein:testing-notebook-dummy-url
                                     :notebook-version "5.7.0"
                                     :path path)))
    (ignore content)
    (cl-letf (((symbol-function 'ein:need-notebook-version) (lambda (&rest _ignore) "5.7.0"))
              ((symbol-function 'ein:kernel-retrieve-session) #'ignore)
              ((symbol-function 'ein:notebook-enable-autosaves) #'ignore))
      (let ((notebook (ein:notebook-new ein:testing-notebook-dummy-url path kernelspec)))
        (setf (ein:$notebook-kernel notebook)
              (ein:kernel-new 8888 "" nil  "/kernels" (ein:$notebook-events notebook) (ein:need-notebook-version (ein:$notebook-url-or-port notebook))))
        (setf (ein:$kernel-events (ein:$notebook-kernel notebook))
              (ein:events-new))
        ; matryoshka: new-content makes a ein:$content using CONTENT as template
        ; populating its raw_content field with DATA's content field
        (ein:notebook-open--callback
         notebook nil
         (ein:new-content (ein:$notebook-url-or-port notebook)
                          (ein:$notebook-notebook-path notebook) data))
        (ein:notebook-buffer notebook)))))

(defun ein:testing-notebook-make-data (name path cells)
  (setq cells
        (ein:testing-notebook--preprocess-cells-data-for-json-encode cells))
  `((path . ,path)
    (name . ,name)
    (type . "notebook")
    (format . "json")
    (mimetype . nil)
    (writeable . t)
    (content (metadata . ())
             (nbformat . 4)
             (nbformat_minor . 0)
             (cells . ,(apply #'vector cells)))))

(defun ein:testing-notebook--preprocess-cells-data-for-json-encode (cells)
  "Preprocess CELLS data to make it work nice with `json-encode'."
  (mapcar (lambda (c)
            (cond
             ((equal (plist-get c :cell_type) "code")
              ;; turn `:outputs' into an array.
              (plist-put c :outputs (apply #'vector (plist-get c :outputs))))
             (t c)))
          cells))

(defun ein:testing-notebook-make-new (&optional name path cells)
  "Make new notebook.  One empty cell will be inserted
automatically if CELLS is nil."
  (ein:testing-notebook-from-json
   (json-encode (ein:testing-notebook-make-data
                 (or name ein:testing-notebook-dummy-name)
                 (or path name ein:testing-notebook-dummy-name)
                 cells))))

(defun ein:testing-notebook-make-empty (&optional name path)
  "Make empty notebook and return its buffer.
Automatically inserted cell for new notebook is deleted."
  (let ((buffer (ein:testing-notebook-make-new name path)))
    (with-current-buffer buffer
      (call-interactively #'ein:worksheet-delete-cell))
    buffer))

(defmacro ein:testing-with-one-cell (type-or-cell &rest body)
  "Insert new cell of TYPE-OR-CELL in a clean notebook and execute BODY.
The new cell is bound to a variable `cell'."
  (declare (indent 1))
  `(with-current-buffer (ein:testing-notebook-make-empty)
     (let ((cell (ein:worksheet-insert-cell-below ein:%worksheet%
                                                  ,type-or-cell nil t)))
       ,@body)))

(defun ein:testing-make-notebook-with-outputs (list-outputs)
  "Make a new notebook with cells with output.
LIST-OUTPUTS is a list of list of strings (pyout text).  Number
of LIST-OUTPUTS equals to the number cells to be contained in the
notebook."
  (ein:testing-notebook-make-new
   ein:testing-notebook-dummy-name nil
   (mapcar (lambda (outputs)
             (ein:testing-codecell-data
              nil nil (mapcar #'ein:testing-codecell-pyout-data outputs)))
           list-outputs)))

(provide 'ein-testing-notebook)

;;; ein-testing-notebook.el ends here
