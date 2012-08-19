;;; ein-worksheet.el --- Worksheet module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-worksheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-worksheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-worksheet.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)

(require 'ein)
(require 'ein-utils)
(require 'ein-cell)
(require 'ein-notification)

(eval-when-compile (defvar ein:notebook-enable-undo))
(declare-function ein:$notebook-url-or-port "ein-notebook")
(declare-function ein:notebook-mode "ein-notebook")


;;; Class and variable

(defvar ein:worksheet-buffer-name-template "*ein: %s/%s*")

(defclass ein:worksheet ()
  (;; Recursive reference to notebook... but needs notebook name here.
   (notebook :initarg :notebook :type ein:$notebook)
   (data :initarg :data)
   (ewoc :initarg :ewoc :type ewoc)
   (kernel :initarg :kernel :type ein:$kernel)
   (dirty :initarg :dirty :type boolean)
   (metadata :initarg :metadata :initform nil)
   (events :initarg :events)
   (notification :initarg :notification)))

(ein:deflocal ein:%worksheet% nil
  "Buffer local variable to store an instance of `ein:worksheet'.")


;;; Initialization of object and buffer

(defun ein:worksheet-new (notebook &rest args)
  (apply #'make-instance 'ein:worksheet :notebook notebook args))

(defmethod ein:worksheet-bind-events ((ws ein:worksheet))
  ;; Bind events for sub components:
  (ein:notification-bind-events (oref ws :notification)
                                (oref ws :events))
  (mapc (lambda (cell) (oset cell :events (oref ws :events)))
        (ein:worksheet-get-cells ws)))

(defmethod ein:worksheet-notebook-name ((ws ein:worksheet))
  (ein:notebook-name (oref ws :notebook)))

(defmethod ein:worksheet-url-or-port ((ws ein:worksheet))
  (ein:$notebook-url-or-port (oref ws :notebook)))

(defmethod ein:worksheet-name ((ws ein:worksheet))
  (plist-get (oref ws :metadata) :name))

(defmethod ein:worksheet-full-name ((ws ein:worksheet))
  (let ((nb-name (ein:worksheet-notebook-name ws)))
    (ein:aif (ein:worksheet-name ws)
        (concat nb-name "/" it)
      nb-name)))

(defmethod ein:worksheet-buffer ((ws ein:worksheet))
  (ein:and-let* (((slot-boundp ws :ewoc))
                 (ewoc (oref ws :ewoc))
                 (buffer (ewoc-buffer ewoc))
                 ((buffer-live-p buffer)))
    buffer))

(defmethod ein:worksheet--get-buffer ((ws ein:worksheet))
  (or (ein:worksheet-buffer ws)
      (generate-new-buffer
       (format ein:worksheet-buffer-name-template
               (ein:worksheet-url-or-port ws)
               (ein:worksheet-full-name ws)))))

(defmethod ein:worksheet-render ((ws ein:worksheet))
  (with-current-buffer (ein:worksheet--get-buffer ws)
    (setq ein:%worksheet% ws)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ewoc (ein:ewoc-create 'ein:worksheet-pp
                                   (ein:propertize-read-only "\n")
                                   nil t)))
        (mapc (lambda (cell-data)
                (ein:cell-enter-last
                 (ein:cell-from-json cell-data :ewoc ewoc)))
              (plist-get (oref ws :data) :cells))))
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)  ; clear undo history
    (when (eq ein:notebook-enable-undo 'no)
      (setq buffer-undo-list t))
    (ein:notebook-mode)
    (oset ws :notification (ein:notification-setup (current-buffer)))
    (ein:worksheet-bind-events ws)
    (ein:log 'info "Worksheet %s is ready" (ein:worksheet-full-name ws))))

(defun ein:worksheet-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))


;;; Persistance and loading

(defmethod ein:worksheet-from-json ((ws ein:worksheet) data)
  (oset ws :data data)
  ws)


;;; Cell indexing, retrieval, etc.

(defmethod ein:worksheet-get-cells ((ws ein:worksheet))
  (let* ((ewoc (oref ws :ewoc))
         (nodes (ewoc-collect ewoc (lambda (n) (ein:cell-node-p n 'prompt)))))
    (mapcar #'ein:$node-data nodes)))


;;; Insertion and deletion of cells


;;; Cell selection.


;;; Cell movement


;;; Cell collapsing and output clearing


;;; Kernel related things

(defmethod ein:worksheet-set-kernel ((ws ein:worksheet) kernel)
  (oset ws :kernel kernel)
  (mapc (lambda (cell) (oset cell :kernel (oref ws :kernel)))
        (ein:worksheet-get-cells ws)))


;;; Generic getter


;;; Buffer


;;; Imenu

(provide 'ein-worksheet)

;;; ein-worksheet.el ends here
