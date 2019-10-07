;;; ein-node.el --- Structure to hold data in ewoc node

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-node.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-node.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-node.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ewoc)

(require 'ein-core)


(cl-defstruct ein:$node
  path                                  ; list of path
  data                                  ; actual data
  class                                 ; list
  )

(defun ein:node-new (path data &optional class &rest args)
  (apply #'make-ein:$node :path path :data data :class class args))

(defun ein:node-add-class (node &rest classes)
  (mapc (lambda (c) (add-to-list (ein:$node-class node) c)) classes))

(defun ein:node-remove-class (node &rest classes)
  (let ((node-class (ein:$node-class node)))
    (mapc (lambda (c) (setq node-class (delq c node-class))) classes)
    (setf (ein:$node-class node) node-class)))

(defun ein:node-has-class (node class)
  (memq class (ein:$node-class node)))

(defun ein:node-filter (ewoc-node-list &rest args)
  (cl-loop for (key . class) in (ein:plist-iter args)
    do (setq ewoc-node-list
             (cl-loop for ewoc-node in ewoc-node-list
               for node = (ewoc-data ewoc-node)
               when (cl-case key
                      (:is (ein:node-has-class node class))
                      (:not (not (ein:node-has-class node class)))
                      (t (error "%s is not supported" key)))
               collect ewoc-node)))
  ewoc-node-list)

(provide 'ein-node)

;;; ein-node.el ends here
