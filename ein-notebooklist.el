;;; ein-notebooklist.el --- Notebook list buffer

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-notebooklist.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notebooklist.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebooklist.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'widget)

(require 'ein-utils)
(require 'ein-notebook)

(defvar ein:notebooklist-data nil)
(make-variable-buffer-local 'ein:notebooklist-data)
(put 'ein:notebooklist-data 'permanent-local t)

(defvar ein:notebooklist-buffer-name-template "*ein:notebooklist %s*")


(defun ein:notebooklist-url ()
  (concat (ein:base-project-url) "notebooks"))

(defun ein:notebooklist-open ()
  (interactive)
  (url-retrieve
   (ein:notebooklist-url)
   (lambda (s) (ein:notebooklist-pop-buffer))))

(defun ein:notebooklist-pop-buffer ()
  (let ((data (ein:json-read)))
    (kill-buffer (current-buffer))
    (with-current-buffer
        (get-buffer-create
         (format ein:notebooklist-buffer-name-template ein:port))
      (setq ein:notebooklist-data data)
      (ein:notebooklist-render)
      (pop-to-buffer (current-buffer)))))

(defun ein:notebooklist-render ()
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Create notebook list
  (widget-insert "IPython Notebook list\n\n")
  (loop for note in ein:notebooklist-data
        for name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        do (progn (widget-insert name " ")
                  (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (message "Open notebook %s." name)
                               (ein:notebook-open notebook-id)))
                   "open")
                  (widget-insert "\n")))
  (use-local-map widget-keymap)
  (widget-setup))

(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
