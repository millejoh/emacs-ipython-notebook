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

(defvar ein:notebooklist-data nil
  "Buffer local variable to store data from the server.")
(make-variable-buffer-local 'ein:notebooklist-data)
(put 'ein:notebooklist-data 'permanent-local t)

(defvar ein:notebooklist-buffer-name-template "*ein:notebooklist %s*")


(defun ein:notebooklist-url ()
  (concat (ein:base-project-url) "notebooks"))

(defun ein:notebooklist-new-url ()
  (concat (ein:base-project-url) "new"))

(defun ein:notebooklist-get-buffer ()
  (get-buffer-create
   (format ein:notebooklist-buffer-name-template ein:port)))

(defun ein:notebooklist-open (&optional no-popup)
  "Open notebook list buffer."
  (interactive)
  ;; FIXME: This function must ask server address or port number.
  (url-retrieve
   (ein:notebooklist-url)
   (if no-popup
       (lambda (s) (ein:notebooklist-url-retrieve-callback))
     (lambda (s) (pop-to-buffer (ein:notebooklist-url-retrieve-callback))))))

(defun ein:notebooklist-url-retrieve-callback ()
  "Called via `ein:notebooklist-open'."
  (let ((data (ein:json-read)))
    (kill-buffer (current-buffer))
    (with-current-buffer (ein:notebooklist-get-buffer)
      (setq ein:notebooklist-data data)
      (ein:notebooklist-render)
      (current-buffer))))

(defun ein:notebooklist-new-notebook ()
  "Ask server to create a new notebook and update the notebook list buffer."
  (message "Creating a new notebook...")
  (url-retrieve
   (ein:notebooklist-new-url)
   (lambda (s buffer)
     ;; To support opening notebook buffer from here will need parsing
     ;; HTML file.  Let's just reload notebook list buffer.
     (with-current-buffer buffer
       (ein:notebooklist-open t) ; FIXME: pass buffer to update (open)
       (message "Creating a new notebook... Done.")))
   (list (ein:notebooklist-get-buffer))))

;; FIXME: implement notebook deletion.
;; See `add_delete_button' in `notebooklist.js'.

(defun ein:notebooklist-render ()
  "Render notebook list widget.
Notebook list data is passed via the buffer local variable
`ein:notebooklist-data'."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Create notebook list
  (widget-insert "IPython Notebook list\n\n")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein:notebooklist-new-notebook))
   "New Notebook")
  (widget-insert "\n")
  (loop for note in ein:notebooklist-data
        for name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        do (progn (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (message "Open notebook %s." name)
                               (ein:notebook-open notebook-id)))
                   "Open")
                  (widget-insert " " name)
                  (widget-insert "\n")))
  (use-local-map widget-keymap)
  (widget-setup))

(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
