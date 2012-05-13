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

(defstruct ein:$notebooklist
  "Hold notebooklist variables.

`ein:$notebooklist-url-or-port'
  URL or port of IPython server.

`ein:$notebooklist-data'
  JSON data sent from the server."
  url-or-port
  data)

(ein:deflocal ein:notebooklist nil
  "Buffer local variable to store an instance of `ein:$notebooklist'.")

(defvar ein:notebooklist-buffer-name-template "*ein:notebooklist %s*")


(defvar ein:notebook-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "g" 'ein:notebooklist-reload)
    map))


(defun ein:notebooklist-url (url-or-port)
  (ein:url url-or-port "notebooks"))

(defun ein:notebooklist-new-url (url-or-port)
  (ein:url url-or-port "new"))

(defun ein:notebooklist-get-buffer (url-or-port)
  (get-buffer-create
   (format ein:notebooklist-buffer-name-template url-or-port)))

(defun ein:notebooklist-open (&optional url-or-port no-popup)
  "Open notebook list buffer."
  (interactive)
  ;; FIXME: This function must ask server address or port number.
  (unless url-or-port (setq url-or-port ein:default-port))
  (url-retrieve
   (ein:notebooklist-url url-or-port)
   (if no-popup
       #'ein:notebooklist-url-retrieve-callback
     (lambda (&rest args)
       (pop-to-buffer (apply #'ein:notebooklist-url-retrieve-callback args))))
   (list url-or-port)))

(defun ein:notebooklist-url-retrieve-callback (status url-or-port)
  "Called via `ein:notebooklist-open'."
  ;; FIXME: check status
  (let ((data (ein:json-read)))
    (kill-buffer (current-buffer))
    (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
      (setq ein:notebooklist
            (make-ein:$notebooklist :url-or-port url-or-port
                                    :data data))
      (ein:notebooklist-render)
      (current-buffer))))

(defun ein:notebooklist-reload ()
  "Reload current Notebook list."
  (interactive)
  (ein:notebooklist-open (ein:$notebooklist-url-or-port ein:notebooklist) t))

(defun ein:notebooklist-new-notebook ()
  "Ask server to create a new notebook and update the notebook list buffer."
  (message "Creating a new notebook...")
  (url-retrieve
   (ein:notebooklist-new-url (ein:$notebooklist-url-or-port ein:notebooklist))
   (lambda (s buffer)
     ;; To support opening notebook buffer from here will need parsing
     ;; HTML file.  Let's just reload notebook list buffer.
     (with-current-buffer buffer
       (ein:notebooklist-reload)
       (message "Creating a new notebook... Done.")))
   (list (current-buffer))))

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
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein:notebooklist-reload))
   "Reload List")
  (widget-insert "\n")
  (loop for note in (ein:$notebooklist-data ein:notebooklist)
        for name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        do (progn (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (message "Open notebook %s." name)
                               (ein:notebook-open
                                (ein:$notebooklist-url-or-port
                                 ein:notebooklist)
                                notebook-id)))
                   "Open")
                  (widget-insert " " name)
                  (widget-insert "\n")))
  (use-local-map ein:notebook-keymap)
  (widget-setup))

(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
