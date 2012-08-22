;;; ein-notebooklist.el --- Notebook list buffer

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

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
(require 'ein-subpackages)

(defcustom ein:notebooklist-first-open-hook nil
  "Hooks to run when the notebook list is opened at first time.

Example to open a notebook named _scratch_ when the notebook list
is opened at first time.::

  (add-hook
   'ein:notebooklist-first-open-hook
   (lambda () (ein:notebooklist-open-notebook-by-name \"_scratch_\")))

"
  :type 'hook
  :group 'ein)

(defstruct ein:$notebooklist
  "Hold notebooklist variables.

`ein:$notebooklist-url-or-port'
  URL or port of IPython server.

`ein:$notebooklist-data'
  JSON data sent from the server."
  url-or-port
  data)

(ein:deflocal ein:%notebooklist% nil
  "Buffer local variable to store an instance of `ein:$notebooklist'.")
(define-obsolete-variable-alias 'ein:notebooklist 'ein:%notebooklist% "0.1.2")

(defvar ein:notebooklist-buffer-name-template "*ein:notebooklist %s*")

(defvar ein:notebooklist-map (make-hash-table :test 'equal)
  "Data store for `ein:notebooklist-list'.
Mapping from URL-OR-PORT to an instance of `ein:$notebooklist'.")

(defun ein:notebooklist-list ()
  "Get a list of opened `ein:$notebooklist'."
  (ein:hash-vals ein:notebooklist-map))

(defun ein:notebooklist-list-add (nblist)
  "Register notebook list instance NBLIST for global lookup.
This function adds NBLIST to `ein:notebooklist-map'."
  (puthash (ein:$notebooklist-url-or-port nblist)
           nblist
           ein:notebooklist-map))

(defun ein:notebooklist-list-get (url-or-port)
  "Get an instance of `ein:$notebooklist' by URL-OR-PORT as a key."
  (gethash url-or-port ein:notebooklist-map))

(defun ein:notebooklist-open-notebook-by-name (name &optional url-or-port
                                                    callback cbargs)
  "Open notebook named NAME in the server URL-OR-PORT.
If URL-OR-PORT is not given or `nil', and the current buffer is
the notebook list buffer, the notebook is searched in the
notebook list of the current buffer.

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'.
To suppress popup, you can pass a function `ein:do-nothing' as CALLBACK."
  (loop with nblist = (if url-or-port
                          (ein:notebooklist-list-get url-or-port)
                        ein:%notebooklist%)
        for note in (ein:$notebooklist-data nblist)
        for notebook-name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        when (equal notebook-name name)
        return (ein:notebook-open (ein:$notebooklist-url-or-port nblist)
                                  notebook-id callback cbargs)))

(defun ein:notebooklist-url (url-or-port)
  (ein:url url-or-port "notebooks"))

(defun ein:notebooklist-new-url (url-or-port)
  (ein:url url-or-port "new"))

(defun ein:notebooklist-get-buffer (url-or-port)
  (get-buffer-create
   (format ein:notebooklist-buffer-name-template url-or-port)))

(defun ein:notebooklist-ask-url-or-port ()
  (let* ((url-or-port-list (mapcar (lambda (x) (format "%s" x))
                                   ein:url-or-port))
         (default (format "%s" (ein:aif (ein:get-notebook)
                                   (ein:$notebook-url-or-port it)
                                 (ein:aif ein:%notebooklist%
                                     (ein:$notebooklist-url-or-port it)
                                   (ein:default-url-or-port)))))
         (url-or-port
          (completing-read (format "URL or port number (default %s): " default)
                           url-or-port-list
                           nil nil nil nil
                           default)))
    (if (string-match "^[0-9]+$" url-or-port)
        (string-to-number url-or-port)
      url-or-port)))

;;;###autoload
(defun ein:notebooklist-open (&optional url-or-port no-popup)
  "Open notebook list buffer."
  (interactive (list (ein:notebooklist-ask-url-or-port)))
  (unless url-or-port (setq url-or-port (ein:default-url-or-port)))
  (ein:subpackages-load)
  (let ((success
         (if no-popup
             #'ein:notebooklist-url-retrieve-callback
           (lambda (&rest args)
             (pop-to-buffer
              (apply #'ein:notebooklist-url-retrieve-callback args))))))
    (ein:query-singleton-ajax
     (list 'notebooklist-open url-or-port)
     (ein:notebooklist-url url-or-port)
     :cache nil
     :parser #'ein:json-read
     :error (cons #'ein:notebooklist-open-error url-or-port)
     :success (cons success url-or-port)))
  (ein:notebooklist-get-buffer url-or-port))

(defun* ein:notebooklist-url-retrieve-callback (url-or-port
                                                &key
                                                status
                                                data
                                                &allow-other-keys)
  "Called via `ein:notebooklist-open'."
  (ein:aif (plist-get status :error)
      (error "Failed to connect to server '%s'.  Got: %S"
             (ein:url url-or-port) it))
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (let ((already-opened-p (ein:notebooklist-list-get url-or-port))
          (orig-point (point)))
      (setq ein:%notebooklist%
            (make-ein:$notebooklist :url-or-port url-or-port
                                    :data data))
      (ein:notebooklist-list-add ein:%notebooklist%)
      (ein:notebooklist-render)
      (goto-char orig-point)
      (message "Opened notebook list at %s" url-or-port)
      (unless already-opened-p
        (run-hooks 'ein:notebooklist-first-open-hook))
      (current-buffer))))

(defun* ein:notebooklist-open-error (url-or-port
                                     &key symbol-status
                                     &allow-other-keys)
  (ein:log 'error
    "Error (%s) while opening notebook list at the server %s."
    symbol-status url-or-port))

(defun ein:notebooklist-reload ()
  "Reload current Notebook list."
  (interactive)
  (ein:notebooklist-open (ein:$notebooklist-url-or-port ein:%notebooklist%) t))

(defun ein:notebooklist-refresh-related ()
  "Reload notebook list in which current notebook locates.
This function is called via `ein:notebook-after-rename-hook'."
  (ein:notebooklist-open (ein:$notebook-url-or-port ein:%notebook%) t))

(add-hook 'ein:notebook-after-rename-hook 'ein:notebooklist-refresh-related)

(defun ein:notebooklist-get-data-in-body-tag (key)
  "Very ad-hoc parser to get data in body tag."
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (search-forward "<body")
      (search-forward-regexp (format "%s=\\([^[:space:]\n]+\\)" key))
      (match-string 1))))

(defun ein:notebooklist-open-notebook (nblist notebook-id &optional name
                                              callback cbargs)
  (message "Open notebook %s." (or name notebook-id))
  (ein:notebook-open (ein:$notebooklist-url-or-port nblist) notebook-id
                     callback cbargs))

(defun ein:notebooklist-new-notebook (&optional url-or-port callback cbargs)
  "Ask server to create a new notebook and open it in a new buffer."
  (interactive (list (ein:notebooklist-ask-url-or-port)))
  (message "Creating a new notebook...")
  (unless url-or-port
    (setq url-or-port (ein:$notebooklist-url-or-port ein:%notebooklist%)))
  (assert url-or-port nil
          (concat "URL-OR-PORT is not given and the current buffer "
                  "is not the notebook list buffer."))
  (ein:query-singleton-ajax
   (list 'notebooklist-new-notebook url-or-port)
   (ein:notebooklist-new-url url-or-port)
   :parser (lambda ()
             (ein:notebooklist-get-data-in-body-tag "data-notebook-id"))
   :success (cons #'ein:notebooklist-new-notebook-callback
                  (list url-or-port callback cbargs))))

(defun* ein:notebooklist-new-notebook-callback (packed &key
                                                       data
                                                       &allow-other-keys
                                                       &aux
                                                       (notebook-id data)
                                                       (no-popup t))
  (destructuring-bind (url-or-port callback cbargs)
      packed
    (message "Creating a new notebook... Done.")
    (if notebook-id
        (progn
          (message "Open new notebook %s." notebook-id)
          (ein:notebook-open url-or-port notebook-id callback cbargs))
      (message (concat "Oops. EIN failed to open new notebook. "
                       "Please find it in the notebook list."))
      (setq no-popup nil))
    ;; reload or open notebook list
    (ein:notebooklist-open url-or-port no-popup)))

(defun ein:notebooklist-new-notebook-with-name (name &optional url-or-port)
  "Open new notebook and rename the notebook."
  (interactive "sNotebook name: ")
  (ein:notebooklist-new-notebook
   url-or-port
   (lambda (notebook created name)
     (assert created)
     (with-current-buffer (ein:notebook-buffer notebook)
       (ein:notebook-rename-command name)
       ;; As `ein:notebook-open' does not call `pop-to-buffer' when
       ;; callback is specified, `pop-to-buffer' must be called here:
       (pop-to-buffer (current-buffer))))
   (list name)))

(defun ein:notebooklist-new-scratch-notebook ()
  "Open a notebook to try random thing.
Notebook name is determined based on
`ein:scratch-notebook-name-template'."
  (interactive)
  (ein:notebooklist-new-notebook-with-name
   (ein:scratch-notebook-name)
   (ein:default-url-or-port)))

(defun ein:notebooklist-delete-notebook-ask (notebook-id name)
  (when (y-or-n-p (format "Delete notebook %s?" name))
    (ein:notebooklist-delete-notebook notebook-id name)))

(defun ein:notebooklist-delete-notebook (notebook-id name)
  (message "Deleting notebook %s..." name)
  (ein:query-singleton-ajax
   (list 'notebooklist-delete-notebook
         (ein:$notebooklist-url-or-port ein:%notebooklist%) notebook-id)
   (ein:notebook-url-from-url-and-id
    (ein:$notebooklist-url-or-port ein:%notebooklist%)
    notebook-id)
   :cache nil
   :type "DELETE"
   :success (cons (lambda (packed &rest ignore)
                    (message "Deleting notebook %s... Done." (cdr packed))
                    (with-current-buffer (car packed)
                      (ein:notebooklist-reload)))
                  (cons (current-buffer) name))))

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
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore)
             (browse-url
              (ein:url (ein:$notebooklist-url-or-port ein:%notebooklist%))))
   "Open In Browser")
  (widget-insert "\n")
  (loop for note in (ein:$notebooklist-data ein:%notebooklist%)
        for name = (plist-get note :name)
        for notebook-id = (plist-get note :notebook_id)
        do (progn (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (ein:notebooklist-open-notebook
                                ein:%notebooklist% notebook-id name)))
                   "Open")
                  (widget-insert " ")
                  (widget-create
                   'link
                   :notify (lexical-let ((name name)
                                         (notebook-id notebook-id))
                             (lambda (&rest ignore)
                               (ein:notebooklist-delete-notebook-ask
                                notebook-id
                                name)))
                   "Delete")
                  (widget-insert " : " name)
                  (widget-insert "\n")))
  (ein:notebooklist-mode)
  (widget-setup))

(defun ein:notebooklist-list-notebooks ()
  "Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\"."
  (apply #'append
         (loop for nblist in (ein:notebooklist-list)
               for url-or-port = (ein:$notebooklist-url-or-port nblist)
               collect
               (loop for note in (ein:$notebooklist-data nblist)
                     collect (format "%s/%s"
                                     url-or-port
                                     (plist-get note :name))))))

(defun ein:notebooklist-open-notebook-global (nbpath &optional callback cbargs)
  "Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'."
  (interactive
   (list (completing-read
          "Open notebook [URL-OR-PORT/NAME]: "
          (ein:notebooklist-list-notebooks))))
  (let* ((path (split-string nbpath "/"))
         (url-or-port (car path))
         (name (cadr path)))
    (when (and (stringp url-or-port)
               (string-match "^[0-9]+$" url-or-port))
      (setq url-or-port (string-to-number url-or-port)))
    (let ((notebook-id
           (loop for nblist in (ein:notebooklist-list)
                 when (equal (ein:$notebooklist-url-or-port nblist) url-or-port)
                 if (loop for note in (ein:$notebooklist-data nblist)
                          when (equal (plist-get note :name) name)
                          return (plist-get note :notebook_id))
                 return it)))
      (if notebook-id
          (ein:notebook-open url-or-port notebook-id callback cbargs)
        (message "Notebook '%s' not found" nbpath)))))


;;; Generic getter

(defun ein:get-url-or-port--notebooklist ()
  (when (ein:$notebooklist-p ein:%notebooklist%)
    (ein:$notebooklist-url-or-port ein:%notebooklist%)))


;;; Notebook list mode

(define-derived-mode ein:notebooklist-mode fundamental-mode "ein:notebooklist"
  "IPython notebook list mode.")

(defun ein:notebooklist-prev-item () (interactive) (move-beginning-of-line 0))
(defun ein:notebooklist-next-item () (interactive) (move-beginning-of-line 2))

(setq ein:notebooklist-mode-map (copy-keymap widget-keymap))

(let ((map ein:notebooklist-mode-map))
  (define-key map "\C-c\C-r" 'ein:notebooklist-reload)
  (define-key map "g" 'ein:notebooklist-reload)
  (define-key map "p" 'ein:notebooklist-prev-item)
  (define-key map "n" 'ein:notebooklist-next-item)
  (define-key map "q" 'bury-buffer))

(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
