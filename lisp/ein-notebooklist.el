;;; ein-notebooklist.el --- Notebook list buffer    -*- lexical-binding:t -*-

;; Copyright (C) 2018- John M. Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

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

;;; Code:

(require 'widget)
(require 'cus-edit)

(require 'ein-core)
(require 'ein-contents-api)
(require 'deferred)
(require 'dash)
(require 'ido)

(declare-function ein:jupyter-crib-token "ein-jupyter")
(declare-function ein:jupyter-get-default-kernel "ein-jupyter")
(declare-function ein:jupyter-crib-running-servers "ein-jupyter")
(declare-function ein:file-open "ein-file")
(autoload 'ein:get-notebook "ein-notebook")

(defcustom ein:notebooklist-login-timeout (truncate (* 6.3 1000))
  "Timeout in milliseconds for logging into server"
  :group 'ein
  :type 'integer)

(make-obsolete-variable 'ein:notebooklist-first-open-hook nil "0.17.0")

(cl-defstruct ein:$notebooklist
  "Hold notebooklist variables.

`ein:$notebooklist-url-or-port'
  URL or port of IPython server.

`ein:$notebooklist-path'
  The path for the notebooklist.

`ein:$notebooklist-data'
  JSON data sent from the server.
`ein:$notebooklist-api-version'
  Major version of the IPython notebook server we are talking to."
  url-or-port
  path
  data
  api-version)

(define-obsolete-variable-alias 'ein:notebooklist 'ein:%notebooklist% "0.1.2")
(ein:deflocal ein:%notebooklist% nil
  "Buffer local variable to store an instance of `ein:$notebooklist'.")

(ein:deflocal ein:%notebooklist-new-kernel% nil
  "Buffer local variable to store kernel type for newly created notebooks.")

(defcustom ein:notebooklist-sort-field :name
  "The notebook list sort field."
  :type '(choice (const :tag "Name" :name)
                 (const :tag "Last modified" :last_modified))
  :group 'ein)

(defcustom ein:notebooklist-sort-order :ascending
  "The notebook list sort order."
  :type '(choice (const :tag "Ascending" :ascending)
                 (const :tag "Descending" :descending))
  :group 'ein)

(defvar ein:notebooklist-buffer-name-template "*ein:notebooklist %s*")

(defvar ein:notebooklist-map (make-hash-table :test 'equal)
  "Data store for `ein:notebooklist-list'.
Mapping from URL-OR-PORT to an instance of `ein:$notebooklist'.")

(defun ein:notebooklist-keys ()
  "Get a list of registered server urls."
  (hash-table-keys ein:notebooklist-map))

(defun ein:notebooklist-list ()
  "Get a list of opened `ein:$notebooklist'."
  (hash-table-values ein:notebooklist-map))

(defun ein:notebooklist-list-remove (url-or-port)
  (remhash url-or-port ein:notebooklist-map))

(defun ein:notebooklist-list-add (nblist)
  "Register notebook list instance NBLIST for global lookup.
This function adds NBLIST to `ein:notebooklist-map'."
  (puthash (ein:$notebooklist-url-or-port nblist)
           nblist
           ein:notebooklist-map))

(defun ein:notebooklist-list-get (url-or-port)
  "Get an instance of `ein:$notebooklist' by URL-OR-PORT as a key."
  (gethash url-or-port ein:notebooklist-map))

(defsubst ein:notebooklist-url (url-or-port &rest paths)
  (apply #'ein:url url-or-port "api/contents" paths))

(defun ein:notebooklist-sentinel (url-or-port process event)
  "Remove URL-OR-PORT from ein:notebooklist-map when PROCESS dies"
  (when (not (string= "open" (substring event 0 4)))
    (ein:log 'info "Process %s %s %s"
             (car (process-command process))
             (replace-regexp-in-string "\n$" "" event)
             url-or-port)
    (ein:notebooklist-list-remove url-or-port)))

(defun ein:notebooklist-get-buffer (url-or-port)
  (get-buffer-create
   (format ein:notebooklist-buffer-name-template url-or-port)))

(defun ein:notebooklist-token-or-password (url-or-port)
  "Return token or password for URL-OR-PORT.

Jupyter requires one or the other but not both.
Return empty string token if all authentication disabled.
Return nil if unclear what, if any, authentication applies."
  (cl-multiple-value-bind (password-p token) (ein:jupyter-crib-token url-or-port)
    (cond ((eq password-p t) (read-passwd (format "Password for %s: " url-or-port)))
          ((and (stringp token) (eq password-p :json-false)) token)
          (t nil))))

(defun ein:notebooklist-ask-url-or-port ()
  (let* ((default (ein:url (aif (ein:get-notebook)
                               (ein:$notebook-url-or-port it)
                             (aif ein:%notebooklist%
                                 (ein:$notebooklist-url-or-port it)))))
         (url-or-port-list
          (-distinct (mapcar #'ein:url
                             (append (when default (list default))
                                     (if (stringp ein:urls)
                                         (list ein:urls)
                                       ein:urls)
                                     (mapcar
                                      (lambda (json)
                                        (cl-destructuring-bind (&key url &allow-other-keys)
                                            json
                                          (ein:url url)))
                                      (ein:jupyter-crib-running-servers))))))
         (url-or-port (let (ido-report-no-match ido-use-faces)
                        (ein:completing-read "URL or port: "
                                             url-or-port-list
                                             nil nil nil nil
                                             (car-safe url-or-port-list)))))
    (ein:url url-or-port)))

(defsubst ein:notebooklist-canonical-url-or-port (url-host username)
  "Canonicalize.
For the record,
  https://hub.data8x.berkeley.edu
needs to look like
  https://hub.data8x.berkeley.edu/user/1dcdab3c2f59736806b85af865a1a28d"
  (ein:url url-host "user" username))

(cl-defun ein:notebooklist-open* (url-or-port &optional path resync callback errback hub-p
                                              &aux (canonical-p (not hub-p)) tokens-key)
  "Workhorse of `ein:login'.

A notebooklist can be opened from any PATH within the server root hierarchy.
PATH is empty at the root.  RESYNC, when non-nil, requeries the contents-api
version and kernelspecs.

Full jupyterhub url is https://hub.data8x.berkeley.edu/user/1dcdab3c2f59736806b85af865a1a28d/?token=c421c6863ddb4e7ea5a311c31c948cd0

URL-HOST is hub.data8x.berkeley.edu
USERNAME is 1dcdab3c2f59736806b85af865a1a28d
TOKEN is c421c6863ddb4e7ea5a311c31c948cd0

CALLBACK takes two arguments, the resulting buffer and URL-OR-PORT.
ERRBACK takes one argument, the resulting buffer."
  (setq path (or path ""))
  (if (and (not resync) (ein:notebooklist-list-get url-or-port))
      (ein:content-query-contents
       url-or-port path
       (apply-partially #'ein:notebooklist-open--finish url-or-port callback)
       errback)
    (when hub-p
      (let* ((parsed-url (url-generic-parse-url url-or-port))
             (url-host (url-host parsed-url))
             (cookies (ein:query-get-cookies url-host "/user/"))
             (previous-users
              (mapcar
               (lambda (entry)
                 (file-name-nondirectory (directory-file-name (plist-get entry :path))))
               cookies))
             (pq (url-path-and-query parsed-url))
             (path0 (car pq))
             (query (cdr pq))
             (_ (setf canonical-p
                      (and (stringp path0)
                           (string-match "user/\\([a-z0-9]+\\)" path0))))
             (username (if canonical-p
                           (match-string-no-properties 1 path0)
                         (read-no-blanks-input "User: " (car previous-users))))
             (_ (setf url-or-port
                      (ein:notebooklist-canonical-url-or-port url-host username)))
             (_ (setf tokens-key
                      (ein:query-divine-authorization-tokens-key url-or-port)))
             (token
              (if (and (stringp query)
                       (string-match "token=\\([a-z0-9]+\\)" query))
                  (prog1
                      (match-string-no-properties 1 query)
                    (cl-assert canonical-p))
                (when canonical-p
                  (read-no-blanks-input "Token: ")))))
        (when token
          (setf (gethash tokens-key ein:query-authorization-tokens) token))))
    (if (not canonical-p)
        ;; Retread to get _xsrf for canonical url
        (progn
          (ein:notebooklist-list-remove url-or-port)
          (ein:notebooklist-login--iteration url-or-port callback errback nil -1 nil))
      (when tokens-key
        (let ((belay-tokens
               (lambda (&rest _args)
                 (remhash tokens-key ein:query-authorization-tokens))))
          (add-function :before (var errback) belay-tokens)
          (add-function :before (var callback) belay-tokens)))
      (ein:query-notebook-api-version
       url-or-port
       (lambda ()
         (ein:query-kernelspecs
          url-or-port
          (lambda ()
            (deferred:$
              (deferred:next
                (lambda ()
                  (ein:content-query-hierarchy url-or-port))))
            (ein:content-query-contents
             url-or-port path
             (apply-partially #'ein:notebooklist-open--finish url-or-port callback)
             errback))))))))

(make-obsolete-variable 'ein:notebooklist-keepalive-refresh-time nil "0.17.0")
(make-obsolete-variable 'ein:enable-keepalive nil "0.17.0")

(defcustom ein:notebooklist-date-format "%F"
  "The format spec for date in notebooklist mode.
See `ein:format-time-string'."
  :type '(or string function)
  :group 'ein)

(defun ein:notebooklist-open--finish (url-or-port callback content)
  "Called via `ein:notebooklist-open*'."
  (ein:log 'verbose "Opening notebooklist at %s"
           (ein:url url-or-port (ein:$content-path content)))
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (ein:notebooklist-mode)
    (let ((restore-point (aand (widget-at)
                               (awhen (widget-value it)
                                 (and (stringp it) it))
                               (string-match-p "Open\\|Stop\\|Delete" it)
                               (point))))
      (awhen ein:%notebooklist%
        (ein:notebooklist-list-remove (ein:$notebooklist-url-or-port it)))
      (setq ein:%notebooklist%
            (make-ein:$notebooklist :url-or-port url-or-port
                                    :path (ein:$content-path content)
                                    :data (ein:$content-raw-content content)
                                    :api-version (ein:$content-notebook-api-version content)))
      (ein:notebooklist-list-add ein:%notebooklist%)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (when callback
        (funcall callback (current-buffer) url-or-port))
      (ein:content-query-sessions url-or-port (apply-partially #'ein:notebooklist-render
                                                               url-or-port
                                                               restore-point))
      (current-buffer))))

(cl-defun ein:notebooklist-open-error (url-or-port path
                                       &key error-thrown &allow-other-keys)
  (ein:log 'error
    "ein:notebooklist-open-error %s: ERROR %s DATA %s" (concat (file-name-as-directory url-or-port) path) (car error-thrown) (cdr error-thrown)))

;;;###autoload
(defun ein:notebooklist-reload (&optional nblist resync callback)
  "Reload current Notebook list."
  (interactive)
  (setq nblist (or nblist ein:%notebooklist%))
  (ein:notebooklist-open* (ein:$notebooklist-url-or-port nblist)
                          (ein:$notebooklist-path nblist) resync callback))

;;;###autoload
(defun ein:notebooklist-new-notebook (url-or-port kernelspec &optional callback no-pop retry explicit-path)
  (interactive (list (ein:notebooklist-ask-url-or-port)
                     (ein:completing-read
                      "Select kernel: "
                      (ein:list-available-kernels
                       (ein:$notebooklist-url-or-port ein:%notebooklist%))
                      nil t nil nil "default" nil)))
  (let* ((notebooklist (ein:notebooklist-list-get url-or-port))
         (path (or explicit-path (ein:$notebooklist-path notebooklist)))
         (url (ein:notebooklist-url url-or-port path)))
    (ein:query-singleton-ajax
     url
     :type "POST"
     :data (ein:json-encode '((type . "notebook")))
     :headers (list (cons "Content-Type" "application/json"))
     :parser #'ein:json-read
     :error (apply-partially #'ein:notebooklist-new-notebook-error
                             url-or-port kernelspec callback no-pop retry explicit-path)
     :success (apply-partially #'ein:notebooklist-new-notebook-success
                               url-or-port kernelspec
                               path
                               callback no-pop))))

(cl-defun ein:notebooklist-new-notebook-success (url-or-port
                                                 kernelspec
                                                 path
                                                 callback
                                                 no-pop
                                                 &key data
                                                 &allow-other-keys)
  (let ((nbpath (plist-get data :path)))
    (ein:notebook-open url-or-port nbpath kernelspec callback nil no-pop)
    (ein:notebooklist-open* url-or-port path)))

(cl-defun ein:notebooklist-new-notebook-error
    (url-or-port kernelspec callback no-pop retry explicit-path
                 &key symbol-status error-thrown &allow-other-keys)
  (let ((notice (format "ein:notebooklist-new-notebook-error: %s %s"
                        symbol-status error-thrown)))
    (if retry
        (ein:log 'error notice)
      (ein:log 'info notice)
      (sleep-for 0 1500)
      (ein:notebooklist-new-notebook url-or-port kernelspec callback no-pop t explicit-path))))

;;;###autoload
(defun ein:notebooklist-new-notebook-with-name
    (url-or-port kernelspec name &optional callback no-pop)
  "Upon notebook-open, rename the notebook, then funcall CALLBACK."
  (interactive
   (let ((url-or-port (ein:get-url-or-port)))
     (unless url-or-port
       (error "ein:notebooklist-new-notebook-with-name: no server context"))
     (let ((kernelspec (ein:completing-read
                        "Select kernel: "
                        (ein:list-available-kernels url-or-port)
                        nil t nil nil "default" nil))
           (name (read-from-minibuffer
                  (format "Notebook name (at %s): " url-or-port))))
       (list url-or-port kernelspec name))))
  (unless callback
    (setq callback #'ignore))
  (add-function :before (var callback)
                (apply-partially
                 (lambda (name* notebook _created)
                   (with-current-buffer (ein:notebook-buffer notebook)
                     (ein:notebook-rename-command name*)))
                 name))
  (ein:notebooklist-new-notebook url-or-port kernelspec callback no-pop))

(defun ein:notebooklist-delete-notebook (_notebooklist url-or-port path &optional callback)
  "CALLBACK with no arguments, e.g., semaphore"
  (setq callback (or callback #'ignore))
  (dolist (buf (seq-filter (lambda (b)
                             (with-current-buffer b
                               (aif (ein:get-notebook)
                                   (string= path (ein:$notebook-notebook-path it)))))
                           (buffer-list)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) nil)))
      (kill-buffer buf)))
  (if (ein:notebook-opened-notebooks (lambda (nb)
                                       (string= path
                                                (ein:$notebook-notebook-path nb))))
      (ein:log 'error "ein:notebooklist-delete-notebook: cannot close %s" path)
    (let ((delete-nb
           (apply-partially
            (lambda (url* settings* _kernel)
              (apply #'ein:query-singleton-ajax url* settings*))
            (ein:notebooklist-url url-or-port path)
            (list :type "DELETE"
                  :complete (apply-partially
                             #'ein:notebooklist-delete-notebook--complete
                             (ein:url url-or-port path) callback)))))
      (ein:message-whir
       "Ending session" (var delete-nb)
       (ein:kernel-delete-session delete-nb
                                  :url-or-port url-or-port
                                  :path path)))))

(cl-defun ein:notebooklist-delete-notebook--complete
    (_url callback
     &key data response _symbol-status
     &allow-other-keys
     &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:notebooklist-delete-notebook--complete %s" resp-string)
  (when callback (funcall callback)))

(defun generate-breadcrumbs (path)
  "Given notebooklist path, generate alist of breadcrumps of form (name . path)."
  (let* ((paths (split-string path "/" t))
         (current-path "/")
         (pairs (list (cons "Home" ""))))
    (dolist (p paths pairs)
      (setf current-path (concat current-path "/" p)
            pairs (append pairs (list (cons p current-path)))))))

(cl-defun ein:nblist--sort-group (group by-param order)
  (sort group #'(lambda (x y)
                  (cond ((eq order :ascending)
                         (string-lessp (plist-get x by-param)
                                       (plist-get y by-param)))
                        ((eq order :descending)
                         (string-greaterp (plist-get x by-param)
                                          (plist-get y by-param)))))))

(defun ein:notebooklist--order-data (nblist-data sort-param sort-order)
  "Try to sanely sort the notebooklist data for the current path."
  (let* ((groups (-group-by (lambda (x) (plist-get x :type)) nblist-data))
         (dirs (ein:nblist--sort-group (cdr (assoc "directory" groups))
                                       sort-param
                                       sort-order))
         (nbs (ein:nblist--sort-group (cdr (assoc "notebook" groups))
                                      sort-param
                                      sort-order))
         (files (ein:nblist--sort-group
                 (-flatten-n 1 (-map #'cdr (-group-by
                                            #'(lambda (x) (car (last (split-string (plist-get x :name) "\\."))))
                                            (cdr (assoc "file" groups)))))
                 sort-param
                 sort-order)))
    (-concat dirs nbs files)))

(defun render-header (url-or-port &rest _args)
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (widget-insert
     (format "Contents API %s (%s)\n\n"
             (ein:need-notebook-api-version url-or-port)
             url-or-port))
    (let ((breadcrumbs (generate-breadcrumbs
                        (ein:$notebooklist-path ein:%notebooklist%))))
      (dolist (p breadcrumbs)
        (let ((url-or-port url-or-port)
              (name (car p))
              (path (cdr p)))
          (widget-insert " | ")
          (widget-create
           'link
           :notify (lambda (&rest _ignore)
                     (ein:notebooklist-open* url-or-port path nil
                                             (lambda (buffer _url-or-port)
                                               (pop-to-buffer buffer))))
           name)))
      (widget-insert " |\n\n"))
    (let* ((url-or-port url-or-port)
           (kernels (ein:list-available-kernels url-or-port)))
      (widget-create
       'link
       :notify (lambda (&rest _ignore) (ein:notebooklist-new-notebook
                                        url-or-port
                                        ein:%notebooklist-new-kernel%))
       "New Notebook")
      (widget-insert " ")
      (widget-create
       'link
       :notify (lambda (&rest _ignore) (ein:notebooklist-reload nil t))
       "Resync")
      (widget-insert " ")
      (widget-create
       'link
       :notify (lambda (&rest _ignore)
                 (browse-url (ein:url url-or-port)))
       "Open In Browser")

      (widget-insert "\n\nCreate New Notebooks Using Kernel:\n")
      (let ((radio-widget
             (widget-create
              'radio-button-choice
              :notify (lambda (widget &rest _args)
                        (let ((update (ein:get-kernelspec url-or-port
                                                          (widget-value widget))))
                          (unless (equal ein:%notebooklist-new-kernel% update)
                            (when ein:%notebooklist-new-kernel%
                              (message "New notebooks started with %s kernel"
                                       (ein:$kernelspec-display-name update)))
                            (setq ein:%notebooklist-new-kernel% update)))))))
        (if kernels
            (let ((initial (ein:jupyter-get-default-kernel kernels)))
              (dolist (k kernels)
                (let ((child (widget-radio-add-item
                              radio-widget
                              (list 'item
                                    :value (car k)
                                    :format (format "%s\n" (cdr k))))))
                  (when (string= initial (car k))
                    (widget-apply-action (widget-get child :button)))))
              (widget-insert "\n"))
          (widget-insert "\n  No kernels found\n"))))))

(defun ein:format-nbitem-data (name last-modified)
  (let ((dt (date-to-time last-modified)))
    (format "%-40s%+20s" name
            (ein:format-time-string ein:notebooklist-date-format dt))))

(defun render-directory (url-or-port sessions)
  ;; SESSIONS is a hashtable of path to (session-id . kernel-id) pairs
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (cl-loop with reloader = (apply-partially (lambda (nblist _kernel)
                                                (ein:notebooklist-reload nblist))
                                              ein:%notebooklist%)
             for note in (ein:notebooklist--order-data
                          (ein:$notebooklist-data ein:%notebooklist%)
                          ein:notebooklist-sort-field
                          ein:notebooklist-sort-order)
             for name = (plist-get note :name)
             for path = (plist-get note :path)
             for last-modified = (plist-get note :last_modified)
             for type = (plist-get note :type)
             do (ein:notebook-get-opened-notebook url-or-port path)
             if (string= type "directory")
             do (progn (widget-create
                        'link
                        :notify (let ((url-or-port url-or-port)
                                      (name name))
                                  (lambda (&rest _ignore)
                                    ;; each directory creates a whole new notebooklist
                                    (ein:notebooklist-open* url-or-port
                                                            (concat (file-name-as-directory
                                                                     (ein:$notebooklist-path ein:%notebooklist%))
                                                                    name)
                                                            nil
                                                            (lambda (buffer _url-or-port) (pop-to-buffer buffer)))))
                        "Dir")
                       (widget-insert " : " name)
                       (widget-insert "\n"))
             end
             if (string= type "file")
             do (progn (widget-create
                        'link
                        :notify (apply-partially
                                 (lambda (url-or-port* path* &rest _args)
                                   (ein:file-open url-or-port* path*))
                                 url-or-port path)
                        "Open")
                       (widget-insert "                ")
                       (widget-insert " : " (ein:format-nbitem-data name last-modified))
                       (widget-insert "\n"))
             end
             if (string= type "notebook")
             do (progn (widget-create
                        'link
                        :notify (apply-partially
                                 (lambda (url-or-port* path* &rest _args)
                                   (ein:notebook-open url-or-port* path*))
                                 url-or-port path)
                        "Open")
                       (widget-insert " ")
                       (if (gethash path sessions)
                           (widget-create
                            'link
                            :notify
                            (apply-partially
                             (cl-function
                              (lambda (url-or-port*
                                       path*
                                       &rest _ignore
                                       &aux (callback (lambda (_kernel) t)))
                                (ein:message-whir
                                 "Ending session" (var callback)
                                 (ein:kernel-delete-session callback
                                                            :url-or-port url-or-port*
                                                            :path path*))))
                             url-or-port path)
                            "Stop")
                         (widget-insert "[----]"))
                       (widget-insert " ")
                       (widget-create
                        'link
                        :notify (apply-partially
                                 (lambda (notebooklist* url-or-port* path* callback*
                                          &rest _args)
                                   (when (or noninteractive
                                             (y-or-n-p (format "Delete notebook %s?" path*)))
                                     (ein:notebooklist-delete-notebook
                                       notebooklist* url-or-port* path*
                                       (apply-partially callback* nil))))
                                 ein:%notebooklist% url-or-port path reloader)
                        "Delete")
                       (widget-insert " : " (ein:format-nbitem-data name last-modified))
                       (widget-insert "\n"))
             end)))

(defun ein:notebooklist-render (url-or-port restore-point sessions)
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (if (not (ein:$notebooklist-path ein:%notebooklist%))
        (ein:log 'error "ein:notebooklist-render: cannot render null")
      (render-header url-or-port sessions)
      (render-directory url-or-port sessions)
      (widget-setup)
      (awhen (get-buffer-window (current-buffer))
        (set-window-point it (or restore-point (point-min)))))))

;;;###autoload
(defun ein:notebooklist-list-paths (&optional content-type)
  "Return all files of CONTENT-TYPE for all sessions"
  (apply #'append
         (cl-loop for nblist in (ein:notebooklist-list)
               for url-or-port = (ein:$notebooklist-url-or-port nblist)
               collect
               (cl-loop for content in (ein:content-need-hierarchy url-or-port)
                     when (or (null content-type)
                              (string= (ein:$content-type content) content-type))
                     collect (ein:url url-or-port (ein:$content-path content))))))


(defun ein:notebooklist-parse-nbpath (nbpath)
  "Return `(,url-or-port ,path) from URL-OR-PORT/PATH"
  (cl-loop for url-or-port in (ein:notebooklist-keys)
           if (cl-search url-or-port nbpath :end2 (length url-or-port))
           return (list (substring nbpath 0 (length url-or-port))
                        (substring nbpath (1+ (length url-or-port))))
           end
           finally (ein:display-warning
                    (format "%s not among: %s" nbpath (ein:notebooklist-keys))
                    :error)))

(defsubst ein:notebooklist-ask-path (&optional content-type)
  (ein:completing-read (format  "Open %s: " content-type)
                       (ein:notebooklist-list-paths content-type)
                       nil t))

;;;###autoload

(defun ein:notebooklist-load (&optional url-or-port)
  "Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein:notebooklist-load)"
  (ein:notebooklist-open* url-or-port))

;;; Login

(defun ein:notebooklist-login--iteration (url-or-port callback errback token iteration response-status)
  (ein:log 'debug "Login attempt #%d in response to %s from %s."
           iteration response-status url-or-port)
  (setq callback (or callback #'ignore))
  (setq errback (or errback #'ignore))
  (let* ((reset-p (not response-status))
         (request-curl-options (if reset-p
                                   (cons "--junk-session-cookies" request-curl-options)
                                 request-curl-options))
         (parsed-url (url-generic-parse-url (file-name-as-directory url-or-port)))
         (host (url-host parsed-url))
         (query (cdr (url-path-and-query parsed-url))))
    (when reset-p
      (remhash host ein:query-xsrf-cache))
    (ein:query-singleton-ajax
     (ein:url url-or-port (if query "" "login"))
     ;; do not use :type "POST" here (see git history)
     :timeout ein:notebooklist-login-timeout
     :data (when (and token (not query)) (concat "password=" (url-hexify-string token)))
     :parser #'ein:notebooklist-login--parser
     :complete (apply-partially #'ein:notebooklist-login--complete url-or-port)
     :error (apply-partially #'ein:notebooklist-login--error url-or-port token
                             callback errback iteration)
     :success (apply-partially #'ein:notebooklist-login--success url-or-port callback
                               errback token iteration))))

;;;###autoload
(defun ein:notebooklist-open (url-or-port callback)
  "This is now an alias for `ein:notebooklist-login'."
  (interactive `(,(ein:notebooklist-ask-url-or-port)
                 ,(lambda (buffer _url-or-port) (pop-to-buffer buffer))))
  (ein:notebooklist-login url-or-port callback))

(make-obsolete 'ein:notebooklist-open 'ein:notebooklist-login "0.14.2")

;;;###autoload
(defalias 'ein:login 'ein:notebooklist-login)

;;;###autoload
(defun ein:notebooklist-login (url-or-port callback &optional cookie-name cookie-content token)
  "Deal with security before main entry of ein:notebooklist-open*.
CALLBACK takes two arguments, the buffer created by
ein:notebooklist-open--success and the url-or-port argument of
ein:notebooklist-open*."
  (interactive `(,(ein:notebooklist-ask-url-or-port)
                 ,(lambda (buffer _url-or-port) (pop-to-buffer buffer))
                 ,(when current-prefix-arg
                    (read-no-blanks-input "Cookie name: "))
                 ,(when current-prefix-arg
                    (read-no-blanks-input "Cookie content: "))
		 nil))
  (when cookie-name
    (let* ((parsed-url (url-generic-parse-url (file-name-as-directory url-or-port)))
           (domain (url-host parsed-url))
           (securep (string-match "^wss://" url-or-port))
           (line (mapconcat #'identity (list domain "FALSE" (car (url-path-and-query parsed-url)) (if securep "TRUE" "FALSE") "0" cookie-name (concat cookie-content "\n")) "\t")))
      (write-region line nil (request--curl-cookie-jar) 'append)))
  (let ((token (or token (ein:notebooklist-token-or-password url-or-port))))
    (cond ((null token) ;; don't know
           (ein:notebooklist-login--iteration url-or-port callback nil nil -1 nil))
          ((string= token "") ;; all authentication disabled
           (ein:log 'verbose "Skipping login %s" url-or-port)
           (ein:notebooklist-open* url-or-port nil nil callback nil))
          (t
           (ein:notebooklist-login--iteration url-or-port callback nil token 0 nil)))))

(defun ein:notebooklist-login--parser ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "<input type=.?password" nil t)
      (list :reprompt t))))

(defun ein:notebooklist-login--success-1 (url-or-port callback errback &optional hub-p)
  (ein:log 'info "Login to %s complete." url-or-port)
  (ein:notebooklist-open* url-or-port nil nil callback errback hub-p))

(defun ein:notebooklist-login--error-1 (url-or-port error-thrown response errback)
  (ein:log 'error "Login to %s failed, error-thrown %s, raw-header %s"
           url-or-port
           (subst-char-in-string ?\n ?\  (format "%s" error-thrown))
           (request-response--raw-header response))
  (funcall errback))

(cl-defun ein:notebooklist-login--complete
    (_url-or-port
     &key data response
     &allow-other-keys &aux
     (resp-string (format "STATUS: %s DATA: %s"
                          (request-response-status-code response) data)))
  (ein:log 'debug "ein:notebooklist-login--complete %s" resp-string))

(cl-defun ein:notebooklist-login--success
    (url-or-port callback errback token iteration
     &key data response error-thrown
     &allow-other-keys &aux
     (response-status (request-response-status-code response))
     (hub-p (request-response-header response "x-jupyterhub-version")))
  (if (plist-get data :reprompt)
      (cond ((>= iteration 0)
             (ein:notebooklist-login--error-1 url-or-port error-thrown response errback))
            (hub-p (ein:notebooklist-open* url-or-port nil nil callback errback t))
            (t (setq token (read-passwd (format "Password for %s: " url-or-port)))
               (ein:notebooklist-login--iteration url-or-port callback errback token
                                                  (1+ iteration) response-status)))
    (ein:notebooklist-login--success-1 url-or-port callback errback hub-p)))

(cl-defun ein:notebooklist-login--error
    (url-or-port token callback errback iteration
     &key _data response error-thrown
     &allow-other-keys &aux
     (response-status (request-response-status-code response))
     (hub-p (request-response-header response "x-jupyterhub-version")))
  (cond (hub-p
         (if (< iteration 0)
             (ein:notebooklist-login--iteration url-or-port callback errback
                                                token (1+ iteration) response-status)
           (if (and (eq response-status 405)) ;; no javascript is okay
               (ein:notebooklist-login--success-1 url-or-port callback errback hub-p)
             (ein:notebooklist-login--error-1 url-or-port error-thrown response errback))))
        ((and response-status (< iteration 0))
         (setq token (read-passwd (format "Password for %s: " url-or-port)))
         (ein:notebooklist-login--iteration url-or-port callback errback token (1+ iteration) response-status))
        ((and (eq response-status 403) (< iteration 1))
         (ein:notebooklist-login--iteration url-or-port callback errback token (1+ iteration) response-status))
        (t (ein:notebooklist-login--error-1 url-or-port error-thrown response errback))))

(defun ein:get-url-or-port--notebooklist ()
  (when (ein:$notebooklist-p ein:%notebooklist%)
    (ein:$notebooklist-url-or-port ein:%notebooklist%)))

(defun ein:notebooklist-prev-item () (interactive) (move-beginning-of-line 0))
(defun ein:notebooklist-next-item () (interactive) (move-beginning-of-line 2))

(defvar ein:notebooklist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (define-key map "\C-c\C-r" 'ein:notebooklist-reload)
    (define-key map "\C-c\C-f" 'ein:file-open)
    (define-key map "\C-c\C-o" 'ein:notebook-open)
    (define-key map "p" 'ein:notebooklist-prev-item)
    (define-key map "n" 'ein:notebooklist-next-item)
    map)
  "Keymap for ein:notebooklist-mode.")

(easy-menu-define ein:notebooklist-menu ein:notebooklist-mode-map
  "EIN Notebook List Mode Menu"
  `("EIN Notebook List"
    ,@(ein:generate-menu
       '(("Reload" ein:notebooklist-reload)
         ("New Notebook" ein:notebooklist-new-notebook)
         ("New Notebook (with name)"
          ein:notebooklist-new-notebook-with-name)))))

(define-derived-mode ein:notebooklist-mode special-mode "ein:notebooklist"
  "IPython notebook list mode.
Commands:
\\{ein:notebooklist-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       (lambda (&rest _args) (ein:notebooklist-reload))))


(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
