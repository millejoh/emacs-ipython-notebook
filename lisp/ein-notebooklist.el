;;; ein-notebooklist.el --- Notebook list buffer

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

(eval-when-compile (require 'cl))
(require 'widget)
(require 'cus-edit)

(require 'ein-core)
(require 'ein-notebook)
(require 'ein-junk)
(require 'ein-connect)
(require 'ein-file)
(require 'ein-contents-api)
(require 'ein-subpackages)
(require 'deferred)
(require 'dash)
(require 'ido)

(defcustom ein:notebooklist-login-timeout (truncate (* 6.3 1000))
  "Timeout in milliseconds for logging into server"
  :group 'ein
  :type 'integer
)

(defcustom ein:notebooklist-render-order
  '(render-header
    render-opened-notebooks
    render-directory)
  "Order of notebook list sections.
Must contain render-header, render-opened-notebooks, and render-directory."
  :group 'ein
  :type 'list
)

(defcustom ein:notebooklist-first-open-hook nil
  "Hooks to run when the notebook list is opened at first time.

Example to open a notebook named _scratch_ when the notebook list
is opened at first time.::

  (add-hook
   'ein:notebooklist-first-open-hook
   (lambda () (ein:notebook-open (ein:$notebooklist-url-or-port ein:%notebooklist%) \"main.ipynb\")))

"
  :type 'hook
  :group 'ein)

(defstruct ein:$notebooklist
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

(ein:deflocal ein:%notebooklist% nil
  "Buffer local variable to store an instance of `ein:$notebooklist'.")

(ein:deflocal ein:%notebooklist-new-kernel% nil
  "Buffer local variable to store kernel type for newly created notebooks.")

(defcustom ein:notebooklist-sort-field :name
  "The notebook list sort field."
  :type '(choice (const :tag "Name" :name)
                 (const :tag "Last modified" :last_modified))
  :group 'ein)
(make-variable-buffer-local 'ein:notebooklist-sort-field)
(put 'ein:notebooklist-sort-field 'permanent-local t)

(defcustom ein:notebooklist-sort-order :ascending
  "The notebook list sort order."
  :type '(choice (const :tag "Ascending" :ascending)
                 (const :tag "Descending" :descending))
  :group 'ein)
(make-variable-buffer-local 'ein:notebooklist-sort-order)
(put 'ein:notebooklist-sort-order 'permanent-local t)

(defmacro ein:make-sorting-widget (tag custom-var)
  "Create the sorting widget."
  ;; assume that custom-var has type `choice' of `const's.
  `(widget-create
    'menu-choice :tag ,tag
    :value ,custom-var
    :notify (lambda (widget &rest ignore)
              (run-at-time 1 nil #'ein:notebooklist-reload)
              (setq ,custom-var (widget-value widget)))
    ,@(mapcar (lambda (const)
                `'(item :tag ,(third const) :value ,(fourth const)))
              (rest (custom-variable-type custom-var)))))

(define-obsolete-variable-alias 'ein:notebooklist 'ein:%notebooklist% "0.1.2")

(defvar ein:notebooklist-buffer-name-template "*ein:notebooklist %s*")

(defvar ein:notebooklist-map (make-hash-table :test 'equal)
  "Data store for `ein:notebooklist-list'.
Mapping from URL-OR-PORT to an instance of `ein:$notebooklist'.")

(defun ein:notebooklist-list ()
  "Get a list of opened `ein:$notebooklist'."
  (ein:hash-vals ein:notebooklist-map))

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

(defun ein:notebooklist-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((>= version 3) "api/contents"))))
    (ein:url url-or-port base-path path)))

(defun ein:notebooklist-proc--sentinel (url-or-port process event)
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

(defun ein:crib-token--all-local-tokens ()
  "Generate a hash table of authorization tokens (when they
exist) for allow local jupyter instances, keyed by they url and
port the instance is running on."
  (let ((lines
         (condition-case err
             ;; there may be NO local jupyter installation
             (process-lines ein:jupyter-default-server-command
                            "notebook" "list" "--json")
           (error (message "Error getting local tokens: %s" err)
                  ())))         ; empty list
        (url-tokens (make-hash-table :test #'equal)))
    (loop for line in lines
          do (destructuring-bind
                 (&key password url token &allow-other-keys)
                 (ein:json-read-from-string line)
               (push (list password token) (gethash (ein:url url) url-tokens))))
    url-tokens))

(defun ein:crib-token (url-or-port)
  (let ((pw-pairs (gethash url-or-port (ein:crib-token--all-local-tokens))))
    ;; pw-pairs is of the form ((PASSWORD-P TOKEN) (PASSWORD-P TOKEN))
    (cond ((= (length pw-pairs) 1) (car pw-pairs))
          ((> (length pw-pairs) 1)
           ;; orig code: (list :json-false token) meant "no password, yes token"
           ;; It's not clear how two entries for the same url could happen but if it did,
           ;; 1. what if both entries don't have any auth enabled?
           ;; 2. what if an entry required a password and not a token?
           ;; It's best to return "nil nil" in this unlikely (impossible?) event, and let
           ;; the downstream logic handle it.
           (warn "I see multiple jupyter servers registered on the same url! Please enter the token for one that is actually running.")
           (list nil nil))
          (t (list nil nil)))))

(defun ein:notebooklist-token-or-password (url-or-port)
  "Return token or password (jupyter requires one or the other but not both) for URL-OR-PORT.  Empty string token means all authentication disabled.  Nil means don't know."
  (multiple-value-bind (password-p token) (ein:crib-token url-or-port)
    (autoload 'ein:jupyter-server-conn-info "ein-jupyter")
    (multiple-value-bind (my-url-or-port my-token) (ein:jupyter-server-conn-info)
      (cond ((eq password-p t) (read-passwd (format "Password for %s: " url-or-port)))
            ((and (stringp token) (eql password-p :json-false)) token)
            ((equal url-or-port my-url-or-port) my-token)
            (t nil)))))

(defun ein:notebooklist-ask-url-or-port ()
  (let* ((default (ein:url (ein:aif (ein:get-notebook)
                               (ein:$notebook-url-or-port it)
                             (ein:aif ein:%notebooklist%
                                 (ein:$notebooklist-url-or-port it)
                               (ein:default-url-or-port)))))
         (url-or-port-list
          (-distinct (mapcar #'ein:url
                             (append (list default)
                                     ein:url-or-port
                                     (hash-table-keys
                                      (ein:crib-token--all-local-tokens))))))
         (url-or-port (let ((ido-report-no-match nil)
                            (ido-use-faces nil))
                        (ido-completing-read "URL or port: "
                                             url-or-port-list
                                             nil nil nil nil
                                             default))))
    (ein:url url-or-port)))

(defun ein:notebooklist-open* (url-or-port &optional path resync callback errback)
  "The main entry to server at URL-OR-PORT.  Users should not directly call this, but instead `ein:notebooklist-login'.

PATH is specifying directory from file navigation.  PATH is empty on login.  RESYNC is requery server attributes such as ipython version and kernelspecs.  CALLBACK takes one argument, the resulting buffer.  ERRBACK takes one argument, the resulting buffer.
"
  (unless path (setq path ""))
  (setq url-or-port (ein:url url-or-port)) ;; should work towards not needing this
  (ein:subpackages-load)
  (lexical-let* ((url-or-port url-or-port)
                 (path path)
                 (success (apply-partially #'ein:notebooklist-open--finish url-or-port callback))
                 (failure errback))
    (if (or resync (not (ein:notebooklist-list-get url-or-port)))
        (deferred:$
          (deferred:parallel
            (lexical-let ((d (deferred:new #'identity)))
              (ein:query-notebook-version url-or-port (lambda ()
                                                       (deferred:callback-post d)))
              d)
            (lexical-let ((d (deferred:new #'identity)))
              (ein:query-kernelspecs url-or-port (lambda ()
                                                   (deferred:callback-post d)))
              d))
          (deferred:nextc it
            (lambda (&rest ignore)
              (lexical-let ((d (deferred:new #'identity)))
                (ein:content-query-hierarchy url-or-port (lambda (tree)
                                                           (deferred:callback-post d)))
                d)))
          (deferred:nextc it
            (lambda (&rest ignore)
              (ein:content-query-contents url-or-port path success failure))))
      (ein:content-query-contents url-or-port path success failure))))

(defcustom ein:notebooklist-keepalive-refresh-time 1
  "When the notebook keepalive is enabled, the frequency, IN
HOURS, with which to make calls to the jupyter content API to
refresh the notebook connection."
  :type 'float
  :group 'ein)

(defcustom ein:enable-keepalive nil
  "When non-nil, will cause EIN to automatically call
  `ein:notebooklist-enable-keepalive' after any call to
  `ein:notebooklist-open'."
  :type 'boolean
  :group 'ein)

(defcustom ein:notebooklist-date-format "%x"
  "The format spec for date in notebooklist mode.
See `ein:format-time-string'."
  :type '(or string function)
  :group 'ein)

(defvar ein:notebooklist--keepalive-timer nil)

;;;###autoload
(defun ein:notebooklist-enable-keepalive (&optional url-or-port)
  "Enable periodic calls to the notebook server to keep long running sessions from expiring.
By long running we mean sessions to last days, or weeks. The
frequency of the refresh (which is very similar to a call to
`ein:notebooklist-open`) is controlled by
`ein:notebooklist-keepalive-refresh-time`, and is measured in
terms of hours. If `ein:enable-keepalive' is non-nil this will
automatically be called during calls to `ein:notebooklist-open`."
  (interactive (list (ein:notebooklist-ask-url-or-port)))
  (unless ein:notebooklist--keepalive-timer
    (message "Enabling notebooklist keepalive...")
    (let ((success
           (lambda (content)
             (ein:log 'info "Refreshing notebooklist connection.")))
          (refresh-time (* ein:notebooklist-keepalive-refresh-time 60 60)))
      (setq ein:notebooklist--keepalive-timer
            (run-at-time 0.1 refresh-time #'ein:content-query-contents url-or-port "" success nil)))))

;;;###autoload
(defun ein:notebooklist-disable-keepalive ()
  "Disable the notebooklist keepalive calls to the jupyter notebook server."
  (interactive)
  (message "Disabling notebooklist keepalive...")
  (cancel-timer ein:notebooklist--keepalive-timer)
  (setq ein:notebooklist--keepalive-timer nil))

(defun ein:notebooklist-open--finish (url-or-port callback content)
  "Called via `ein:notebooklist-open'."
  (let ((path (ein:$content-path content))
        (nb-version (ein:$content-notebook-version content))
        (data (ein:$content-raw-content content)))
    (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
      (let ((already-opened-p (ein:notebooklist-list-get url-or-port))
            (orig-point (point)))
        (setq ein:%notebooklist%
              (make-ein:$notebooklist :url-or-port url-or-port
                                      :path path
                                      :data data
                                      :api-version nb-version))
        (ein:notebooklist-list-add ein:%notebooklist%)
        (ein:notebooklist-render nb-version)
        (goto-char orig-point)
        (ein:log 'verbose "Opened notebooklist at %s" (ein:url url-or-port path))
        (unless already-opened-p
          (run-hooks 'ein:notebooklist-first-open-hook))
        (when ein:enable-keepalive
          (ein:notebooklist-enable-keepalive url-or-port))
        (when callback
          (funcall callback (current-buffer)))
        (current-buffer)))))

(defun* ein:notebooklist-open-error (url-or-port path
                                     &key error-thrown
                                     &allow-other-keys)
  (ein:log 'error
    "ein:notebooklist-open-error %s: ERROR %s DATA %s" (concat (file-name-as-directory url-or-port) path) (car error-thrown) (cdr error-thrown)))

;;;###autoload
(defun ein:notebooklist-reload (&optional nblist resync)
  "Reload current Notebook list."
  (interactive)
  (unless nblist
    (setq nblist ein:%notebooklist%))
  (when nblist
    (ein:notebooklist-open* (ein:$notebooklist-url-or-port nblist)
                            (ein:$notebooklist-path nblist) resync)))

(defun ein:notebooklist-refresh-related ()
  "Reload notebook list in which current notebook locates.
This function is called via `ein:notebook-after-rename-hook'."
  (ein:notebooklist-open* (ein:$notebook-url-or-port ein:%notebook%)
                          (ein:$notebook-notebook-path ein:%notebook%)))

(add-hook 'ein:notebook-after-rename-hook 'ein:notebooklist-refresh-related)

;;;###autoload
(defun ein:notebooklist-upload-file (upload-path)
  (interactive "fSelect file to upload:")
  (unless ein:%notebooklist%
    (error "Only works when called from an ein:notebooklist buffer."))
  (let ((nb-path (ein:$notebooklist-path ein:%notebooklist%)))
    (ein:content-upload nb-path upload-path)))

;;;###autoload
(defun ein:notebooklist-new-notebook (&optional url-or-port kernelspec path callback)
  "Ask server to create a new notebook and open it in a new buffer.

TODO - New and open should be separate, and we should flag an exception if we try to new an existing.
"

  (interactive (list (ein:notebooklist-ask-url-or-port)
                     (ido-completing-read
                      "Select kernel: "
                      (ein:list-available-kernels (ein:$notebooklist-url-or-port ein:%notebooklist%)) nil t nil nil "default" nil)))
  (let ((path (or path (ein:$notebooklist-path (or ein:%notebooklist%
                                                   (ein:notebooklist-list-get url-or-port)))))
        (version (ein:$notebooklist-api-version (or ein:%notebooklist%
                                                    (ein:notebooklist-list-get url-or-port)))))
    (unless url-or-port
      (setq url-or-port (ein:$notebooklist-url-or-port ein:%notebooklist%)))
    (assert url-or-port nil
            (concat "URL-OR-PORT is not given and the current buffer "
                    "is not the notebook list buffer."))
    (let ((url (ein:notebooklist-url url-or-port
                                     version
                                     path)))
      (ein:query-singleton-ajax
       (list 'notebooklist-new-notebook url-or-port path)
       url
       :type "POST"
       :data (json-encode '((:type . "notebook")))
       :parser #'ein:json-read
       ;; (lambda ()
       ;;   (ein:html-get-data-in-body-tag "data-notebook-id"))
       :error (apply-partially #'ein:notebooklist-new-notebook-error
                               url-or-port path callback)
       :success (apply-partially #'ein:notebooklist-new-notebook-callback
                                 url-or-port kernelspec path callback)))))

(defun* ein:notebooklist-new-notebook-callback (url-or-port
                                                kernelspec
                                                path
                                                callback
                                                &key
                                                data
                                                &allow-other-keys)
  (let ((nbname (plist-get data :name))
        (nbpath (plist-get data :path)))
    (when (= (ein:need-notebook-version url-or-port) 2)
      (if (string= nbpath "")
          (setq nbpath nbname)
        (setq nbpath (format "%s/%s" nbpath nbname))))
    (ein:notebook-open url-or-port nbpath kernelspec callback)
    (ein:notebooklist-open* url-or-port path)))

(defun* ein:notebooklist-new-notebook-error
    (url-or-port callback
                 &key response &allow-other-keys
                 &aux
                 (error (request-response-error-thrown response))
                 (dest (request-response-url response)))
  (ein:log 'verbose
    "NOTEBOOKLIST-NEW-NOTEBOOK-ERROR url-or-port: %S; error: %S; dest: %S"
    url-or-port error dest)
  (ein:log 'error
    "Failed to open new notebook (error: %S). \
You may find the new one in the notebook list." error)
  (ein:notebooklist-open* url-or-port nil nil #'pop-to-buffer))

;;;###autoload
(defun ein:notebooklist-new-notebook-with-name (name kernelspec url-or-port &optional path)
  "Open new notebook and rename the notebook."
  (interactive (let* ((url-or-port (or (ein:get-url-or-port)
                                       (ein:default-url-or-port)))
                      (kernelspec (ido-completing-read
                                   "Select kernel: "
                                   (ein:list-available-kernels url-or-port) nil t nil nil "default" nil))
                      (name (read-from-minibuffer
                             (format "Notebook name (at %s): " url-or-port))))
                 (list name kernelspec url-or-port)))
  (let ((path (or path (ein:$notebooklist-path
                        (or ein:%notebooklist%
                            (ein:get-notebook)
                            (gethash url-or-port ein:notebooklist-map))))))
    (ein:notebooklist-new-notebook
     url-or-port
     kernelspec
     path
     (apply-partially
      (lambda (name* notebook created)
        (assert created)
        (with-current-buffer (ein:notebook-buffer notebook)
          (ein:notebook-rename-command name*)
          ;; As `ein:notebook-open' does not call `pop-to-buffer' when
          ;; callback is specified, `pop-to-buffer' must be called here:
          (pop-to-buffer (current-buffer))))
      name))))

(defun ein:notebooklist-delete-notebook-ask (path)
  (when (y-or-n-p (format "Delete notebook %s?" path))
    (ein:notebooklist-delete-notebook path)))

(defun ein:notebooklist-delete-notebook (path &optional callback)
  "CALLBACK with no arguments, e.g., semaphore"
  (lexical-let* ((path path)
                 (notebooklist ein:%notebooklist%)
                 (callback callback)
                 (url-or-port (ein:$notebooklist-url-or-port notebooklist)))
    (unless callback (setq callback (lambda () (ein:notebooklist-reload notebooklist))))
    (ein:query-singleton-ajax
     (list 'notebooklist-delete-notebook (ein:url url-or-port path))
     (ein:notebook-url-from-url-and-id
      url-or-port (ein:$notebooklist-api-version notebooklist) path)
     :type "DELETE"
     :complete (apply-partially #'ein:notebooklist-delete-notebook--complete (ein:url url-or-port path) callback))))

(defun* ein:notebooklist-delete-notebook--complete (url callback
                                             &key data response symbol-status
                                             &allow-other-keys
                                             &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:notebooklist-delete-notebook--complete %s" resp-string)
  (when (and callback (eq symbol-status 'success)) (funcall callback)))

;; Because MinRK wants me to suffer (not really, I love MinRK)...
(defun ein:get-actual-path (path)
  (ein:aif (cl-position ?/ path :from-end t)
      (substring path 0 it)
    ""))

(defun generate-breadcrumbs (path)
  "Given notebooklist path, generate alist of breadcrumps of form (name . path)."
  (let* ((paths (split-string path "/" t))
         (current-path "/")
         (pairs (list (cons "Home" ""))))
    (dolist (p paths pairs)
      (setf current-path (concat current-path "/" p)
            pairs (append pairs (list (cons p current-path)))))))

(defun* ein:nblist--sort-group (group by-param order)
  (sort group #'(lambda (x y)
                  (cond ((eql order :ascending)
                         (string-lessp (plist-get x by-param)
                                       (plist-get y by-param)))
                        ((eql order :descending)
                         (string-greaterp (plist-get x by-param)
                                          (plist-get y by-param)))))))

(defun ein:notebooklist--order-data (nblist-data sort-param sort-order)
  "Try to sanely sort the notebooklist data for the current path."
  (let* ((groups (-group-by #'(lambda (x) (plist-get x :type))
                            nblist-data))
         (dirs (ein:nblist--sort-group (cdr (assoc "directory" groups))
                                       sort-param
                                       sort-order))
         (nbs (ein:nblist--sort-group (cdr (assoc "notebook" groups))
                                      sort-param
                                      sort-order))
         (files (ein:nblist--sort-group (-flatten-n 1 (-map #'cdr (-group-by
                                             #'(lambda (x) (car (last (s-split "\\." (plist-get x :name)))))
                                             (cdr (assoc "file" groups)))))
                                        sort-param
                                        sort-order)))
    (-concat dirs nbs files)))

(defun render-header-ipy2 (&rest args)
  "Render the header (for ipython2)."
  ;; Create notebook list
  (widget-insert (format "IPython %s Notebook list\n\n" (ein:$notebooklist-api-version ein:%notebooklist%)))

  (let ((breadcrumbs (generate-breadcrumbs (ein:$notebooklist-path ein:%notebooklist%))))
    (dolist (p breadcrumbs)
      (lexical-let ((name (car p))
                    (path (cdr p)))
        (widget-insert " | ")
        (widget-create
         'link
         :notify (lambda (&rest ignore)
                   (ein:notebooklist-login
                    (ein:$notebooklist-url-or-port ein:%notebooklist%) path))
         name)))
    (widget-insert " |\n\n"))

  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein:notebooklist-new-notebook
                                   (ein:$notebooklist-url-or-port ein:%notebooklist%)))
   "New Notebook")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein:notebooklist-reload nil t))
   "Reload List")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore)
             (browse-url
              (ein:url (ein:$notebooklist-url-or-port ein:%notebooklist%))))
   "Open In Browser")
  (widget-insert "\n"))

(defun render-header* (url-or-port &rest args)
  "Render the header (for ipython>=3)."
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (widget-insert
     (format "Notebook v%s (%s)\n\n" (ein:$notebooklist-api-version ein:%notebooklist%) url-or-port))

    (let ((breadcrumbs (generate-breadcrumbs (ein:$notebooklist-path ein:%notebooklist%))))
      (dolist (p breadcrumbs)
        (lexical-let ((url-or-port url-or-port)
                      (name (car p))
                      (path (cdr p)))
          (widget-insert " | ")
          (widget-create
           'link
           :notify (lambda (&rest ignore)
                     (ein:notebooklist-open* url-or-port path nil #'pop-to-buffer))
           name)))
      (widget-insert " |\n\n"))

    (lexical-let* ((url-or-port url-or-port)
                   (kernels (ein:list-available-kernels url-or-port)))
      (if (null ein:%notebooklist-new-kernel%)
          (setq ein:%notebooklist-new-kernel% (ein:get-kernelspec url-or-port (caar kernels))))
      (widget-create
       'link
       :notify (lambda (&rest ignore) (ein:notebooklist-new-notebook
                                       url-or-port
                                       ein:%notebooklist-new-kernel%))
       "New Notebook")
      (widget-insert " ")
      (widget-create
       'link
       :notify (lambda (&rest ignore) (ein:notebooklist-reload nil t))
       "Resync")
      (widget-insert " ")
      (widget-create
       'link
       :notify (lambda (&rest ignore)
                 (browse-url (ein:url url-or-port)))
       "Open In Browser")

      (widget-insert "\n\nCreate New Notebooks Using Kernel:\n")
      (let* ((radio-widget (widget-create 'radio-button-choice
                                          :value (and ein:%notebooklist-new-kernel% (ein:$kernelspec-name ein:%notebooklist-new-kernel%))
                                          :notify (lambda (widget &rest ignore)
                                                    (setq ein:%notebooklist-new-kernel%
                                                          (ein:get-kernelspec url-or-port (widget-value widget)))
                                                    (message "New notebooks will be started using the %s kernel."
                                                             (ein:$kernelspec-display-name ein:%notebooklist-new-kernel%))))))
        (if (null kernels)
            (widget-insert "\n  No kernels found.")
          (dolist (k kernels)
            (widget-radio-add-item radio-widget (list 'item :value (car k)
                                                      :format (format "%s\n" (cdr k)))))
          (widget-insert "\n"))))))

(defun render-opened-notebooks (url-or-port &rest args)
  "Render the opened notebooks section (for ipython>=3)."
  ;; Opened Notebooks Section
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (widget-insert "\n---------- All Opened Notebooks ----------\n\n")
    (loop for buffer in (ein:notebook-opened-buffers)
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((buffer buffer))
                               (lambda (&rest ignore)
                                 (condition-case err
                                     (switch-to-buffer buffer)
                                   (error
                                    (message "%S" err)
                                    (ein:notebooklist-reload)))))
                     "Open")
                    (widget-create
                     'link
                     :notify (lexical-let ((buffer buffer))
                               (lambda (&rest ignore)
                                 (if (buffer-live-p buffer)
                                     (kill-buffer buffer))
                                 (run-at-time 1 nil #'ein:notebooklist-reload)))
                     "Close")
                    (widget-insert " : " (buffer-name buffer))
                    (widget-insert "\n")))))

(defun ein:format-nbitem-data (name last-modified)
  (let ((dt (date-to-time last-modified)))
    (format "%-40s%+20s" name
            (ein:format-time-string ein:notebooklist-date-format dt))))

(defun render-directory (url-or-port sessions)
  ;; SESSIONS is a hashtable of path to (session-id . kernel-id) pairs
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (widget-insert "\n------------------------------------------\n\n")
    (ein:make-sorting-widget "Sort by" ein:notebooklist-sort-field)
    (ein:make-sorting-widget "In Order" ein:notebooklist-sort-order)
    (widget-insert "\n")
    (loop for note in (ein:notebooklist--order-data (ein:$notebooklist-data ein:%notebooklist%)
                                                    ein:notebooklist-sort-field
                                                    ein:notebooklist-sort-order)
          for name = (plist-get note :name)
          for path = (plist-get note :path)
          for last-modified = (plist-get note :last_modified)
          ;; (cond ((= 2 api-version)
          ;;        (plist-get note :path))
          ;;       ((= 3 api-version)
          ;;        (ein:get-actual-path (plist-get note :path))))
          for type = (plist-get note :type)
          for opened-notebook-maybe = (ein:notebook-get-opened-notebook url-or-port path)
          do (widget-insert " ")
          if (string= type "directory")
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((url-or-port url-or-port)
                                           (name name))
                               (lambda (&rest ignore)
                                 ;; each directory creates a whole new notebooklist
                                 (ein:notebooklist-open* url-or-port
                                                        (concat (file-name-as-directory (ein:$notebooklist-path ein:%notebooklist%)) name) nil #'pop-to-buffer)))
                     "Dir")
                    (widget-insert " : " name)
                    (widget-insert "\n"))
          if (and (string= type "file") (> (ein:need-notebook-version url-or-port) 2))
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((url-or-port url-or-port)
                                           (path path))
                               (lambda (&rest ignore)
                                 (ein:file-open url-or-port path)))
                     "Open")
                    (widget-insert " ------ ")
                    (widget-create
                     'link
                     :notify (lexical-let ((path path))
                               (lambda (&rest ignore)
                                 (message "[EIN]: NBlist delete file command. Implement me!")))
                     "Delete")
                    (widget-insert " : " (ein:format-nbitem-data name last-modified))
                    (widget-insert "\n"))
          if (string= type "notebook")
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((url-or-port url-or-port)
                                           (path path))
                               (lambda (&rest ignore)
                                 (run-at-time 3 nil #'ein:notebooklist-reload) ;; TODO using deferred better?
                                 (ein:notebook-open url-or-port path)))

                     "Open")
                    (widget-insert " ")
                    (if (gethash path sessions)
                        (widget-create
                         'link
                         :notify (lexical-let ((url url-or-port)
                                               (session (car (gethash path sessions))))
                                   (lambda (&rest ignore)
                                     (ein:kernel-delete--from-session-id url session #'ein:notebooklist-reload)))
                         "Stop")
                      (widget-insert "------"))
                    (widget-insert " ")
                    (widget-create
                     'link
                     :notify (lexical-let ((path path))
                               (lambda (&rest ignore)
                                 (ein:notebooklist-delete-notebook-ask
                                  path)))
                     "Delete")
                    (widget-insert " : " (ein:format-nbitem-data name last-modified))
                    (widget-insert "\n")))))

(defun ein:notebooklist-render (nb-version)
  "Render notebook list widget.
Notebook list data is passed via the buffer local variable
`ein:notebooklist-data'."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let ((url-or-port (ein:$notebooklist-url-or-port ein:%notebooklist%)))
    (ein:content-query-sessions url-or-port
                                (apply-partially #'ein:notebooklist-render--finish nb-version url-or-port)
                                nil)))

(defun ein:notebooklist-render--finish (nb-version url-or-port sessions)
  (cl-letf (((symbol-function 'render-header) (if (< nb-version 3)
                                                  #'render-header-ipy2
                                                #'render-header*)))
    (mapc (lambda (x) (funcall (symbol-function x) url-or-port sessions))
          ein:notebooklist-render-order))
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (ein:notebooklist-mode)
    (widget-setup)
    (goto-char (point-min))))

;;;###autoload

(defun ein:notebooklist-list-paths (&optional content-type)
  "Return all files of CONTENT-TYPE for all sessions"
  (apply #'append
         (loop for nblist in (ein:notebooklist-list)
               for url-or-port = (ein:$notebooklist-url-or-port nblist)
               collect
               (loop for content in (ein:content-need-hierarchy url-or-port)
                     when (or (null content-type) (string= (ein:$content-type content) content-type))
                     collect (ein:url url-or-port (ein:$content-path content))))))


(defsubst ein:notebooklist-parse-nbpath (nbpath)
  "Return `(,url-or-port ,path) from URL-OR-PORT/PATH"
  (let* ((parsed (url-generic-parse-url nbpath))
         (path (url-filename parsed)))
    (list (substring nbpath 0 (- (length nbpath) (length path)))
          (substring path 1))))

(defsubst ein:notebooklist-ask-path (&optional content-type)
  (ido-completing-read (format  "Open %s: " content-type)
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
  (run-with-idle-timer 3 nil #'ein:notebooklist-load)

You should setup `ein:url-or-port' or `ein:default-url-or-port'
in order to make this code work.

See also:
`ein:connect-to-default-notebook', `ein:connect-default-notebook'."
  (ein:notebooklist-open* url-or-port nil))

;;; Login

(defun ein:notebooklist-login--iteration (url-or-port callback errback token iteration response-status)
  "Called from `ein:notebooklist-login'."
  (ein:log 'debug "Login attempt #%d in response to %s from %s."
           iteration response-status url-or-port)
  (unless callback
    (setq callback #'ignore))
  (unless errback
    (setq errback #'ignore))
  (lexical-let (done-p)
    (add-function :after callback (lambda (&rest ignore) (setq done-p t)))
    (add-function :after errback (lambda (&rest ignore) (setq done-p t)))
    (ein:query-singleton-ajax
     (list 'notebooklist-login--iteration url-or-port)
     (ein:url url-or-port "login")
     ;; do not use :type "POST" here (see git history)
     :timeout ein:notebooklist-login-timeout
     :data (if token (concat "password=" (url-hexify-string token)))
     :parser #'ein:notebooklist-login--parser
     :complete (apply-partially #'ein:notebooklist-login--complete url-or-port)
     :error (apply-partially #'ein:notebooklist-login--error url-or-port token callback errback iteration)
     :success (apply-partially #'ein:notebooklist-login--success url-or-port callback errback token iteration))
    (unless noninteractive
      (with-local-quit
        (loop until done-p
              do (sleep-for 0 450))))))

;;;###autoload
(defun ein:notebooklist-open (url-or-port callback)
  "This is now an alias for ein:notebooklist-login"
  (interactive `(,(ein:notebooklist-ask-url-or-port) ,#'pop-to-buffer))
  (ein:notebooklist-login url-or-port callback))

(make-obsolete 'ein:notebooklist-open 'ein:notebooklist-login "0.14.2")

;;;###autoload
(defalias 'ein:login 'ein:notebooklist-login)

(defun ein:notebooklist-ask-one-cookie ()
  "If we need more than one cookie, we first need to ask for how many.  Returns list of name and content."
  (plist-put nil (intern (read-no-blanks-input "Cookie name: "))
             (read-no-blanks-input "Cookie content: ")))

;;;###autoload
(defun ein:notebooklist-login (url-or-port callback &optional cookie-plist)
  "Deal with security before main entry of ein:notebooklist-open*.

CALLBACK takes one argument, the buffer created by ein:notebooklist-open--success."
  (interactive `(,(ein:notebooklist-ask-url-or-port)
                 ,#'pop-to-buffer
                 ,(if current-prefix-arg (ein:notebooklist-ask-one-cookie))))
  (unless callback (setq callback (lambda (buffer))))

  (when cookie-plist
    (let* ((parsed-url (url-generic-parse-url (file-name-as-directory url-or-port)))
           (domain (url-host parsed-url))
           (securep (string-match "^wss://" url-or-port)))
      (loop for (name content) on cookie-plist by (function cddr)
            for line = (mapconcat #'identity (list domain "FALSE" (car (url-path-and-query parsed-url)) (if securep "TRUE" "FALSE") "0" (symbol-name name) (concat content "\n")) "\t")
            do (write-region line nil (request--curl-cookie-jar) 'append))))

  (ein:message-whir "Establishing session"
    (let ((token (ein:notebooklist-token-or-password url-or-port)))
      (add-function :before callback done-callback)
      (cond ((null token) ;; don't know
             (ein:notebooklist-login--iteration url-or-port callback errback nil -1 nil))
            ((string= token "") ;; all authentication disabled
             (ein:log 'verbose "Skipping login %s" url-or-port)
             (ein:notebooklist-open* url-or-port nil nil callback errback))
            (t (ein:notebooklist-login--iteration url-or-port callback errback token 0 nil))))))

(defun ein:notebooklist-login--parser ()
  (goto-char (point-min))
  (list :bad-page (re-search-forward "<input type=.?password" nil t)))

(defun ein:notebooklist-login--success-1 (url-or-port callback errback)
  (ein:log 'info "Login to %s complete." url-or-port)
  (ein:notebooklist-open* url-or-port nil nil callback errback))

(defun ein:notebooklist-login--error-1 (url-or-port errback)
  (ein:log 'error "Login to %s failed" url-or-port)
  (funcall errback))

(defun* ein:notebooklist-login--complete (url-or-port &key data response
                                                      &allow-other-keys
                                                      &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:notebooklist-login--complete %s" resp-string))

(defun* ein:notebooklist-login--success (url-or-port callback errback token iteration
                                                     &key data response
                                                     &allow-other-keys
                                                     &aux
                                                     (response-status (request-response-status-code response)))
  (if (plist-get data :bad-page)
      (if (>= iteration 0)
          (ein:notebooklist-login--error-1 url-or-port errback)
        (setq token (read-passwd (format "Password for %s: " url-or-port)))
        (ein:notebooklist-login--iteration url-or-port callback errback token (1+ iteration) response-status))
    (ein:notebooklist-login--success-1 url-or-port callback errback)))

(defun* ein:notebooklist-login--error
    (url-or-port token callback errback iteration &key
                 data
                 symbol-status
                 response
                 &allow-other-keys
                 &aux
                 (response-status (request-response-status-code response)))
  (cond ((and response-status (< iteration 0))
         (setq token (read-passwd (format "Password for %s: " url-or-port)))
         (ein:notebooklist-login--iteration url-or-port callback errback token (1+ iteration) response-status))
        ((and (eq response-status 403) (< iteration 1))
         (ein:notebooklist-login--iteration url-or-port callback errback token (1+ iteration) response-status))
        ((and (eq symbol-status 'timeout) ;; workaround for url-retrieve backend
              (eq response-status 302)
              (request-response-header response "set-cookie"))
         (ein:notebooklist-login--success-1 url-or-port callback errback))
        (t (ein:notebooklist-login--error-1 url-or-port errback))))

;;;###autoload

(defun ein:notebooklist-change-url-port (new-url-or-port)
  "Update the ipython/jupyter notebook server URL for all the
notebooks currently opened from the current notebooklist buffer.

This function works by calling `ein:notebook-update-url-or-port'
on all the notebooks opened from the current notebooklist."
  (interactive (list (ein:notebooklist-ask-url-or-port)))
  (unless (eql major-mode 'ein:notebooklist-mode)
    (error "This command needs to be called from within a notebooklist buffer."))
  (let* ((current-nblist ein:%notebooklist%)
         (old-url (ein:$notebooklist-url-or-port current-nblist))
         (new-url-or-port new-url-or-port)
         (open-nb (ein:notebook-opened-notebooks #'(lambda (nb)
                                                     (equal (ein:$notebook-url-or-port nb)
                                                            (ein:$notebooklist-url-or-port current-nblist))))))
    (ein:notebooklist-open* new-url-or-port nil)
    (loop for x upfrom 0 by 1
          until (or (get-buffer (format ein:notebooklist-buffer-name-template new-url-or-port))
                    (= x 100))
          do (sit-for 0.1))
    (dolist (nb open-nb)
      (ein:notebook-update-url-or-port new-url-or-port nb))
    (kill-buffer (ein:notebooklist-get-buffer old-url))
    (ein:notebooklist-open* new-url-or-port nil nil #'pop-to-buffer)))

(defun ein:notebooklist-change-url-port--deferred (new-url-or-port)
  (lexical-let* ((current-nblist ein:%notebooklist%)
                 (old-url (ein:$notebooklist-url-or-port current-nblist))
                 (new-url-or-port new-url-or-port)
                 (open-nb (ein:notebook-opened-notebooks
                           (lambda (nb)
                             (equal (ein:$notebook-url-or-port nb)
                                    (ein:$notebooklist-url-or-port current-nblist))))))
    (deferred:$
      (deferred:next
        (lambda ()
          (ein:notebooklist-open* new-url-or-port nil)
          (loop until (get-buffer (format ein:notebooklist-buffer-name-template new-url-or-port))
                do (sit-for 0.1))))
      (deferred:nextc it
        (lambda ()
          (dolist (nb open-nb)
            (ein:notebook-update-url-or-port new-url-or-port nb))))
      (deferred:nextc it
        (lambda ()
          (kill-buffer (ein:notebooklist-get-buffer old-url))
          (ein:notebooklist-open* new-url-or-port nil nil #'pop-to-buffer))))))

;;; Generic getter



(defun ein:get-url-or-port--notebooklist ()
  (when (ein:$notebooklist-p ein:%notebooklist%)
    (ein:$notebooklist-url-or-port ein:%notebooklist%)))


;;; Notebook list mode


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
          ein:notebooklist-new-notebook-with-name)
         ("New Junk Notebook" ein:junk-new)))))

(defun ein:notebooklist-revert-wrapper (&optional ignore-auto noconfirm preserve-modes)
  (ein:notebooklist-reload))

(define-derived-mode ein:notebooklist-mode special-mode "ein:notebooklist"
  "IPython notebook list mode.
Commands:
\\{ein:notebooklist-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       'ein:notebooklist-revert-wrapper))


(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
