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
(require 'ein-connect)
(require 'ein-file)
(require 'ein-contents-api)
(require 'ein-subpackages)
(require 'deferred)
(require 'dash)


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
   (lambda () (ein:notebooklist-open-notebook-by-name \"_scratch_\")))

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
              (run-at-time 1 nil
                           #'ein:notebooklist-reload
                           ein:%notebooklist%)
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

(defun ein:notebooklist-list-add (nblist)
  "Register notebook list instance NBLIST for global lookup.
This function adds NBLIST to `ein:notebooklist-map'."
  (puthash (ein:$notebooklist-url-or-port nblist)
           nblist
           ein:notebooklist-map))

(defun ein:notebooklist-list-get (url-or-port)
  "Get an instance of `ein:$notebooklist' by URL-OR-PORT as a key."
  (gethash url-or-port ein:notebooklist-map))

;; TODO: FIXME. Use content API.
(defun ein:notebooklist-open-notebook-by-name (name &optional url-or-port
                                                    callback cbargs)
  "Open notebook named NAME in the server URL-OR-PORT.
If URL-OR-PORT is not given or `nil', and the current buffer is
the notebook list buffer, the notebook is searched in the
notebook list of the current buffer.

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'.
To suppress popup, you can pass `ignore' as CALLBACK."
  (loop with nblist = (if url-or-port
                          (ein:notebooklist-list-get url-or-port)
                        ein:%notebooklist%)
        for note in (ein:$notebooklist-data nblist)
        for notebook-name = (plist-get note :name)
        for notebook-path = (plist-get note :path)
        when (equal notebook-name name)
        return (ein:notebook-open (ein:$notebooklist-url-or-port nblist)
                                  notebook-path nil callback cbargs)))

(defun ein:notebooklist-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((>= version 3) "api/contents"))))
    (if path
        (ein:url url-or-port base-path (or path ""))
      (ein:url url-or-port base-path))))

(defun ein:notebooklist-new-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((>= version 3) "api/contents"))))
    (ein:log 'info "New notebook %s" (concat (file-name-as-directory url-or-port) path))
    (if (and path (not (string= path "")))
        (ein:url url-or-port base-path path)
      (ein:url url-or-port base-path))))

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
      (unless (string-match "^https?:" url-or-port)
        (error "EIN doesn't want to assume what protocol you are using (http or https), so could you please specify the full URL (e.g http://my.jupyter.url:8888?"))
      url-or-port)))

;;;###autoload
(defun ein:notebooklist-open (url-or-port &optional path no-popup resync)
  "Open notebook list buffer."
  (interactive (list (ein:notebooklist-ask-url-or-port)))
  (unless path (setq path ""))
  (if (and (stringp url-or-port) (not (string-match-p "^https?" url-or-port)))
      (setq url-or-port (format "http://%s" url-or-port)))
  (ein:subpackages-load)
  (lexical-let ((url-or-port url-or-port)
                (path path)
                (success (if no-popup
                             #'ein:notebooklist-open--finish
                           (lambda (content)
                             (pop-to-buffer
                              (funcall #'ein:notebooklist-open--finish content))))))
    (if (or resync (not (ein:notebooklist-list-get url-or-port)))
        (deferred:$
          (deferred:parallel
            (lexical-let ((d (deferred:new #'identity)))
              (ein:query-ipython-version url-or-port (lambda ()
                                                       (deferred:callback-post d)))
              d)
            (lexical-let ((d (deferred:new #'identity)))
              (ein:query-kernelspecs url-or-port (lambda ()
                                                   (deferred:callback-post d)))
              d))
          (deferred:nextc it
            (lambda (&rest ignore)
              (ein:content-query-contents url-or-port path success))))
      (ein:content-query-contents url-or-port path success)))
  )

;; point of order (poo): ein:notebooklist-refresh-kernelspecs requeries the kernelspecs and calls ein:notebooklist-reload.  ein:notebooklist-reload already requeries the kernelspecs in one of its callbacks, so this function seems redundant.

;; (defun ein:notebooklist-refresh-kernelspecs (&optional url-or-port)
;;   (interactive (list (or (and ein:%notebooklist% (ein:$notebooklist-url-or-port ein:%notebooklist%))
;;                          (ein:notebooklist-ask-url-or-port))))
;;   (unless url-or-port
;;     (if ein:%notebooklist%
;;         (setq url-or-port (ein:$notebooklist-url-or-port ein:%notebooklist%))
;;       (setq url-or-port (ein:default-url-or-port))))
;;   (ein:query-kernelspecs url-or-port)
;;   (when ein:%notebooklist%
;;     (ein:notebooklist-reload ein:%notebooklist%))
;; )

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
            (run-at-time 0.1 refresh-time #'ein:content-query-contents url-or-port "" success)))))

;;;###autoload
(defun ein:notebooklist-disable-keepalive ()
  "Disable the notebooklist keepalive calls to the jupyter notebook server."
  (interactive)
  (message "Disabling notebooklist keepalive...")
  (cancel-timer ein:notebooklist--keepalive-timer)
  (setq ein:notebooklist--keepalive-timer nil))

(defun ein:notebooklist-open--finish (content)
  "Called via `ein:notebooklist-open'."
  (let ((url-or-port (ein:$content-url-or-port content))
        (path (ein:$content-path content))
        (ipy-version (ein:$content-ipython-version content))
        (data (ein:$content-raw-content content)))
    (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
      (let ((already-opened-p (ein:notebooklist-list-get url-or-port))
            (orig-point (point)))
        (setq ein:%notebooklist%
              (make-ein:$notebooklist :url-or-port url-or-port
                                      :path path
                                      :data data
                                      :api-version ipy-version))
        (ein:notebooklist-list-add ein:%notebooklist%)
        (ein:notebooklist-render ipy-version)
        (goto-char orig-point)
        (ein:log 'verbose "Opened notebooklist at %s" (concat (file-name-as-directory url-or-port) path))
        (unless already-opened-p
          (run-hooks 'ein:notebooklist-first-open-hook))
        (when ein:enable-keepalive
          (ein:notebooklist-enable-keepalive (ein:$content-url-or-port content)))
        (current-buffer)))))

(defun* ein:notebooklist-open-error (url-or-port path
                                     &key error-thrown
                                     &allow-other-keys)
  (ein:log 'error
    "ein:notebooklist-open-error %s: ERROR %s DATA %s" (concat (file-name-as-directory url-or-port) path) (car error-thrown) (cdr error-thrown)))

;;;###autoload
(defun ein:notebooklist-reload (notebooklist &optional resync)
  "Reload current Notebook list."
  (interactive (list ein:%notebooklist%))
  (when notebooklist
    (ein:notebooklist-open (ein:$notebooklist-url-or-port notebooklist)
                           (ein:$notebooklist-path notebooklist) t resync)))

(defun ein:notebooklist-refresh-related ()
  "Reload notebook list in which current notebook locates.
This function is called via `ein:notebook-after-rename-hook'."
  (ein:notebooklist-open (ein:$notebook-url-or-port ein:%notebook%)
                         (ein:$notebook-notebook-path ein:%notebook%) t))

(add-hook 'ein:notebook-after-rename-hook 'ein:notebooklist-refresh-related)

(defun ein:notebooklist-open-notebook (nblist path &optional callback cbargs)
  (ein:notebook-open (ein:$notebooklist-url-or-port nblist)
                     path
                     nil
                     callback
                     cbargs))

(defun ein:notebooklist-open-file (url-or-port path)
  (ein:file-open url-or-port
                 path))

;;;###autoload
(defun ein:notebooklist-upload-file (upload-path)
  (interactive "fSelect file to upload:")
  (unless ein:%notebooklist%
    (error "Only works when called from an ein:notebooklist buffer."))
  (let ((nb-path (ein:$notebooklist-path ein:%notebooklist%)))
    (ein:content-upload nb-path upload-path)))

;;;###autoload
(defun ein:notebooklist-new-notebook (&optional url-or-port kernelspec path callback cbargs)
  "Ask server to create a new notebook and open it in a new buffer."
  (interactive (list (ein:notebooklist-ask-url-or-port)
                     (completing-read
                      "Select kernel [default]: "
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
    (let ((url (ein:notebooklist-new-url url-or-port
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
                               url-or-port path callback cbargs)
       :success (apply-partially #'ein:notebooklist-new-notebook-callback
                                 url-or-port kernelspec path callback cbargs)))))

(defun* ein:notebooklist-new-notebook-callback (url-or-port
                                                kernelspec
                                                path
                                                callback
                                                cbargs
                                                &key
                                                data
                                                &allow-other-keys
                                                &aux
                                                (no-popup t))
  (if data
      (let ((name (plist-get data :name))
            (path (plist-get data :path)))
        (if (= (ein:need-ipython-version url-or-port) 2)
            (if (string= path "")
                (setq path name)
              (setq path (format "%s/%s" path name))))
        (ein:notebook-open url-or-port path kernelspec callback cbargs))
    (ein:log 'info (concat "Oops. EIN failed to open new notebook. "
                           "Please find it in the notebook list."))
    (setq no-popup nil))
  ;; reload or open notebook list
  (ein:notebooklist-open url-or-port path no-popup))

(defun* ein:notebooklist-new-notebook-error
    (url-or-port callback cbargs
                 &key response &allow-other-keys
                 &aux
                 (no-popup t)
                 (error (request-response-error-thrown response))
                 (dest (request-response-url response)))
  (ein:log 'verbose
    "NOTEBOOKLIST-NEW-NOTEBOOK-ERROR url-or-port: %S; error: %S; dest: %S"
    url-or-port error dest)
  (ein:log 'error
    "Failed to open new notebook (error: %S). \
You may find the new one in the notebook list." error)
  (setq no-popup nil)
  (ein:notebooklist-open url-or-port "" no-popup))

;;;###autoload
(defun ein:notebooklist-new-notebook-with-name (name kernelspec url-or-port &optional path)
  "Open new notebook and rename the notebook."
  (interactive (let* ((url-or-port (or (ein:get-url-or-port)
                                       (ein:default-url-or-port)))
                      (kernelspec (completing-read
                                   "Select kernel [default]: "
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
     (lambda (notebook created name)
       (assert created)
       (with-current-buffer (ein:notebook-buffer notebook)
         (ein:notebook-rename-command name)
         ;; As `ein:notebook-open' does not call `pop-to-buffer' when
         ;; callback is specified, `pop-to-buffer' must be called here:
         (pop-to-buffer (current-buffer))))
     (list name))))

(defun ein:notebooklist-delete-notebook-ask (path)
  (when (y-or-n-p (format "Delete notebook %s?" path))
    (ein:notebooklist-delete-notebook path)))

(defun ein:notebooklist-delete-notebook (path)
  (ein:query-singleton-ajax
   (list 'notebooklist-delete-notebook
         (ein:$notebooklist-url-or-port ein:%notebooklist%) path)
   (ein:notebook-url-from-url-and-id
    (ein:$notebooklist-url-or-port ein:%notebooklist%)
    (ein:$notebooklist-api-version ein:%notebooklist%)
    path)
   :type "DELETE"
   :success (apply-partially (lambda (path notebooklist &rest ignore)
                               (ein:log 'info
                                 "Deleted notebook %s" path)
                               (ein:notebooklist-reload notebooklist))
                             path ein:%notebooklist%)))

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
         :notify (lambda (&rest ignore) (ein:notebooklist-open
                                         (ein:$notebooklist-url-or-port ein:%notebooklist%)
                                         path))
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
   :notify (lambda (&rest ignore) (ein:notebooklist-reload ein:%notebooklist% t))
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
     (if (< (ein:$notebooklist-api-version ein:%notebooklist%) 4)
         (format "IPython v%s Notebook list (%s)\n\n" (ein:$notebooklist-api-version ein:%notebooklist%) url-or-port)
       (format "Jupyter v%s Notebook list (%s)\n\n" (ein:$notebooklist-api-version ein:%notebooklist%) url-or-port)))

    (let ((breadcrumbs (generate-breadcrumbs (ein:$notebooklist-path ein:%notebooklist%))))
      (dolist (p breadcrumbs)
        (lexical-let ((url-or-port url-or-port)
                      (name (car p))
                      (path (cdr p)))
          (widget-insert " | ")
          (widget-create
           'link
           :notify (lambda (&rest ignore)
                     (ein:notebooklist-open url-or-port path))
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
       :notify (lambda (&rest ignore) (ein:notebooklist-reload ein:%notebooklist% t))
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
                                 (switch-to-buffer buffer)))
                     "Open")
                    (widget-create
                     'link
                     :notify (lexical-let ((buffer buffer))
                               (lambda (&rest ignore)
                                 (kill-buffer buffer)
                                 (run-at-time 1 nil
                                              #'ein:notebooklist-reload
                                              ein:%notebooklist%)))
                     "Close")
                    (widget-insert " : " (buffer-name buffer))
                    (widget-insert "\n")))))

(defun ein:format-nbitem-data (name last-modified)
  (let ((dt (date-to-time last-modified)))
    (format "%-40s%+20s" name
            (ein:format-time-string ein:notebooklist-date-format dt))))

(defun render-directory (url-or-port sessions)
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
                                 (ein:notebooklist-open url-or-port
                                                        (ein:url (ein:$notebooklist-path ein:%notebooklist%) name))))
                     "Dir")
                    (widget-insert " : " name)
                    (widget-insert "\n"))
          if (and (string= type "file") (> (ein:need-ipython-version url-or-port) 2))
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((url-or-port url-or-port)
                                           (path path))
                               (lambda (&rest ignore)
                                 (ein:notebooklist-open-file url-or-port path)))
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
                     :notify (lexical-let ((name name)
                                           (path path))
                               (lambda (&rest ignore)
                                 (run-at-time 3 nil
                                              #'ein:notebooklist-reload ein:%notebooklist%) ;; TODO using deferred better?
                                 (ein:notebooklist-open-notebook
                                  ein:%notebooklist% path)))
                     "Open")
                    (widget-insert " ")
                    (if (gethash path sessions)
                        (widget-create
                         'link
                         :notify (lexical-let ((session (car (gethash path sessions)))
                                               (nblist ein:%notebooklist%))
                                   (lambda (&rest ignore)
                                     (run-at-time 1 nil
                                                  #'ein:notebooklist-reload
                                                  ein:%notebooklist%)
                                     (ein:kernel-kill (make-ein:$kernel :url-or-port (ein:$notebooklist-url-or-port nblist)
                                                                        :session-id session))))
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

(defun ein:notebooklist-render (ipy-version)
  "Render notebook list widget.
Notebook list data is passed via the buffer local variable
`ein:notebooklist-data'."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let ((url-or-port (ein:$notebooklist-url-or-port ein:%notebooklist%)))
    (ein:content-query-sessions url-or-port
                                (apply-partially #'ein:notebooklist-render--finish ipy-version url-or-port))))

(defun ein:notebooklist-render--finish (ipy-version url-or-port sessions)
  (cl-letf (((symbol-function 'render-header) (if (< ipy-version 3) 
                                                  #'render-header-ipy2 
                                                #'render-header*)))
    (mapc (lambda (x) (funcall (symbol-function x) url-or-port sessions))
          ein:notebooklist-render-order))
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (ein:notebooklist-mode)
    (widget-setup)
    (goto-char (point-min))))

;;;###autoload

(defun ein:notebooklist-list-notebooks ()
  "Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\"."
  (apply #'append
         (loop for nblist in (ein:notebooklist-list)
               for url-or-port = (ein:$notebooklist-url-or-port nblist)
               collect
               (loop for content in (ein:content-need-hierarchy url-or-port)
                     when (string= (ein:$content-type content) "notebook")
                     collect (format "%s/%s" url-or-port
                                     (ein:$content-path content)))
               ;; (if (= api-version 3)
               ;;     (loop for note in (ein:content-need-hierarchy url-or-port)
               ;;           collect (format "%s/%s" url-or-port
               ;;                           (ein:$content-path note)
               ;;                           ))
               ;;   (loop for note in (ein:$notebooklist-data nblist)
               ;;         collect (format "%s/%s"
               ;;                         url-or-port
               ;;                         (plist-get note :name))))
               )))

;;FIXME: Code below assumes notebook is in root directory - need to do a better
;;       job listing notebooks in subdirectories and parsing out the path.
;;;###autoload

(defun ein:notebooklist-open-notebook-global (nbpath &optional callback cbargs)
  "Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'."
  (interactive
   (list (if noninteractive
             (car (ein:notebooklist-list-notebooks)) 
           (completing-read
            "Open notebook [URL-OR-PORT/NAME]: "
            (ein:notebooklist-list-notebooks)))))
  (let* ((parsed (url-generic-parse-url nbpath))
         (path (url-filename parsed)))
    (ein:notebook-open (substring nbpath 0 (- (length nbpath) (length path))) 
                       (substring path 1) nil callback cbargs)))

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
  (ein:notebooklist-open url-or-port "" t))


(defun ein:notebooklist-find-server-by-notebook-name (name)
  "Find a notebook named NAME and return a list (URL-OR-PORT PATH)."
  (loop named outer
        for nblist in (ein:notebooklist-list)
        for url-or-port = (ein:$notebooklist-url-or-port nblist)
        for ipython-version = (ein:$notebooklist-api-version nblist)
        do
        (if (>= ipython-version 3)
            (loop for note in (ein:content-need-hierarchy url-or-port)
                  when (equal (ein:$content-name note) name)
                  do (return-from outer
                       (list url-or-port (ein:$content-path note))))
          (loop for note in (ein:$notebooklist-data nblist)
                when (equal (plist-get note :name) name)
                do (return-from outer
                     (list url-or-port
                           (format "%s/%s" (plist-get note :path) (plist-get note :name))))))))

(defun ein:notebooklist-open-notebook-by-file-name
    (&optional filename noerror buffer-callback)
  "Find the notebook named as same as the current file in the servers.
Open the notebook if found.  Note that this command will *not*
upload the current file to the server.

.. When FILENAME is unspecified the variable `buffer-file-name'
   is used instead.  Set NOERROR to non-`nil' to suppress errors.
   BUFFER-CALLBACK is called after opening notebook with the
   current buffer as the only one argument."
  (interactive (progn (assert buffer-file-name nil "Not visiting a file.")
                      nil))
  (unless filename (setq filename buffer-file-name))
  (assert filename nil "No file found.")
  (let* ((name (file-name-sans-extension
                (file-name-nondirectory (or filename))))
         (found (ein:notebooklist-find-server-by-notebook-name name))
         (callback (lambda (-ignore-1- -ignore-2- buffer buffer-callback)
                     (ein:notebook-pop-to-current-buffer) ; default
                     (when (buffer-live-p buffer)
                       (funcall buffer-callback buffer))))
         (cbargs (list (current-buffer) (or buffer-callback #'ignore))))
    (unless noerror
      (assert found nil "No server has notebook named: %s" name))
    (destructuring-bind (url-or-port path) found
      (ein:notebook-open url-or-port path nil callback cbargs))))

(defvar ein:notebooklist-find-file-buffer-callback #'ignore)

(defun ein:notebooklist-find-file-callback ()
  "A callback function for `find-file-hook' to open notebook.

FIMXE: document how to use `ein:notebooklist-find-file-callback'
       when I am convinced with the API."
  (ein:and-let* ((filename buffer-file-name)
                 ((string-match-p "\\.ipynb$" filename)))
                (ein:notebooklist-open-notebook-by-file-name
                 filename t ein:notebooklist-find-file-buffer-callback)))


;;; Login

;;;###autoload

(defun ein:notebooklist-login (url-or-port password &optional retry-p)
  "Login to IPython notebook server."
  (interactive (list (ein:notebooklist-ask-url-or-port)
                     (read-passwd "Password: ")))
  (ein:query-singleton-ajax
   (list 'notebooklist-login url-or-port)
   (ein:url url-or-port "login")
   :type "POST"
   :data (concat "password=" (url-hexify-string password))
   :parser #'ein:notebooklist-login--parser
   :error (apply-partially #'ein:notebooklist-login--error url-or-port password retry-p)
   :success (apply-partially #'ein:notebooklist-login--success url-or-port)))

(defun ein:notebooklist-login--parser ()
  (goto-char (point-min))
  (list :bad-page (re-search-forward "<input type=.?password" nil t)))

(defun ein:notebooklist-login--success-1 (url-or-port)
  (ein:log 'info "Login to %s complete. \
Now you can open notebook list by `ein:notebooklist-open'." url-or-port))

(defun ein:notebooklist-login--error-1 (url-or-port)
  (ein:log 'info "Failed to login to %s" url-or-port))

(defun* ein:notebooklist-login--success (url-or-port &key
                                                     data
                                                     &allow-other-keys)
  (if (plist-get data :bad-page)
      (ein:notebooklist-login--error-1 url-or-port)
    (ein:notebooklist-login--success-1 url-or-port)))

(defun* ein:notebooklist-login--error
    (url-or-port password retry-p &key
                 data
                 symbol-status
                 response
                 &allow-other-keys
                 &aux
                 (response-status (request-response-status-code response)))
  (if (and (eq response-status 403)
           (not retry-p))
      (ein:notebooklist-login url-or-port password t))
  (if (or
       ;; workaround for url-retrieve backend
       (and (eq symbol-status 'timeout)
            (equal response-status 302)
            (request-response-header response "set-cookie"))
       ;; workaround for curl backend
       (and (equal response-status 405)
            (ein:aand (car (request-response-history response))
                      (request-response-header it "set-cookie"))))
      (ein:notebooklist-login--success-1 url-or-port)
    (ein:notebooklist-login--error-1 url-or-port)))

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
    (ein:notebooklist-open new-url-or-port "" t)
    (loop for x upfrom 0 by 1
          until (or (get-buffer (format ein:notebooklist-buffer-name-template new-url-or-port))
                    (= x 100))
          do (sit-for 0.1))
    (dolist (nb open-nb)
      (ein:notebook-update-url-or-port new-url-or-port nb))
    (kill-buffer (ein:notebooklist-get-buffer old-url))
    (ein:notebooklist-open new-url-or-port "" nil)))

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
          (ein:notebooklist-open new-url-or-port "" t)
          (loop until (get-buffer (format ein:notebooklist-buffer-name-template new-url-or-port))
                do (sit-for 0.1))))
      (deferred:nextc it
        (lambda ()
          (dolist (nb open-nb)
            (ein:notebook-update-url-or-port new-url-or-port nb))))
      (deferred:nextc it
        (lambda ()
          (kill-buffer (ein:notebooklist-get-buffer old-url))
          (ein:notebooklist-open new-url-or-port "" nil))))))

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
  (ein:notebooklist-reload ein:%notebooklist%))

(define-derived-mode ein:notebooklist-mode special-mode "ein:notebooklist"
  "IPython notebook list mode.
Commands:
\\{ein:notebooklist-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       'ein:notebooklist-revert-wrapper))


(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
