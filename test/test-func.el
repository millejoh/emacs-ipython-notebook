(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebooklist)
(require 'ein-jupyter)
(require 'wid-edit)
(require 'ein-testing)
(require 'ein-testing-cell)

(let ((backend (getenv "EL_REQUEST_BACKEND")))
  (when (and backend (not (equal backend "")))
    (setq request-backend (intern backend))
    (message "Using request-backend = %S" request-backend)))

(defvar *ein:testing-notebook-name* nil
  "This is the name of the notebook the server creates for this test.  It could be Untitled.ipynb or if that was already there, Untitled1.ipynb, etc.")

(setq message-log-max t)

(defun ein:testing-get-notebook (url-or-port &rest paths)
  (ein:log 'debug "TESTING-GET-NOTEBOOK start")
  (ein:notebooklist-open* url-or-port)
  (ein:testing-wait-until (lambda () (and (bufferp (get-buffer (format ein:notebooklist-buffer-name-template url-or-port)))
                                          (ein:notebooklist-get-buffer url-or-port))))
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (prog1 (ein:notebook-get-opened-notebook url-or-port (apply #'ein:glom-paths paths))
      (ein:log 'debug "TESTING-GET-NOTEBOOK end"))))

(defun ein:testing-get-untitled0-or-create (url-or-port &optional path)
  (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-notebook url-or-port path *ein:testing-notebook-name*)))
    (if notebook
        (progn (ein:log 'debug
                 "TESTING-GET-UNTITLED0-OR-CREATE notebook already exists")
               notebook)
      (ein:log 'debug
        "TESTING-GET-UNTITLED0-OR-CREATE creating notebook")
      (lexical-let (done-p
                    (kernelspec (ein:get-kernelspec url-or-port "default")))
        (ein:notebooklist-new-notebook url-or-port kernelspec
                                       (lambda (notebook created)
                                         (setq *ein:testing-notebook-name*
                                               (ein:$notebook-notebook-name notebook))
                                         (setq done-p t)))
        (ein:testing-wait-until (lambda () done-p) nil 10000 2000)
        (prog1
            (ein:testing-get-notebook url-or-port path *ein:testing-notebook-name*)
          (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
            (lexical-let (done-p)
              (ein:notebooklist-reload nil t (lambda (&rest args) (setq done-p t)))
              (ein:testing-wait-until (lambda () done-p) nil 10000 1000)))
          (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE end"))))))

(defvar ein:notebooklist-after-open-hook nil)

(defadvice ein:notebooklist-open--finish
  (after ein:testing-notebooklist-open--finish activate)
  "Advice to add `ein:notebooklist-after-open-hook'."
  (run-hooks 'ein:notebooklist-after-open-hook))

;; (ert-deftest 00-jupyter-start-server ()
;;   (ein:log 'verbose "ERT TESTING-JUPYTER-START-SERVER start")
;;   (condition-case err
;;       (ein:testing-start-server)
;;     (error (ein:log 'verbose "ERT TESTING-JUPYTER-START-SERVER error when launching: %s" err)
;;            (sit-for 10)
;;            (ein:jupyter-server-login-and-open)))
;;   (should (processp %ein:jupyter-server-session%))
;;   (ein:log 'verbose "ERT TESTING-JUPYTER-START-SERVER end"))

(ert-deftest 01-open-notebooklist ()
  (ein:log 'verbose "ERT OPEN-NOTEBOOKLIST start")
  (ein:notebooklist-open* *ein:testing-port*)
  (ein:testing-wait-until
   (lambda () (ein:aand
               (ein:notebooklist-get-buffer *ein:testing-port*)
               (with-current-buffer it (eq major-mode 'ein:notebooklist-mode))))))


(ert-deftest 00-query-kernelspecs ()
  (ein:log 'info "ERT QUERY-KERNELSPECS")
  (ein:log 'info (format "ERT QUERY-KERNELSPECS: Pre-query kernelspec count %s." (hash-table-count *ein:kernelspecs*)))
  (should (>= (hash-table-count *ein:kernelspecs*) 1))
  (ein:log 'info (format "ERT QUERY-KERNELSPECS: Post-query kernelspec %S." (ein:need-kernelspecs *ein:testing-port*))))

(ert-deftest 10-get-untitled0-or-create ()
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:%notebook%)
                     *ein:testing-notebook-name*))))
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE end"))

(ert-deftest 20-delete-untitled0 ()
  (ein:log 'verbose "----------------------------------")
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 start")
  (with-current-buffer (ein:notebooklist-get-buffer *ein:testing-port*)
    (let* ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*))
           (the-url (ein:url *ein:testing-port* (ein:$notebook-notebook-path notebook))))
      (should (member the-url (ein:notebooklist-list-paths "notebook")))
      (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 deleting notebook")
      (lexical-let (done-p)
        (ein:notebooklist-delete-notebook
         (ein:$notebook-notebook-path notebook)
         (lambda (&rest args) (setq done-p t)))
        (ein:testing-wait-until (lambda () done-p) nil 10000 1000))
      (lexical-let (done-p)
        (ein:content-query-hierarchy
         (ein:url *ein:testing-port*)
         (lambda (&rest args) (setq done-p t)))
        (ein:testing-wait-until (lambda () done-p) nil 10000 1000))
      (should-not (member the-url (ein:notebooklist-list-paths "notebook")))))
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 end"))

(ert-deftest 11-notebook-execute-current-cell-simple ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "a = 100\na")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until (lambda () (not (slot-value cell 'running)))))
      ;; (message "%s" (buffer-string))
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward "100" nil t))))))

(defun ein:testing-image-type (image)
  "Return the type of IMAGE.
See the definition of `create-image' for how it works."
  (assert (and (listp image) (eq (car image) 'image)) nil
          "%S is not an image." image)
  (plist-get (cdr image) :type))

(ert-deftest 12-notebook-execute-current-cell-pyout-image ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      ;; Use IPython.core.display rather than IPython.display to
      ;; test it with older (< 0.13) IPython.
      (insert (concat "from IPython.core.display import SVG\n"
                      (format "SVG(data=\"\"\"%s\"\"\")"
                              ein:testing-example-svg)))
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        ;; It seems in this case, watching `:running' does not work
        ;; well sometimes.  Probably "output reply" (iopub) comes
        ;; before "execute reply" in this case.
        (ein:testing-wait-until (lambda () (slot-value cell 'outputs)))
        ;; This cell has only one input
        (should (= (length (oref cell :outputs)) 1))
        ;; This output is a SVG image
        (let ((out (nth 0 (oref cell :outputs))))
          (should (equal (plist-get out :output_type) "execute_result"))
          (should (plist-get out :svg))))
      ;; Check the actual output in the buffer:
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (= (forward-line) 0))
        (if (image-type-available-p 'svg)
            (let ((image (get-text-property (point) 'display)))
              (should (eq (ein:testing-image-type image) 'svg)))
          (ein:log 'info
            "Skipping image check as SVG image type is not available."))))))

(ert-deftest 13-notebook-execute-current-cell-stream ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "print('Hello')")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until (lambda () (not (oref cell :running)))
                                ))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))

(ert-deftest 14-notebook-execute-current-cell-question ()
  (lexical-let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "range?")
      (lexical-let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until
         (lambda ()
           (and (not (oref cell :running))
                (ein:$notebook-pager notebook)
                (get-buffer (ein:$notebook-pager notebook))))))
      (with-current-buffer (get-buffer (ein:$notebook-pager notebook))
        (should (search-forward "Docstring:"))))))

(ert-deftest 15-notebook-request-help ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (let ((pager-name (ein:$notebook-pager ein:%notebook%)))
        (ein:aif (get-buffer pager-name)
            (kill-buffer it))
        (insert "file")
        (call-interactively #'ein:pytools-request-help)
        ;; Pager buffer will be created when got the response
        (ein:testing-wait-until
         (lambda () (get-buffer pager-name)))
        (with-current-buffer (get-buffer pager-name)
          (should (search-forward "Docstring:")))))))

(ert-deftest 30-testing-jupyter-stop-server ()
  (ein:log 'verbose "ERT TESTING-JUPYTER-STOP-SERVER start")

  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (ein:jupyter-server-stop t ein:testing-dump-file-server)
    (cl-flet ((orphans-find (pid) (search (ein:$kernel-kernel-id (ein:$notebook-kernel notebook)) (alist-get 'args (process-attributes pid)))))
      (should-not (loop repeat 10
                        with orphans = (seq-filter #'orphans-find
                                                   (list-system-processes))
                        until (and (null orphans) (ein:jupyter-server-process))
                        do (sleep-for 0 1000)
                           (setq orphans (seq-filter #'orphans-find (list-system-processes)))
                        finally return orphans))))
  (ein:log 'verbose "ERT TESTING-JUPYTER-STOP-SERVER end"))
