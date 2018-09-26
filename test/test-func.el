(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-loaddefs)
(require 'ein-notebooklist)
(require 'ein-jupyter)
(require 'wid-edit)
(require 'ein-testing)
(require 'ein-testing-cell)

(let ((backend (getenv "EL_REQUEST_BACKEND")))
  (when (and backend (not (equal backend "")))
    (setq request-backend (intern backend))
    (message "Using request-backend = %S" request-backend)))


(setq message-log-max t)

(defun ein:testing-wait-until (message predicate &optional predargs max-count)
  "Wait until PREDICATE function returns non-`nil'.
PREDARGS is argument list for the PREDICATE function.
Make MAX-COUNT larger \(default 50) to wait longer before timeout."
  (ein:log 'debug "TESTING-WAIT-UNTIL start")
  (ein:log 'debug "TESTING-WAIT-UNTIL waiting on: %s" message)
  (unless max-count (setq max-count 50))
  (unless (loop repeat max-count
                when (apply predicate predargs)
                return t
                ;; borrowed from `deferred:sync!':
                do (sit-for 0.2)
                do (sleep-for 0.2))
    (error "Timeout"))
  (ein:log 'debug "TESTING-WAIT-UNTIL end"))

(defun ein:testing-get-notebook-by-name (url-or-port notebook-name &optional path)
  (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME start")
  (when path
    (setq notebook-name (format "%s/%s" path notebook-name)))
  (ein:notebooklist-open url-or-port path t)
  (ein:testing-wait-until "ein:notebooklist-open"
                          (lambda () (and (bufferp (get-buffer (format ein:notebooklist-buffer-name-template url-or-port)))
                                          (ein:notebooklist-get-buffer url-or-port))))
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (prog1
        (ignore-errors
          (ein:notebooklist-open-notebook-by-name notebook-name url-or-port))
      (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME end"))))

(defun ein:testing-get-untitled0-or-create (url-or-port &optional path)
  (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-notebook-by-name url-or-port "Untitled.ipynb" path)))
    (if notebook
        (progn (ein:log 'debug
                 "TESTING-GET-UNTITLED0-OR-CREATE notebook already exists")
               notebook)
      (ein:log 'debug
        "TESTING-GET-UNTITLED0-OR-CREATE creating notebook")
      (let ((created nil)
            (kernelspec (ein:get-kernelspec url-or-port "default")))
        (ein:notebooklist-new-notebook url-or-port kernelspec path
                                       (lambda (&rest -ignore-)
                                         (setq created t)))
        (ein:testing-wait-until "ein:notebooklist-new-notebook"
                                (lambda () created)))
      (prog1
          (ein:testing-get-notebook-by-name url-or-port "Untitled.ipynb" path)
        (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE end")))))

(defvar ein:notebooklist-after-open-hook nil)

(defadvice ein:notebooklist-url-retrieve-callback
  (after ein:testing-notebooklist-url-retrieve-callback activate)
  "Advice to add `ein:notebooklist-after-open-hook'."
  (run-hooks 'ein:notebooklist-after-open-hook))

(defun ein:testing-delete-notebook (url-or-port notebook &optional path)
  (ein:log 'debug "TESTING-DELETE-NOTEBOOK start")
  (ein:notebooklist-open url-or-port (ein:$notebook-notebook-path notebook) t)
  (ein:testing-wait-until "ein:notebooklist-open"
                          (lambda ()
                            (bufferp (get-buffer (format ein:notebooklist-buffer-name-template url-or-port))))
                          nil 50)
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (ein:testing-wait-until "ein:notebooklist-get-buffer"
                            (lambda () (eql major-mode 'ein:notebooklist-mode))
                            nil
                            50)
    (ein:log 'debug "TESTING-DELETE-NOTEBOOK deleting notebook")
    (ein:notebooklist-delete-notebook (ein:$notebook-notebook-path notebook)))
  (ein:log 'debug "TESTING-DELETE-NOTEBOOK end"))

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
  (ein:notebooklist-open *ein:testing-port* "/" t)
  (ein:testing-wait-until
   "ein:notebooklist-open"
   (lambda ()
     (ein:notebooklist-get-buffer *ein:testing-port*))
   nil 5000)
  (with-current-buffer (ein:notebooklist-get-buffer *ein:testing-port*)
    (should (eql major-mode 'ein:notebooklist-mode))))


(ert-deftest 00-query-kernelspecs ()
  (ein:log 'info "ERT QUERY-KERNELSPECS")
  (ein:log 'info (format "ERT QUERY-KERNELSPECS: Pre-query kernelspec count %s." (hash-table-count ein:available-kernelspecs)))
  (ein:query-kernelspecs *ein:testing-port*)
  (should (>= (hash-table-count ein:available-kernelspecs) 1))
  (ein:log 'info (format "ERT QUERY-KERNELSPECS: Post-query kernelspec %S." (ein:list-available-kernels *ein:testing-port*))))

(ert-deftest 10-get-untitled0-or-create ()
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil)
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:%notebook%)
                     "Untitled.ipynb"))))
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE end"))

(ert-deftest 20-delete-untitled0 ()
  (ein:log 'verbose "----------------------------------")
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 start")
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 creating notebook")
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     "ein:test-get-untitled0-or-create"
     (lambda ()
       (ein:aand notebook
                 (ein:$notebook-kernel it)
                 (ein:kernel-live-p it)))
     nil 50)
    (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 deleting notebook")
    (ein:testing-delete-notebook *ein:testing-port* notebook))
  (ein:log 'verbose
    "ERT TESTING-DELETE-UNTITLED0 check that the notebook is deleted")
  (let ((num-notebook
         (length (ein:testing-get-notebook-by-name *ein:testing-port*
                                                   "Untitled.ipynb"
                                                   ""))))
    (should (= num-notebook 0)))
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 end"))

(ert-deftest 11-notebook-execute-current-cell-simple ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "a = 100\na")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (not (slot-value cell 'running)))
                                nil))
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
     "ein:testing-get-untitled0-or-create"
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
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (slot-value cell 'outputs))
                                nil
                                50)
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
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "print('Hello')")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (not (oref cell :running)))
                                nil))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))

(ert-deftest 14-notebook-execute-current-cell-question ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "range?")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until
         "ein:worksheet-execute-cell"
         (lambda () (not (oref cell :running)))
         nil 50))
      (with-current-buffer (get-buffer (ein:$notebook-pager notebook))
        (should (search-forward "Docstring:"))))))

(ert-deftest 15-notebook-request-help ()
  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (let ((pager-name (ein:$notebook-pager ein:%notebook%)))
        (ein:aif (get-buffer pager-name)
            (kill-buffer it))
        (insert "file")
        (call-interactively #'ein:pytools-request-help)
        ;; Pager buffer will be created when got the response
        (ein:testing-wait-until
         "ein:pythools-request-help"
         (lambda () (get-buffer pager-name))
         nil)
        (with-current-buffer (get-buffer pager-name)
          (should (search-forward "Docstring:")))))))

(ert-deftest 30-testing-jupyter-stop-server ()
  (ein:log 'verbose "ERT TESTING-JUPYTER-STOP-SERVER start")

  (let ((notebook (ein:testing-get-untitled0-or-create *ein:testing-port*)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (ein:jupyter-server-stop t ein:testing-dump-server-log))
    (should-not (processp %ein:jupyter-server-session%))
    (cl-flet ((orphans-find (pid) (search (ein:$kernel-kernel-id (ein:$notebook-kernel notebook)) (alist-get 'args (process-attributes pid)))))
      (should-not (loop repeat 10
                        with orphans = (seq-filter #'orphans-find
                                                   (list-system-processes))
                        until (null orphans)
                        do (sleep-for 0 1000) 
                           (setq orphans (seq-filter #'orphans-find (list-system-processes)))
                        finally return orphans))))
  (ein:log 'verbose "ERT TESTING-JUPYTER-STOP-SERVER end"))
