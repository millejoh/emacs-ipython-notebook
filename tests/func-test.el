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

(ein:setq-if-not ein:testing-dump-file-log "func-test-batch-log.log")
(ein:setq-if-not ein:testing-dump-file-messages "func-test-batch-messages.log")
(ein:setq-if-not ein:testing-dump-server-log "func-test_server_batch_emacs.log")
(ein:setq-if-not ein:testing-jupyter-server-command "c:/Users/mille/Miniconda3/envs/jupyter/Scripts/jupyter.exe")
(ein:setq-if-not ein:testing-jupyter-server-directory "c:/Users/mille/Dropbox/Projects/emacs-ipython-notebook/tests/notebook/nbformat4")

(setq message-log-max t)

(defvar ein:testing-port nil)
(defvar ein:testing-token nil)

(defun ein:testing-start-server ()
  (unless (and (boundp '%ein:jupyter-server-session%) (processp %ein:jupyter-server-session%) (bufferp (process-buffer %ein:jupyter-server-session%)))
    (ein:log 'debug "TESTING-START-SERVER starting jupyter server.")
    (ein:jupyter-server-start ein:testing-jupyter-server-command ein:testing-jupyter-server-directory t)
    (ein:testing-wait-until "Jupyter notebook server."
                            #'(lambda (buf)
                                (with-current-buffer buf
                                  (goto-char (point-min))
                                  (search-forward "Copy/paste this URL into your browser" nil t)))
                            (list (process-buffer %ein:jupyter-server-session%))
                            500000)
    (ein:log 'debug "TESTING-START-SERVER logging in.")
    (multiple-value-bind (url token) (ein:jupyter-server-conn-info)
      (ein:notebooklist-login url token)
      (setq ein:testing-port url)
      (setq ein:testing-token token)
      (ein:log 'debug "TESTING-START-SERVER succesfully logged in.")
      url)))

(defun ein:testing-wait-until (message predicate &optional predargs max-count)
  "Wait until PREDICATE function returns non-`nil'.
PREDARGS is argument list for the PREDICATE function.
Make MAX-COUNT larger \(default 50) to wait longer before timeout."
  (ein:log 'debug "TESTING-WAIT-UNTIL start")
  (ein:log 'debug "TESTING-WAIT-UNTIL waiting on: %s" message)
  (unless max-count (setq max-count 50000))
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
  ;; Kill notebook list buffer here to make sure next
  ;; `ein:testing-wait-until' works properly.
  (kill-buffer (ein:notebooklist-get-buffer url-or-port))
  (when path
    (setq notebook-name (format "%s/%s" path notebook-name)))
  (let ((content (ein:notebooklist-open url-or-port path t)))
    ;; (sit-for 1.0) ;; Because some computers are too fast???
    (ein:testing-wait-until "ein:notebooklist-open"
                            (lambda () (and content
                                            (ein:$content-url-or-port content))))
    (with-current-buffer (ein:notebooklist-get-buffer (ein:$content-url-or-port content))
      (prog1
          (ignore-errors
            (ein:notebooklist-open-notebook-by-name notebook-name (ein:$content-url-or-port content)))
        (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME end")))))

(defun ein:testing-get-untitled0-or-create (url-or-port &optional path)
  (unless path (setq path ""))
  (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-notebook-by-name url-or-port "Untitled.ipynb")))
    (if notebook
        (progn (ein:log 'debug
                 "TESTING-GET-UNTITLED0-OR-CREATE notebook already exists")
               notebook)
      (ein:log 'debug
        "TESTING-GET-UNTITLED0-OR-CREATE creating notebook")
      (let ((created nil)
            (kernelspec (first (ein:list-available-kernels url-or-port))))
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
                          nil 500000)
  (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
    (ein:testing-wait-until "ein:notebooklist-get-buffer"
                            (lambda () (eql major-mode 'ein:notebooklist-mode))
                            nil
                            50000)
    (ein:log 'debug "TESTING-DELETE-NOTEBOOK deleting notebook")
    (ein:notebooklist-delete-notebook (ein:$notebook-notebook-path notebook)))
  (ein:log 'debug "TESTING-DELETE-NOTEBOOK end"))

(ert-deftest ein:testing-jupyter-start-server ()
  (ein:log 'verbose "ERT TESTING-JUPYTER-START-SERVER start")
  (ein:testing-start-server)
  (should (processp %ein:jupyter-server-session%))
  (ein:log 'verbose "ERT TESTING-JUPYTER-START-SERVER end"))

(ert-deftest ein:testing-get-untitled0-or-create ()
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE start")
  (ein:testing-start-server)
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50000)
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:%notebook%)
                     "Untitled.ipynb"))))
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE end"))

(ert-deftest ein:testing-delete-untitled0 ()
  (ein:log 'verbose "----------------------------------")
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 start")
  (ein:testing-start-server)

  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 creating notebook")
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:test-get-untitled0-or-create"
     (lambda ()
       (ein:aand notebook
                 (ein:$notebook-kernel it)
                 (ein:kernel-live-p it)))
     nil 50000)
    (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 deleting notebook")
    (ein:testing-delete-notebook ein:testing-port notebook))
  (ein:log 'verbose
    "ERT TESTING-DELETE-UNTITLED0 check that the notebook is deleted")
  (let ((num-notebook
         (length (ein:testing-get-notebook-by-name ein:testing-port
                                                   "Untitled.ipynb"
                                                   ""))))
    (should (= num-notebook 0)))
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 end"))

(ert-deftest ein:notebook-execute-current-cell-simple ()
  (ein:testing-start-server)
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50000)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "a = 100\na")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (not (slot-value cell 'running)))
                                nil
                                50000))
      ;; (message "%s" (buffer-string))
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward "100" nil t))))))

(defun ein:testing-image-type (image)
  "Return the type of IMAGE.
See the definition of `create-image' for how it works."
  (assert (and (listp image) (cl-find 'image image :key #'car)) nil
          "%S is not an image." image)
  (plist-get (cdr (cl-find 'image image :key #'car)) :type))

(ert-deftest ein:notebook-execute-current-cell-pyout-image ()
  (ein:testing-start-server)
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
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
                                50000)
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

(ert-deftest ein:notebook-execute-current-cell-stream ()
  (ein:testing-start-server)
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50000)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "print('Hello')")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (not (oref cell :running)))
                                nil
                                50000))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))

(ert-deftest ein:notebook-execute-current-cell-question ()
  (ein:testing-start-server)
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50000)
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "range?")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until
         "ein:worksheet-execute-cell"
         (lambda () (not (oref cell :running)))
         nil 50000))
      (with-current-buffer (get-buffer (ein:$notebook-pager notebook))
        (should (search-forward "Docstring:"))))))

(ert-deftest ein:notebook-request-help ()
  (ein:testing-start-server)
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it)))
     nil 50000)
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
         nil 50000)
        (with-current-buffer (get-buffer pager-name)
          (should (search-forward "Docstring:")))))))
