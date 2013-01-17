(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebooklist)
(require 'wid-edit)
(require 'ein-testing)
(require 'ein-testing-cell)

(let ((backend (getenv "EL_REQUEST_BACKEND")))
  (when (and backend (not (equal backend "")))
    (setq request-backend (intern backend))
    (message "Using request-backend = %S" request-backend)))

(ein:setq-if-not ein:testing-dump-file-log "func-test-batch-log.log")
(ein:setq-if-not ein:testing-dump-file-messages "func-test-batch-messages.log")
(setq message-log-max t)


(defvar ein:testing-port 8889)

(defun ein:testing-wait-until (predicate &optional predargs max-count)
  "Wait until PREDICATE function returns non-`nil'.
PREDARGS is argument list for the PREDICATE function.
Make MAX-COUNT larger \(default 50) to wait longer before timeout."
  (ein:log 'debug "TESTING-WAIT-UNTIL start")
  (unless (setq max-count 50))
  (unless (loop repeat max-count
                when (apply predicate predargs)
                return t
                ;; borrowed from `deferred:sync!':
                do (sit-for 0.05)
                do (sleep-for 0.05))
    (error "Timeout"))
  (ein:log 'debug "TESTING-WAIT-UNTIL end"))

(defun ein:testing-get-notebook-by-name (url-or-port notebook-name)
  (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME start")
  ;; Kill notebook list buffer here to make sure next
  ;; `ein:testing-wait-until' works properly.
  (kill-buffer (ein:notebooklist-get-buffer url-or-port))
  (with-current-buffer (ein:notebooklist-open url-or-port nil)
    (ein:testing-wait-until (lambda () ein:%notebooklist%))
    (prog1
        (ein:notebooklist-open-notebook-by-name notebook-name)
      (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME end"))))

(defun ein:testing-get-untitled0-or-create (url-or-port)
  (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-notebook-by-name url-or-port "Untitled0")))
    (if notebook
        (progn (ein:log 'debug
                 "TESTING-GET-UNTITLED0-OR-CREATE notebook already exists")
               notebook)
      (ein:log 'debug
        "TESTING-GET-UNTITLED0-OR-CREATE creating notebook")
      (let ((created nil))
        (ein:notebooklist-new-notebook url-or-port
                                       (lambda (&rest -ignore-)
                                         (setq created t)))
        (ein:testing-wait-until (lambda () created)))
      (prog1
          (ein:testing-get-notebook-by-name url-or-port "Untitled0")
        (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE end")))))

(defvar ein:notebooklist-after-open-hook nil)

(defadvice ein:notebooklist-url-retrieve-callback
  (after ein:testing-notebooklist-url-retrieve-callback activate)
  "Advice to add `ein:notebooklist-after-open-hook'."
  (run-hooks 'ein:notebooklist-after-open-hook))

(defun ein:testing-delete-notebook-by-name (url-or-port notebook-name)
  (ein:log 'debug "TESTING-DELETE-NOTEBOOK-BY-NAME start")
  (lexical-let (called-p)
    (let ((ein:notebooklist-after-open-hook
           (list (lambda () (setq called-p t)))))
      (with-current-buffer (ein:notebooklist-open url-or-port nil)
        (ein:testing-wait-until (lambda () ein:%notebooklist%))
        (save-excursion
          (goto-char (point-min))
          (search-forward notebook-name)
          (move-beginning-of-line 1)
          (search-forward "Delete")
          (flet ((y-or-n-p (ignore) t))
            (widget-button-press (point))))
        (ein:testing-wait-until (lambda () called-p))
        (ein:log 'debug "TESTING-DELETE-NOTEBOOK-BY-NAME end")))))

(ert-deftest ein:testing-get-untitled0-or-create ()
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:%notebook%)
                     "Untitled0"))))
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE end"))

(ert-deftest ein:testing-delete-untitled0 ()
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 start")
  (loop
   for i from 0 to 1
   do (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 i=%s" i)
   do (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 creating notebook")
   do (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
        (ein:testing-wait-until
         (lambda () (ein:aand (ein:$notebook-kernel notebook)
                              (ein:kernel-live-p it)))))
   do (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 delete notebook")
   do (ein:testing-delete-notebook-by-name ein:testing-port "Untitled0")
   do (ein:log 'debug
        "ERT TESTING-DELETE-UNTITLED0 check the notebook is delete")
   do (let ((num-notebook
             (length (ein:testing-get-notebook-by-name ein:testing-port
                                                       "Untitled0"))))
        (should (= num-notebook 0))))
  (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 end"))

(ert-deftest ein:notebook-execute-current-cell-simple ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "a = 100\na")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until (lambda () (not (oref cell :running)))))
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

(ert-deftest ein:notebook-execute-current-cell-pyout-image ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
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
        (ein:testing-wait-until (lambda () (oref cell :outputs)))
        ;; This cell has only one input
        (should (= (length (oref cell :outputs)) 1))
        ;; This output is a SVG image
        (let ((out (nth 0 (oref cell :outputs))))
          (should (equal (plist-get out :output_type) "pyout"))
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
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "print 'Hello'")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until (lambda () (not (oref cell :running)))))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))

(ert-deftest ein:notebook-execute-current-cell-question ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "range?")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until (lambda () (not (oref cell :running)))))
      (with-current-buffer (get-buffer (ein:$notebook-pager notebook))
        (should (search-forward "Docstring:\nrange"))))))

(ert-deftest ein:notebook-request-help ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
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
        (ein:testing-wait-until (lambda () (get-buffer pager-name)))
        (with-current-buffer (get-buffer pager-name)
          (should (search-forward "Docstring:\nfile")))))))
