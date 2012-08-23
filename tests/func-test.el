(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebooklist)
(require 'wid-edit)
(require 'ein-testing)

(ein:setq-if-not ein:testing-dump-file-log "func-test-batch-log.log")
(ein:setq-if-not ein:testing-dump-file-messages "func-test-batch-messages.log")

;; Execute `eintest:dz-ipython-start' before starting the following
;; test to setup server.
;; See: ./setup-server.el


(defvar eintest:port 8889)

(defun eintest:wait-until (predicate &optional predargs max-count)
  "Wait until PREDICATE function returns non-`nil'.
PREDARGS is argument list for the PREDICATE function.
Make MAX-COUNT larger \(default 50) to wait longer before timeout."
  (ein:log 'debug "EINTEST:WAIT-UNTIL start")
  (unless (setq max-count 50))
  (unless (loop repeat max-count
                when (apply predicate predargs)
                return t
                ;; borrowed from `deferred:sync!':
                do (sit-for 0.05)
                do (sleep-for 0.05))
    (error "Timeout"))
  (ein:log 'debug "EINTEST:WAIT-UNTIL end"))

(defun eintest:get-notebook-by-name (url-or-port notebook-name)
  (ein:log 'debug "EINTEST:GET-NOTEBOOK-BY-NAME start")
  ;; Kill notebook list buffer here to make sure next
  ;; `eintest:wait-until' works properly.
  (kill-buffer (ein:notebooklist-get-buffer url-or-port))
  (with-current-buffer (ein:notebooklist-open url-or-port nil)
    (eintest:wait-until (lambda () ein:%notebooklist%))
    (prog1
        (ein:notebooklist-open-notebook-by-name notebook-name)
      (ein:log 'debug "EINTEST:GET-NOTEBOOK-BY-NAME end"))))

(defun eintest:get-untitled0-or-create (url-or-port)
  (ein:log 'debug "EINTEST:GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (eintest:get-notebook-by-name url-or-port "Untitled0")))
    (if notebook
        (progn (ein:log 'debug
                 "EINTEST:GET-UNTITLED0-OR-CREATE notebook already exists")
               notebook)
      (with-current-buffer (ein:notebooklist-open url-or-port t)
        (setq ein:%notebooklist% nil)
        (eintest:wait-until (lambda () ein:%notebooklist%))
        (ein:notebooklist-new-notebook url-or-port)
        (eintest:wait-until
         (lambda () (eintest:get-notebook-by-name url-or-port "Untitled0"))))
      (prog1
          (eintest:get-notebook-by-name url-or-port "Untitled0")
        (ein:log 'debug "EINTEST:GET-UNTITLED0-OR-CREATE end")))))

(defun eintest:delete-notebook-by-name (url-or-port notebook-name)
  (ein:log 'debug "EINTEST:DELETE-NOTEBOOK-BY-NAME start")
  (with-current-buffer (ein:notebooklist-open url-or-port nil)
    (eintest:wait-until (lambda () ein:%notebooklist%))
    (save-excursion
      (goto-char (point-min))
      (search-forward notebook-name)
      (move-beginning-of-line 1)
      (search-forward "Delete")
      (flet ((y-or-n-p (ignore) t))
        (widget-button-press (point))))
    (setq ein:%notebooklist% nil)
    (eintest:wait-until (lambda () ein:%notebooklist%))
    (ein:log 'debug "EINTEST:DELETE-NOTEBOOK-BY-NAME end")))

(ert-deftest eintest:get-untitled0-or-create ()
  (ein:log 'verbose "ERT EINTEST:GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:%notebook%) "Untitled0"))))
  (ein:log 'verbose "ERT EINTEST:GET-UNTITLED0-OR-CREATE end"))

(ert-deftest eintest:delete-untitled0 ()
  (ein:log 'verbose "ERT EINTEST:DELETE-UNTITLED0 start")
  (loop
   for i from 0 to 1
   do (ein:log 'debug "ERT EINTEST:DELETE-UNTITLED0 i=%s" i)
   do (ein:log 'debug "ERT EINTEST:DELETE-UNTITLED0 creating notebook")
   do (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
        (eintest:wait-until
         (lambda () (ein:aand (ein:$notebook-kernel notebook)
                              (ein:kernel-live-p it)))))
   do (ein:log 'debug "ERT EINTEST:DELETE-UNTITLED0 delete notebook")
   do (eintest:delete-notebook-by-name eintest:port "Untitled0")
   do (ein:log 'debug
        "ERT EINTEST:DELETE-UNTITLED0 check the notebook is delete")
   do (let ((num-notebook
             (length (eintest:get-notebook-by-name eintest:port "Untitled0"))))
        (should (= num-notebook 0))))
  (ein:log 'debug "ERT EINTEST:DELETE-UNTITLED0 end"))

(ert-deftest ein:notebook-execute-current-cell-simple ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:notebook-insert-cell-below-command)
      (insert "a = 100\na")
      (let ((cell (ein:notebook-execute-current-cell)))
        (eintest:wait-until (lambda () (not (oref cell :running)))))
      ;; (message "%s" (buffer-string))
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward "100" nil t))))))

(ert-deftest ein:notebook-execute-current-cell-pyout-image ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:notebook-insert-cell-below-command)
      (insert (ein:join-str "\n" '("import pylab"
                                   "pylab.plot([1,2,3])")))
      (let ((cell (ein:notebook-execute-current-cell)))
        (eintest:wait-until (lambda () (not (oref cell :running)))))
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp
                 "<matplotlib\\.lines\\.Line2D at .*>" nil t))))))

(ert-deftest ein:notebook-execute-current-cell-stream ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:notebook-insert-cell-below-command)
      (insert "print 'Hello'")
      (let ((cell (ein:notebook-execute-current-cell)))
        (eintest:wait-until (lambda () (not (oref cell :running)))))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))

(ert-deftest ein:notebook-execute-current-cell-question ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:notebook-insert-cell-below-command)
      (insert "range?")
      (let ((cell (ein:notebook-execute-current-cell)))
        (eintest:wait-until (lambda () (not (oref cell :running)))))
      (with-current-buffer (get-buffer (ein:$notebook-pager notebook))
        (should (search-forward "Docstring:\nrange"))))))

(ert-deftest ein:notebook-request-help ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:notebook-insert-cell-below-command)
      (let ((pager-name (ein:$notebook-pager ein:%notebook%)))
        (ein:aif (get-buffer pager-name)
            (kill-buffer it))
        (insert "file")
        (call-interactively #'ein:pytools-request-help)
        ;; Pager buffer will be created when got the response
        (eintest:wait-until (lambda () (get-buffer pager-name)))
        (with-current-buffer (get-buffer pager-name)
          (should (search-forward "Docstring:\nfile")))))))
