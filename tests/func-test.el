(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebooklist)

;; Execute `eintest:dz-ipython-start' before starting the following
;; test to setup server.
;; See: ./setup-server.el


(defvar eintest:port 8889)

(defun eintest:wait-until (predicate &optional predargs max-count)
  "Wait until PREDICATE function returns non-`nil'.
PREDARGS is argument list for the PREDICATE function.
Make MAX-COUNT larger \(default 50) to wait longer before timeout."
  (unless (setq max-count 50))
  (unless (loop repeat max-count
                when (apply predicate predargs)
                return t
                ;; borrowed from `deferred:sync!':
                do (sit-for 0.05)
                do (sleep-for 0.05))
    (error "Timeout")))

(defun eintest:get-notebook-by-name (url-or-port notebook-name)
  (with-current-buffer (ein:notebooklist-open url-or-port nil)
    (eintest:wait-until (lambda () ein:notebooklist))
    (loop for note in (ein:$notebooklist-data ein:notebooklist)
          for name = (plist-get note :name)
          for notebook-id = (plist-get note :notebook_id)
          when (equal name notebook-name)
          return
          (ein:notebook-open
           (ein:$notebooklist-url-or-port ein:notebooklist)
           notebook-id))))

(defun eintest:get-untitled0-or-create (url-or-port)
  (let ((notebook (eintest:get-notebook-by-name url-or-port "Untitled0")))
    (if notebook
        notebook
      (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
        (setq ein:notebooklist nil)
        (eintest:wait-until (lambda () ein:notebooklist))
        (setq ein:notebooklist nil)
        (ein:notebooklist-new-notebook url-or-port)
        (eintest:wait-until (lambda () ein:notebooklist)))
      (eintest:get-notebook-by-name url-or-port "Untitled0"))))

(ert-deftest eintest:get-untitled0-or-create ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-ready-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:notebook) "Untitled0")))))

(ert-deftest ein:notebook-execute-current-cell-simple ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (eintest:wait-until (lambda () (ein:aand (ein:$notebook-kernel notebook)
                                             (ein:kernel-ready-p it))))
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
                                             (ein:kernel-ready-p it))))
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
                                             (ein:kernel-ready-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (ein:notebook-insert-cell-below-command)
      (insert "print 'Hello'")
      (let ((cell (ein:notebook-execute-current-cell)))
        (eintest:wait-until (lambda () (not (oref cell :running)))))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))
