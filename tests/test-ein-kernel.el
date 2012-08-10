(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-kernel)


(defun eintest:kernel-new (port)
  (ein:kernel-new port "/kernels"
                  (get-buffer-create "*eintest: dummy for kernel test*")))

(ert-deftest ein:kernel-start-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (notebook-id "NOTEBOOK-ID")
         (desired-url "http://127.0.0.1:8888/kernels?notebook=NOTEBOOK-ID")
         (dummy-buffer (get-buffer-create "*eintest:dummy*"))
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url) dummy-buffer)
           (set-process-query-on-exit-flag (process flag)))
      (ein:kernel-start kernel notebook-id)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-restart-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID/restart")
         (dummy-buffer (get-buffer-create "*eintest:dummy*"))
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url) dummy-buffer)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (&rest ignore) (make-ein:$websocket))
           (ein:events-trigger (&rest ignore)))
      (ein:kernel--kernel-started kernel :data (list :kernel_id kernel-id))
      (ein:kernel-restart kernel)
      (should (equal got-url desired-url)))))


(ert-deftest ein:kernel-interrupt-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID/interrupt")
         (dummy-buffer (get-buffer-create "*eintest:dummy*"))
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url) dummy-buffer)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (&rest ignore) (make-ein:$websocket)))
      (ein:kernel--kernel-started kernel :data (list :kernel_id kernel-id))
      (ein:kernel-interrupt kernel)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-kill-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID")
         (dummy-buffer (get-buffer-create "*eintest:dummy*"))
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url) dummy-buffer)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (&rest ignore) (make-ein:$websocket)))
      (ein:kernel--kernel-started kernel :data (list :kernel_id kernel-id))
      (ein:kernel-kill kernel)
      (let* ((l (split-string got-url "?"))
             (got-url-0 (nth 0 l))
             (got-url-1 (nth 1 l)))
        (should (equal got-url-0 desired-url))
        (should (string-match "^_=[0-9]+$" got-url-1))))))


;;; Test `ein:kernel-construct-help-string'

(defvar eintest:kernel-construct-help-string-pcallsig-list
  '(nil :call_def :init_definition :definition))

(defvar eintest:kernel-construct-help-string-pdocstring-list
  '(nil :call_docstring :init_docstring :docstring))

(defun eintest:kernel-construct-help-string-test-func (content result)
  (should (equal (ein:kernel-construct-help-string content) result)))

(defun eintest:kernel-construct-help-string-loop (&optional test
                                                            pcallsig-list
                                                            pdocstring-list)
  "Run tests for `ein:kernel-construct-help-string-loop'.

TEST
   A function takes two arguments, namely CONTENT and RESULT.
   CONTENT is the argument to `ein:kernel-construct-help-string' and
   RESULT must match to its returned value.  Use `should' to test
   equality.
PCALLSIG-LIST
   `nil' or (subset of) `eintest:kernel-construct-help-string-pcallsig-list'.
PDOCSTRING-LIST
   `nil' or (subset of) `eintest:kernel-construct-help-string-pdocstring-list'.

All combinations of PCALLSIG-LIST and PDOCSTRING-LIST are used to
construct CONTENT and RESULT."
  (unless test (setq test #'eintest:kernel-construct-help-string-test-func))
  (unless pcallsig-list
    (setq pcallsig-list
          eintest:kernel-construct-help-string-pcallsig-list))
  (unless pdocstring-list
    (setq pdocstring-list
          eintest:kernel-construct-help-string-pdocstring-list))
  (loop with callsig = "function(a=1, b=2, c=d)"
        with docstring = "This function does what."
        for pcallsig in pcallsig-list
        do (loop for pdoc in pdocstring-list
                 for content = (append
                                (when pcallsig (list pcallsig callsig))
                                (when pdoc (list pdoc docstring)))
                 for result = (ein:aif (append
                                        (when pcallsig (list callsig))
                                        (when pdoc (list docstring)))
                                  (ein:join-str "\n" it))
                 do (funcall test content result))))

(ert-deftest ein:kernel-construct-help-string-when-found ()
  (eintest:kernel-construct-help-string-loop))

(ert-deftest ein:kernel-construct-help-string-when-not-found ()
  (should (equal (ein:kernel-construct-help-string nil) nil)))
;; Included in `ein:kernel-construct-help-string-when-found', but test
;; it explicitly to be sure.

(provide 'test-ein-kernel)
