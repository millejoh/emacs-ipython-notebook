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

(ert-deftest ein:kernel-construct-help-string-when-found ()
  (let ((callsig "function(a=1, b=2, c=d)")
        (docstring "This function does what."))
    (loop for pcallsig in '(:call_def :init_definition :definition)
          do (loop for pdoc in '(:call_docstring :init_docstring :docstring)
                   do (should (equal (ein:kernel-construct-help-string
                                      (list pcallsig callsig
                                            pdoc docstring))
                                     (format "%s\n%s" callsig docstring)))))))

(ert-deftest ein:kernel-construct-help-string-when-callsig-found ()
  (let ((callsig "function(a=1, b=2, c=d)"))
    (loop for pcallsig in '(:call_def :init_definition :definition)
          do (should (equal (ein:kernel-construct-help-string
                             (list pcallsig callsig))
                            callsig)))))

(ert-deftest ein:kernel-construct-help-string-when-doc-found ()
  (let ((docstring "This function does what."))
    (loop for pdoc in '(:call_docstring :init_docstring :docstring)
          do (should (equal (ein:kernel-construct-help-string
                             (list pdoc docstring))
                            docstring)))))

(ert-deftest ein:kernel-construct-help-string-when-not-found ()
  (should (equal (ein:kernel-construct-help-string nil) nil)))
