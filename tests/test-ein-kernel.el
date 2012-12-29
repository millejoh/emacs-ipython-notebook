(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-kernel)
(require 'ein-testing-kernel)


(defun eintest:kernel-new (port)
  (ein:kernel-new port "/kernels"
                  (get-buffer-create "*eintest: dummy for kernel test*")))

(ert-deftest ein:kernel-start-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (notebook-id "NOTEBOOK-ID")
         (desired-url "http://127.0.0.1:8888/kernels?notebook=NOTEBOOK-ID")
         (dummy-response (make-request-response))
         got-url)
    (flet ((request (url &rest ignore) (setq got-url url) dummy-response)
           (set-process-query-on-exit-flag (process flag)))
      (ein:kernel-start kernel notebook-id)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-restart-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID/restart")
         (dummy-response (make-request-response))
         got-url)
    (flet ((request (url &rest ignore) (setq got-url url) dummy-response)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (&rest ignore) (make-ein:$websocket))
           (ein:events-trigger (&rest ignore)))
      (ein:kernel--kernel-started
       kernel :data (list :ws_url "ws://127.0.0.1:8888" :kernel_id kernel-id))
      (ein:kernel-restart kernel)
      (should (equal got-url desired-url)))))


(ert-deftest ein:kernel-interrupt-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID/interrupt")
         (dummy-response (make-request-response))
         got-url)
    (flet ((request (url &rest ignore) (setq got-url url) dummy-response)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (&rest ignore) (make-ein:$websocket)))
      (ein:kernel--kernel-started
       kernel :data (list :ws_url "ws://127.0.0.1:8888" :kernel_id kernel-id))
      (ein:kernel-interrupt kernel)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-kill-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID")
         (dummy-response (make-request-response))
         got-url)
    (flet ((request (url &rest ignore) (setq got-url url) dummy-response)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (&rest ignore) (make-ein:$websocket)))
      (ein:kernel--kernel-started
       kernel :data (list :ws_url "ws://127.0.0.1:8888" :kernel_id kernel-id))
      (ein:kernel-kill kernel)
      (should (equal got-url desired-url)))))


;;; Test `ein:kernel-construct-help-string'

(ert-deftest ein:kernel-construct-help-string-when-found ()
  (ein:testing-kernel-construct-help-string-loop))

(ert-deftest ein:kernel-construct-help-string-when-not-found ()
  (should (equal (ein:kernel-construct-help-string nil) nil)))
;; Included in `ein:kernel-construct-help-string-when-found', but test
;; it explicitly to be sure.
