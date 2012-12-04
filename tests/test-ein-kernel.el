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
      (ein:kernel--kernel-started
       kernel :data (list :ws_url "ws://127.0.0.1:8888" :kernel_id kernel-id))
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
      (ein:kernel--kernel-started
       kernel :data (list :ws_url "ws://127.0.0.1:8888" :kernel_id kernel-id))
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
      (ein:kernel--kernel-started
       kernel :data (list :ws_url "ws://127.0.0.1:8888" :kernel_id kernel-id))
      (ein:kernel-kill kernel)
      (let* ((l (split-string got-url "?"))
             (got-url-0 (nth 0 l))
             (got-url-1 (nth 1 l)))
        (should (equal got-url-0 desired-url))
        (should (string-match "^_=[0-9]+$" got-url-1))))))


;;; Test `ein:kernel-construct-help-string'

(ert-deftest ein:kernel-construct-help-string-when-found ()
  (ein:testing-kernel-construct-help-string-loop))

(ert-deftest ein:kernel-construct-help-string-when-not-found ()
  (should (equal (ein:kernel-construct-help-string nil) nil)))
;; Included in `ein:kernel-construct-help-string-when-found', but test
;; it explicitly to be sure.
