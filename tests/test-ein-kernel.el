(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-kernel)


(ert-deftest ein:kernel-start-check-url ()
  (let* ((port 8888)
         (kernel (ein:kernel-new 8888))
         (notebook-id "NOTEBOOK-ID")
         (desired-url "http://127.0.0.1:8888/kernels?notebook=NOTEBOOK-ID")
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url)))
      (ein:kernel-start kernel notebook-id nil)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-restart-check-url ()
  (let* ((port 8888)
         (kernel (ein:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID/restart")
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url))
           (ein:kernel-start-channels (&rest ignore))
           (ein:kernel-stop-channels (&rest ignore)))
      (ein:kernel--handle-start-kernel
       kernel
       (list :kernel_id kernel-id)
       (lambda ()))
      (ein:kernel-restart kernel nil)
      (should (equal got-url desired-url)))))


(ert-deftest ein:kernel-interrupt-check-url ()
  (let* ((port 8888)
         (kernel (ein:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/kernels/KERNEL-ID/interrupt")
         got-url)
    (flet ((url-retrieve (url &rest ignore) (setq got-url url))
           (ein:kernel-start-channels (&rest ignore))
           (ein:kernel-stop-channels (&rest ignore)))
      (ein:kernel--handle-start-kernel
       kernel
       (list :kernel_id kernel-id)
       (lambda ()))
      (ein:kernel-interrupt kernel)
      (should (equal got-url desired-url)))))
