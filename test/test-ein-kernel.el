;; -*- lexical-binding:t -*-

(require 'ert)

(require 'ein-kernel)
(require 'ein-testing-kernel)
(require 'ein-testing-notebook)

(defun eintest:kernel-new (port)
  (ein:kernel-new port "" nil "/api/kernels"
                  (get-buffer-create "*eintest: dummy for kernel test*")))

(ert-deftest ein:kernel-restart-check-url ()
  (let* ((notebook (ein:notebook-new ein:testing-notebook-dummy-url "" nil))
         (kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/api/sessions/KERNEL-ID")
         (dummy-response (make-request-response))
         got-url)
    (setf (ein:$notebook-kernel notebook) kernel)
    (cl-letf (((symbol-function 'request)
               (lambda (url &rest _ignore) (setq got-url url) dummy-response))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'ein:kernel-stop-channels) #'ignore)
              ((symbol-function 'ein:websocket) (lambda (&rest _ignore) (make-ein:$websocket :ws nil :kernel kernel :closed-by-client nil)))
              ((symbol-function 'ein:events-trigger) #'ignore)
              ((symbol-function 'ein:get-notebook-or-error) (lambda () (ein:get-notebook))))
      (ein:kernel-retrieve-session--success
       kernel nil :data (list :ws_url "ws://127.0.0.1:8888" :id kernel-id))
      (ein:kernel-restart-session (ein:$notebook-kernel notebook))
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-interrupt-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/api/kernels/KERNEL-ID/interrupt")
         (dummy-response (make-request-response))
         got-url)

    (cl-letf (((symbol-function 'request)
               (lambda (url &rest _ignore) (setq got-url url) dummy-response))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'ein:kernel-stop-channels) #'ignore)
              ((symbol-function 'ein:websocket) (lambda (&rest _ignore) (make-ein:$websocket :ws nil :kernel kernel :closed-by-client nil)))
              ((symbol-function 'ein:websocket-open-p) (lambda (&rest _ignore) t)))
      (ein:kernel-retrieve-session--success
       kernel nil :data (list :ws_url "ws://127.0.0.1:8888" :id kernel-id))
      (ein:kernel-interrupt kernel)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-kill-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/api/sessions/KERNEL-ID")
         (dummy-response (make-request-response))
         got-url)
    (cl-letf (((symbol-function 'request)
               (lambda (url &rest _ignore) (setq got-url url) dummy-response))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'ein:kernel-stop-channels) #'ignore)
              ((symbol-function 'ein:websocket)
               (lambda (&rest _ignore) (make-ein:$websocket :ws nil :kernel kernel
                                                           :closed-by-client nil)))
              ((symbol-function 'ein:websocket-open-p) (lambda (&rest _ignore) t)))
      (ein:kernel-retrieve-session--success
       kernel nil :data (list :ws_url "ws://127.0.0.1:8888" :id kernel-id))
      (ein:kernel-delete-session nil :kernel kernel))
    (should (equal got-url desired-url))))

;;; Test `ein:kernel-construct-help-string'

(ert-deftest ein:kernel-construct-help-string-when-found ()
  (ein:testing-kernel-construct-help-string-loop))

(ert-deftest ein:kernel-construct-help-string-when-not-found ()
  (should (equal (ein:kernel-construct-help-string nil) nil)))
;; Included in `ein:kernel-construct-help-string-when-found', but test
;; it explicitly to be sure.
