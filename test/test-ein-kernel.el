(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-kernel)
(require 'ein-testing-kernel)

(defun eintest:kernel-new (port)
  (ein:kernel-new port "/api/kernels"
                  (get-buffer-create "*eintest: dummy for kernel test*")))

(ert-deftest ein:kernel-restart-check-url ()
  (lexical-let* ((kernel (eintest:kernel-new 8888))
                 (kernel-id "KERNEL-ID")
                 (desired-url "http://127.0.0.1:8888/api/sessions/KERNEL-ID")
                 (dummy-response (make-request-response))
                 got-url)
    (cl-letf (((symbol-function 'request) 
               (lambda (url &rest ignore) (setq got-url url) dummy-response))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'ein:kernel-stop-channels) #'ignore)
              ((symbol-function 'ein:websocket) (lambda (&rest ignore) (make-ein:$websocket :ws nil :kernel kernel :closed-by-client nil)))
              ((symbol-function 'ein:events-trigger) #'ignore)
              ((symbol-function 'ein:get-notebook-or-error) (lambda () (ein:get-notebook))))
      (ein:kernel-start--success
       kernel nil :data (list :ws_url "ws://127.0.0.1:8888" :id kernel-id))
      (ein:kernel-restart kernel)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-interrupt-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/api/kernels/KERNEL-ID/interrupt")
         (dummy-response (make-request-response))
         got-url)
    (flet ((request (url &rest ignore) (setq got-url url) dummy-response)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (url kernel on-message on-close on-open) (make-ein:$websocket :ws nil :kernel kernel :closed-by-client nil))
           (ein:websocket-open-p (websocket) t))
      (ein:kernel-start--success
       kernel nil :data (list :ws_url "ws://127.0.0.1:8888" :id kernel-id))
      (ein:kernel-interrupt kernel)
      (should (equal got-url desired-url)))))

(ert-deftest ein:kernel-kill-check-url ()
  (let* ((kernel (eintest:kernel-new 8888))
         (kernel-id "KERNEL-ID")
         (desired-url "http://127.0.0.1:8888/api/sessions/KERNEL-ID")
         (dummy-response (make-request-response))
         got-url)
    (flet ((request (url &rest ignore) (setq got-url url) dummy-response)
           (set-process-query-on-exit-flag (process flag))
           (ein:kernel-stop-channels (&rest ignore))
           (ein:websocket (url kernel on-message on-close on-open) (make-ein:$websocket :ws nil :kernel kernel :closed-by-client nil)))
      (ein:kernel-start--success
       kernel nil :data (list :ws_url "ws://127.0.0.1:8888" :id kernel-id))
      (ein:kernel-delete kernel)
      (should (equal got-url desired-url)))))


;;; Test `ein:kernel-construct-help-string'

(ert-deftest ein:kernel-construct-help-string-when-found ()
  (ein:testing-kernel-construct-help-string-loop))

(ert-deftest ein:kernel-construct-help-string-when-not-found ()
  (should (equal (ein:kernel-construct-help-string nil) nil)))
;; Included in `ein:kernel-construct-help-string-when-found', but test
;; it explicitly to be sure.
