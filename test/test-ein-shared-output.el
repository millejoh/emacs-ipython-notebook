;; -*- lexical-binding:t -*-
(require 'ein-shared-output)

(defmacro eintest:shared-output-with-buffer (&rest body)
  (declare (indent 0))
  `(with-current-buffer (ein:shared-output-create-buffer)
     (ein:shared-output-get-or-create)
     ,@body))

(defmacro eintest:shared-output-is-empty-context-of (func)
  `(ert-deftest ,(intern (format "%s--shared-output" func)) ()
     (eintest:shared-output-with-buffer
       (should-not (,func)))))


;; Generic getter

(ert-deftest ein:get-cell-at-point--shared-output ()
  (eintest:shared-output-with-buffer
    (should (eq (ein:get-cell-at-point)
                   (ein:shared-output-get-cell))))
  (with-temp-buffer
    (should-not (ein:get-cell-at-point--shared-output))))

;; FIXME: Add tests with non-empty shared output buffer.
(eintest:shared-output-is-empty-context-of ein:get-url-or-port)
(eintest:shared-output-is-empty-context-of ein:get-notebook)
(eintest:shared-output-is-empty-context-of ein:get-kernel)
(eintest:shared-output-is-empty-context-of ein:get-traceback-data)
