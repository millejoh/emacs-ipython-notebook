(require 'ein-connect)
(require 'test-ein-notebook)

(defmacro eintest:with-connected-buffer (&rest body)
  (declare (indent 0))
  `(let* ((notebook-buffer (eintest:notebook-make-empty))
          (notebook (buffer-local-value 'ein:notebook notebook-buffer)))
     (with-temp-buffer
       (erase-buffer)
       (ein:connect-buffer-to-notebook notebook)
       ,@body)))

(ert-deftest ein:get-url-or-port--connect ()
  (eintest:with-connected-buffer
    (should (equal (ein:get-url-or-port)
                   (ein:$notebook-url-or-port notebook)))))

(ert-deftest ein:get-notebook--connect ()
  (eintest:with-connected-buffer
    (should (eq (ein:get-notebook) notebook))))

(ert-deftest ein:get-kernel--connect ()
  (eintest:with-connected-buffer
    (should (equal (ein:get-kernel)
                   (ein:$notebook-kernel notebook)))))

(ert-deftest ein:get-cell-at-point--connect ()
  "`ein:get-cell-at-point' is in empty context in connected buffer."
  (eintest:with-connected-buffer
    (should-not (ein:get-cell-at-point))))

(ert-deftest ein:get-traceback-data--connect ()
  (eintest:with-connected-buffer
    ;; FIXME: write test with non-empty TB
    (should-not (ein:get-traceback-data))))
