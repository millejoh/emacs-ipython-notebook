(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-dev)
(ein:dev-require-all)
(eval-when-compile (ein:dev-require-all)) ; do it also at compile time.


(defun eintest:assert-keymap-fboundp (keymap)
  (let (assert-fboundp)
    (setq assert-fboundp
          (lambda (event value) (if (keymapp value)
                                    (map-keymap assert-fboundp value)
                                  (should (commandp value)))))
    (map-keymap assert-fboundp keymap)))

(defmacro eintest:test-keymap (keymap)
  `(ert-deftest ,(intern (format "%s--assert-fboundp" keymap)) ()
     (eintest:assert-keymap-fboundp ,keymap)))

(eintest:test-keymap ein:notebooklist-mode-map)
(eintest:test-keymap ein:notebook-mode-map)
(eintest:test-keymap ein:connect-mode-map)
(eintest:test-keymap ein:traceback-mode-map)
(eintest:test-keymap ein:shared-output-mode-map)
(eintest:test-keymap ein:pager-mode-map)
