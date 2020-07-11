;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-dev)
(ein:dev-require-all :ignore-p (lambda (f) (equal f "ein-autoloads.el")))
(eval-when-compile
 ;; do it also at compile time.
  (ein:dev-require-all :ignore-p (lambda (f) (equal f "ein-autoloads.el"))))


(defun eintest:assert-keymap-fboundp (keymap)
  (let (assert-fboundp)
    (setq assert-fboundp
          (lambda (_event value)
            (cond
             ((keymapp value)
              (map-keymap assert-fboundp value))
             ((and (listp value) (eq (car value) 'menu-item))
              (funcall assert-fboundp (cadr value) (cl-caddr value)))
             ((consp value)
              (should (functionp (cdr value))))
             (value  ; nil is also valid in keymap
              (should (commandp value))))))
    (map-keymap assert-fboundp keymap)))

(defmacro eintest:test-keymap (keymap)
  `(ert-deftest ,(intern (format "%s--assert-fboundp" keymap)) ()
     (eintest:assert-keymap-fboundp ,keymap)))

(eintest:test-keymap ein:notebooklist-mode-map)
(eintest:test-keymap ein:notebook-mode-map)
(eintest:test-keymap ein:traceback-mode-map)
(eintest:test-keymap ein:shared-output-mode-map)
