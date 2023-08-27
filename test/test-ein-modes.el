;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-dev)

(defun eintest:assert-keymap-fboundp (keymap)
  (cl-labels ((assert-fboundp (event value)
		(cond
		 ((keymapp value)
		  (map-keymap #'assert-fboundp value))
		 ((and (listp value) (eq (car value) 'menu-item))
		  (funcall #'assert-fboundp (cadr value) (cl-caddr value)))
		 ((not (equal (car-safe value) "--"))
		  (if (consp value)
		      (should (functionp (cdr value)))
		    (when value ; nil is also valid in keymap
		      (should (commandp value))))))))
    (map-keymap #'assert-fboundp keymap)))

(defmacro eintest:test-keymap (keymap)
  `(ert-deftest ,(intern (format "%s--assert-fboundp" keymap)) ()
     (eintest:assert-keymap-fboundp ,keymap)))

(eintest:test-keymap ein:notebooklist-mode-map)
(eintest:test-keymap ein:notebook-mode-map)
(eintest:test-keymap ein:traceback-mode-map)
(eintest:test-keymap ein:shared-output-mode-map)
