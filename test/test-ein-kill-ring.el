;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-kill-ring)

(ert-deftest ein:kill-ring-simple ()
  (let (ein:kill-ring
        ein:kill-ring-yank-pointer)
    (ein:kill-new 1)
    (should (equal (ein:current-kill 0) 1))))

(defun eintest:kill-ring-simple-repeat-setup ()
  (cl-loop for i from 0 below 5
           do (ein:kill-new i)
           do (should (equal (ein:current-kill 0) i))))

(ert-deftest ein:kill-ring-simple-repeat ()
  (let (ein:kill-ring
        ein:kill-ring-yank-pointer)
    (eintest:kill-ring-simple-repeat-setup)
    (should (equal ein:kill-ring ein:kill-ring-yank-pointer))
    (should (equal ein:kill-ring '(4 3 2 1 0)))))

(ert-deftest ein:kill-ring-repeat-n-1 ()
  (let (ein:kill-ring
        ein:kill-ring-yank-pointer)
    (eintest:kill-ring-simple-repeat-setup)
    (cl-loop for i in '(3 2 1 0 4 3 2)
             do (should (equal (ein:current-kill 1) i)))
    (should-not (equal ein:kill-ring ein:kill-ring-yank-pointer))
    (should (equal ein:kill-ring '(4 3 2 1 0)))
    (should (equal ein:kill-ring-yank-pointer '(2 1 0)))))

(ert-deftest ein:kill-ring-exceeds-max ()
  (let (ein:kill-ring
        ein:kill-ring-yank-pointer
        (ein:kill-ring-max 3))
    (eintest:kill-ring-simple-repeat-setup)
    (should (equal ein:kill-ring ein:kill-ring-yank-pointer))
    (should (equal (length ein:kill-ring) ein:kill-ring-max))
    (should (equal ein:kill-ring '(4 3 2)))))
