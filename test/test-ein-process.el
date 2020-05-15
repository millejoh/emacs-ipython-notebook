;; -*- lexical-binding:t -*-
(require 'ert)
(require 'ein-process)

(ert-deftest ein:process-check-suitable ()
  (should-not (equal (ein:process-suitable-notebook-dir (concat default-directory "features/support")) (concat default-directory "features/support"))))

(ert-deftest ein:process-divine ()
  (with-current-buffer "*scratch*"
    (erase-buffer))
  (ein:process-divine-dir 1 "" "*scratch*")
  (ein:process-divine-port 1 "" "*scratch*"))
