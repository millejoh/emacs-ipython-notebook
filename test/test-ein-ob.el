; -*- lexical-binding:t -*-
(require 'ert)
(require 'ob-ein)


;; Test utils

(ert-deftest ein:ob-anonymous-p ()
  (should (ob-ein-anonymous-p ".ein-python.ipynb"))
  (should (ob-ein-anonymous-p ".ein.ipynb"))
  (should-not (ob-ein-anonymous-p "ein-python.ipynb"))
  (should-not (ob-ein-anonymous-p "Untitled.ipynb")))

;;; This is the content portion of a response from the content API.
(defvar eintest:ob-src-block
  "#+BEGIN_SRC ein :session 8888/Untitled.ipynb
import sys

a = 14500
b = a+1000
sys.version
#+END_SRC
")

(ert-deftest ein:ob-aware ()
  (let ((org-babel-load-languages (quote ((ein . t)))))
    (with-temp-buffer
      (save-excursion
        (org-mode)
        (insert eintest:ob-src-block)
        (search-backward "SRC")
        (cl-letf (((symbol-function 'ob-ein--initiate-session)
                   (lambda (&rest _args) (make-ein:$notebook))))
          (should (call-interactively #'org-edit-special)))))))
