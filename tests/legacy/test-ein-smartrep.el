(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-smartrep)

(ert-deftest ein:smartrep-notebook-mode-alist-fboundp ()
  (loop for (k . f) in ein:smartrep-notebook-mode-alist
        do (should (fboundp f))))
