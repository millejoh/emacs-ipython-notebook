;; Load all test-ein-*.el files for interactive/batch use.

;; Usage:
;;   emacs -Q -batch -L ... -l tests/test-load.el -f ert-run-tests-batch
;; You will need to set load paths using `-L' switch.

(defun eintest:load-files (&optional regex dir)
  (let* ((dir (or dir (file-name-directory load-file-name)))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (mapc #'load files)))

(eintest:load-files "^test-ein-.*\\.el$")
