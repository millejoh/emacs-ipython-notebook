(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebook)
(require 'ein-testing-notebook)


;;; Event handler

(defun ein:testing-worksheet-set-dirty (pre-dirty post-dirty)
  (with-current-buffer (ein:testing-make-notebook-with-outputs '(nil))
    (let ((events (oref ein:%worksheet% :events))
          (cell (ein:worksheet-get-current-cell)))
      (when pre-dirty
        (ein:cell-goto cell)
        (insert "something"))
      (should (equal (ein:worksheet-modified-p ein:%worksheet%) pre-dirty))
      (ein:events-trigger events 'set_dirty.Worksheet
                          (list :cell cell :value post-dirty))
      (should (equal (ein:worksheet-modified-p ein:%worksheet%) post-dirty)))))

(ert-deftest ein:worksheet-set-dirty/t-to-t ()
  (ein:testing-worksheet-set-dirty t t))

(ert-deftest ein:worksheet-set-dirty/t-to-nil ()
  (ein:testing-worksheet-set-dirty t nil))

(ert-deftest ein:worksheet-set-dirty/nil-to-t ()
  (ein:testing-worksheet-set-dirty nil t))

(ert-deftest ein:worksheet-set-dirty/nil-to-nil ()
  (ein:testing-worksheet-set-dirty nil nil))
