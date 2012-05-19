;; Tests for cell function that requires notebook buffer for test

(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebook)
(require 'test-ein-notebook)

(defmacro eintest:with-one-cell (cell-type &rest body)
  "Insert new cell of CELL-TYPE in a clean notebook and execute BODY.
The new cell is bound to a variable `cell'."
  (declare (indent 1))
  `(with-current-buffer (eintest:notebook-make-empty)
     (let ((cell (ein:notebook-insert-cell-below ein:notebook ,cell-type nil)))
       (ein:cell-goto cell)
       ,@body)))

(ert-deftest ein:cell-location-codecell-prompt-beg ()
  (eintest:with-one-cell 'code
    (should (equal (marker-position (ein:cell-location cell :prompt))
                   (save-excursion
                     (goto-char (point-max))
                     (search-backward "In [ ]:")
                     (point))))))

(ert-deftest ein:cell-location-codecell-prompt-end ()
  (eintest:with-one-cell 'code
    (should (equal (marker-position (ein:cell-location cell :prompt t))
                   (1- (point))))))

(ert-deftest ein:cell-location-codecell-input-beg ()
  (eintest:with-one-cell 'code
    (insert "some text")
    (should (equal (marker-position (ein:cell-location cell :input))
                   (1- (point-at-bol))))))

(ert-deftest ein:cell-location-codecell-input-end ()
  (eintest:with-one-cell 'code
    (insert "some text")
    (should (equal (marker-position (ein:cell-location cell :input t))
                   (1+ (point))))))
