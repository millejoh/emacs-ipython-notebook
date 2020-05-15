;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-notebook)
(require 'ein-testing-notebook)


;;; Event handler

(defun ein:testing-worksheet-set-dirty
  (pre-dirty value post-dirty fired-in)
  (with-current-buffer (ein:testing-make-notebook-with-outputs '(nil))
    (when pre-dirty
      (ein:cell-goto (ein:worksheet-get-current-cell))
      (insert "something"))
    (should (equal (ein:worksheet-modified-p ein:%worksheet%) pre-dirty))
    (with-current-buffer (funcall fired-in)
      (let ((events (oref ein:%worksheet% :events))
            (cell (ein:worksheet-get-current-cell)))
        (ein:events-trigger events 'set_dirty.Worksheet
                            (list :cell cell :value value))))
    (should (equal (ein:worksheet-modified-p ein:%worksheet%) post-dirty))))

(defun ein:testing-scratchsheet-buffer ()
  (ein:worksheet-buffer (ein:notebook-scratchsheet-open ein:%notebook%)))

(defmacro ein:testing-worksheet-set-dirty-deftest
  (pre-dirty value post-dirty &optional fired-in)
  (let ((name (intern (format "ein:worksheet-set-dirty/%s-to-%s-fired-in-%s"
                              pre-dirty value
                              (or fired-in "current-buffer"))))
        (fired-in-defun
         (cl-case fired-in
           (scratchsheet 'ein:testing-scratchsheet-buffer)
           (t 'current-buffer))))
    `(ert-deftest ,name ()
       (ein:testing-worksheet-set-dirty ,pre-dirty ,value ,post-dirty
                                        #',fired-in-defun))))

(ein:testing-worksheet-set-dirty-deftest t   nil nil)
(ein:testing-worksheet-set-dirty-deftest t   t   t  )
(ein:testing-worksheet-set-dirty-deftest nil nil nil)
(ein:testing-worksheet-set-dirty-deftest nil t   t  )
(ein:testing-worksheet-set-dirty-deftest t   nil t   scratchsheet)
(ein:testing-worksheet-set-dirty-deftest t   t   t   scratchsheet)
(ein:testing-worksheet-set-dirty-deftest nil nil nil scratchsheet)
(ein:testing-worksheet-set-dirty-deftest nil t   nil scratchsheet)
