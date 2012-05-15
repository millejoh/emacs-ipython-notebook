(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-cell)

(ert-deftest ein:cell-element-get-basecell ()
  (let ((cell (ein:basecell "Cell")))
    ;; it's not supported
    (should-error (ein:cell-element-get :prompt))))

(ert-deftest ein:cell-element-get-codecell ()
  (let* ((element (list :prompt 1
                        :input 2
                        :output '(3 4)
                        :footer 5))
         (cell (ein:cell-from-type "code" :element element)))
    (mapc (lambda (kv)
            (should (equal (ein:cell-element-get cell (car kv)) (cdr kv))))
          (ein:plist-iter element))
    (should (equal (ein:cell-element-get cell :output 0) 3))
    (should (equal (ein:cell-element-get cell :output 1) 4))
    (should (equal (ein:cell-element-get cell :output 2) nil))
    (should (equal (ein:cell-element-get cell :after-input) 3))
    (should (equal (ein:cell-element-get cell :after-output) 5))
    (should (equal (ein:cell-element-get cell :before-input) 1))
    (should (equal (ein:cell-element-get cell :before-output) 2))
    (should (equal (ein:cell-element-get cell :last-output) 4))))

(ert-deftest ein:cell-element-get-codecell-no-ouput ()
  (let* ((element (list :prompt 1
                        :input 2
                        :footer 5))
         (cell (ein:cell-from-type "code" :element element)))
    (mapc (lambda (kv)
            (should (equal (ein:cell-element-get cell (car kv)) (cdr kv))))
          (ein:plist-iter element))
    (should (equal (ein:cell-element-get cell :after-input) 5))
    (should (equal (ein:cell-element-get cell :last-output) 2))))

(ert-deftest ein:cell-element-get-textcell ()
  (let* ((element (list :prompt 1
                        :input 2
                        :footer 5))
         (cell (ein:cell-from-type "text" :element element)))
    (mapc (lambda (kv)
            (should (equal (ein:cell-element-get cell (car kv)) (cdr kv))))
          (ein:plist-iter element))
    (should (equal (ein:cell-element-get cell :after-input) 5))
    (should (equal (ein:cell-element-get cell :before-input) 1))))
