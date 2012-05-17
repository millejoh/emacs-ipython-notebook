(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-cell)


;;; ein:cell-from-json

(defun eintest:cell-from-json (data &optional args)
  (let ((cell (apply #'ein:cell-from-json data args)))
    (should-not (ein:cell-active-p cell))
    cell))

(ert-deftest ein:cell-from-json-code ()
  (let* ((input-prompt-number 111)
         (output-prompt-number 222)
         (input (ein:join-str "\n" '("first input" "second input")))
         (output-0 (list :output_type "pyout"
                         :prompt_number output-prompt-number
                         :text (list "first output"
                                     "second output")))
         (data (list :cell_type "code"
                     :input input
                     :language "python"
                     :outputs (list output-0)
                     :collapsed json-false
                     :prompt_number input-prompt-number))
         (cell (eintest:cell-from-json data)))
    (should (ein:codecell-p cell))
    (should (equal (oref cell :input-prompt-number) input-prompt-number))
    (should (equal (oref cell :input) input))
    (should (equal (car (oref cell :outputs)) output-0))
    (should (equal (oref cell :collapsed) nil))))

(ert-deftest ein:cell-from-json-text ()
  (let* ((input (ein:join-str "\n" '("first input" "second input")))
         (data (list :cell_type "text" :source input))
         (cell (eintest:cell-from-json data)))
    (should (ein:textcell-p cell))
    (should (equal (oref cell :input) input))))

(ert-deftest ein:cell-from-json-html ()
  (let* ((input (ein:join-str "\n" '("first input" "second input")))
         (data (list :cell_type "html" :source input))
         (cell (eintest:cell-from-json data)))
    (should (ein:htmlcell-p cell))
    (should (equal (oref cell :input) input))))

(ert-deftest ein:cell-from-json-markdown ()
  (let* ((input (ein:join-str "\n" '("first input" "second input")))
         (data (list :cell_type "markdown" :source input))
         (cell (eintest:cell-from-json data)))
    (should (ein:markdowncell-p cell))
    (should (equal (oref cell :input) input))))

(ert-deftest ein:cell-from-json-rst ()
  (let* ((input (ein:join-str "\n" '("first input" "second input")))
         (data (list :cell_type "rst" :source input))
         (cell (eintest:cell-from-json data)))
    (should (ein:rstcell-p cell))
    (should (equal (oref cell :input) input))))


;;; ein:cell-element-get

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
