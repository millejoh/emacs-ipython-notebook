;; Tests for cell function that requires notebook buffer

(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebook)
(require 'test-ein-notebook)


;; Test utils

(defmacro eintest:with-one-cell (cell-type &rest body)
  "Insert new cell of CELL-TYPE in a clean notebook and execute BODY.
The new cell is bound to a variable `cell'."
  (declare (indent 1))
  `(with-current-buffer (eintest:notebook-make-empty)
     (let ((cell (ein:notebook-insert-cell-below ein:notebook ,cell-type nil)))
       (ein:cell-goto cell)
       ,@body)))

(defvar eintest:example-svg "\
<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
 \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">

<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">
  <circle cx=\"100\" cy=\"50\" r=\"40\" />
</svg>")


;; ein:cell-location

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


;; Cell outputs

(ert-deftest ein:cell-insert-output-pyout-text ()
  (eintest:with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :outputs (list (list :output_type "pyout"
                                  :prompt_number 222
                                  :text "some output"))
             :input "some input"
             :prompt_number 111)
       :ewoc (ein:$notebook-ewoc ein:notebook))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[111\\]:
some input
Out \\[222\\]:
some output
"))))

(ert-deftest ein:cell-insert-output-pyout-latex ()
  (eintest:with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :outputs (list (list :output_type "pyout"
                                  :prompt_number 222
                                  :text "some output text"
                                  :latex "some output \\LaTeX"))
             :input "some input"
             :prompt_number 111)
       :ewoc (ein:$notebook-ewoc ein:notebook))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[111\\]:
some input
Out \\[222\\]:
some output \\\\LaTeX
"))))

(ert-deftest ein:cell-insert-output-pyout-svg ()
  (eintest:with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :outputs (list (list :output_type "pyout"
                                  :prompt_number 222
                                  :text "some output text"
                                  :svg eintest:example-svg))
             :input "some input"
             :prompt_number 111)
       :ewoc (ein:$notebook-ewoc ein:notebook))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[111\\]:
some input
Out \\[222\\]:
 \n"))))

(ert-deftest ein:cell-insert-output-pyout-html ()
  (eintest:with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :outputs (list (list :output_type "pyout"
                                  :prompt_number 222
                                  :text "some output text"
                                  :html "<b>not shown</b>"))
             :input "some input"
             :prompt_number 111)
       :ewoc (ein:$notebook-ewoc ein:notebook))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[111\\]:
some input
Out \\[222\\]:
some output text
"))))

(ert-deftest ein:cell-insert-output-pyout-javascript ()
  (eintest:with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :outputs (list (list :output_type "pyout"
                                  :prompt_number 222
                                  :text "some output text"
                                  :javascript "$.do.something()"))
             :input "some input"
             :prompt_number 111)
       :ewoc (ein:$notebook-ewoc ein:notebook))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[111\\]:
some input
Out \\[222\\]:
some output text
"))))
