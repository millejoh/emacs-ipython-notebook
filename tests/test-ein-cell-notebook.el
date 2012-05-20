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


;; Insert pyout

(defun eintest:cell-insert-output (outputs regexp)
  (eintest:with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :outputs outputs
             :input "some input"
             :prompt_number 111)
       :ewoc (ein:$notebook-ewoc ein:notebook))
    (goto-char (ein:cell-location cell))
    (should (looking-at (format "\
In \\[111\\]:
some input
%s" regexp)))))

(ert-deftest ein:cell-insert-output-pyout-text ()
  (eintest:cell-insert-output
   (list (list :output_type "pyout"
               :prompt_number 222
               :text "some output"))
   "\
Out \\[222\\]:
some output
"))

(ert-deftest ein:cell-insert-output-pyout-latex ()
  (eintest:cell-insert-output
   (list (list :output_type "pyout"
               :prompt_number 222
               :text "some output text"
               :latex "some output \\LaTeX"))
   "\
Out \\[222\\]:
some output \\\\LaTeX
"))

(ert-deftest ein:cell-insert-output-pyout-svg ()
  (eintest:cell-insert-output
   (list (list :output_type "pyout"
               :prompt_number 222
               :text "some output text"
               :svg eintest:example-svg))
   "\
Out \\[222\\]:
 \n"))

(ert-deftest ein:cell-insert-output-pyout-html ()
  (eintest:cell-insert-output
   (list (list :output_type "pyout"
               :prompt_number 222
               :text "some output text"
               :html "<b>not shown</b>"))
   "\
Out \\[222\\]:
some output text
"))

(ert-deftest ein:cell-insert-output-pyout-javascript ()
  (eintest:cell-insert-output
   (list (list :output_type "pyout"
               :prompt_number 222
               :text "some output text"
               :javascript "$.do.something()"))
   "\
Out \\[222\\]:
some output text
"))


;; Insert pyerr

(ert-deftest ein:cell-insert-output-pyerr-simple ()
  (eintest:cell-insert-output
   (list (list :output_type "pyerr"
               :traceback '("some traceback 1"
                            "some traceback 2")))
   "\
some traceback 1
some traceback 2
"))


;; Insert display_data


;; Insert stream

(ert-deftest ein:cell-insert-output-stream-simple-stdout ()
  (eintest:cell-insert-output
   (list (list :output_type "stream"
               :stream "stdout"
               :text "some stdout 1"))
   "\
some stdout 1
"))

(ert-deftest ein:cell-insert-output-stream-stdout-stderr ()
  (eintest:cell-insert-output
   (list (list :output_type "stream"
               :stream "stdout"
               :text "some stdout 1")
         (list :output_type "stream"
               :stream "stderr"
               :text "some stderr 1"))
   "\
some stdout 1
some stderr 1
"))

(ert-deftest ein:cell-insert-output-stream-flushed-stdout ()
  (eintest:cell-insert-output
   (list (list :output_type "stream"
               :stream "stdout"
               :text "some stdout 1")
         (list :output_type "stream"
               :stream "stdout"
               :text "some stdout 2"))
   "\
some stdout 1
some stdout 2
"))

(ert-deftest ein:cell-insert-output-stream-flushed-stdout-and-stderr ()
  (eintest:cell-insert-output
   (list (list :output_type "stream"
               :stream "stdout"
               :text "some stdout 1")
         (list :output_type "stream"
               :stream "stderr"
               :text "some stderr 1")
         (list :output_type "stream"
               :stream "stdout"
               :text "some stdout 2")
         (list :output_type "stream"
               :stream "stderr"
               :text "some stderr 2"))
   "\
some stdout 1
some stderr 1
some stdout 2
some stderr 2
"))
