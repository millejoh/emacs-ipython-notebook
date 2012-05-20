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


;; Insert pyout/display_data

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

(defmacro eintest:gene-test-cell-insert-output-pyout-and-display-data
  (name regexps outputs)
  (declare (indent defun))
  (let ((test-pyout
         (intern (format "ein:cell-insert-output-pyout-%s" name)))
        (test-display-data
         (intern (format "ein:cell-insert-output-display-data-%s" name)))
        (outputs-pyout
         (loop for i from 1
               for x in outputs
               collect
               (append x (list :output_type "pyout" :prompt_number i))))
        (outputs-display-data
         (mapcar (lambda (x) (append '(:output_type "display_data") x))
                 outputs))
        (regexp-pyout
         (ein:join-str
          ""
          (loop for i from 1
                for x in regexps
                collect (format "Out \\[%s\\]:\n%s\n" i x))))
        (regexp-display-data
         (concat (ein:join-str "\n" regexps) "\n")))
    `(progn
       (ert-deftest ,test-pyout ()
         (eintest:cell-insert-output ',outputs-pyout
                                     ,regexp-pyout))
       (ert-deftest ,test-display-data ()
         (eintest:cell-insert-output ',outputs-display-data
                                     ,regexp-display-data)))))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  text ("some output") ((:text "some output")))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  latex
  ("some output \\\\LaTeX")
  ((:text "some output text" :latex "some output \\LaTeX")))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  svg
  (" ")
  ((:text "some output text" :svg eintest:example-svg)))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  html
  ("some output text")
  ((:text "some output text" :html "<b>not shown</b>")))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  javascript
  ("some output text")
  ((:text "some output text" :javascript "$.do.something()")))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  text-two
  ("first output text" "second output text")
  ((:text "first output text") (:text "second output text")))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  text-javascript
  ("first output text" "second output text")
  ((:text "first output text")
   (:text "second output text" :javascript "$.do.something()")))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  text-latex-svg
  ("first output text" "second output \\\\LaTeX" " ")
  ((:text "first output text")
   (:text "some output text" :latex "second output \\LaTeX")
   (:text "some output text" :svg eintest:example-svg)))


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
some stdout 1some stdout 2
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
