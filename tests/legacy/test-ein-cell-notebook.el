;; Tests for cell function that requires notebook buffer

(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebook)
(require 'ein-testing-notebook)


;; ein:cell-location

(ert-deftest ein:cell-location-codecell-prompt-beg ()
  (ein:testing-with-one-cell 'code
    (should (equal (marker-position (ein:cell-location cell :prompt))
                   (save-excursion
                     (goto-char (point-max))
                     (search-backward "In [ ]:")
                     (point))))))

(ert-deftest ein:cell-location-codecell-prompt-end ()
  (ein:testing-with-one-cell 'code
    (should (equal (marker-position (ein:cell-location cell :prompt t))
                   (1- (point))))))

(ert-deftest ein:cell-location-codecell-input-beg ()
  (ein:testing-with-one-cell 'code
    (insert "some text")
    (should (equal (marker-position (ein:cell-location cell :input))
                   (1- (point-at-bol))))))

(ert-deftest ein:cell-location-codecell-input-end ()
  (ein:testing-with-one-cell 'code
    (insert "some text")
    (should (equal (marker-position (ein:cell-location cell :input t))
                   (1+ (point))))))


;; from-json

(ert-deftest eintest:cell-input-prompt-number ()
  (ein:testing-with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :input "some input"
             :prompt_number 111)
       :ewoc (oref ein:%worksheet% :ewoc))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[111\\]:
some input
"))))

(ert-deftest eintest:cell-input-prompt-star ()
  (ein:testing-with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :input "some input"
             :prompt_number "*")
       :ewoc (oref ein:%worksheet% :ewoc))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[\\*\\]:
some input
"))))

(ert-deftest eintest:cell-input-prompt-empty ()
  (ein:testing-with-one-cell
      (ein:cell-from-json
       (list :cell_type "code"
             :input "some input")
       :ewoc (oref ein:%worksheet% :ewoc))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[ \\]:
some input
"))))


;; Insert pyout/display_data

(defun eintest:cell-insert-output (outputs regexp)
  (let ((ein:output-type-preference
         '(emacs-lisp svg png jpeg text html latex javascript)))
    (ein:testing-with-one-cell
        (ein:cell-from-json
         (list :cell_type "code"
               :outputs outputs
               :input "some input"
               :prompt_number 111)
         :ewoc (oref ein:%worksheet% :ewoc))
      (goto-char (ein:cell-location cell))
      (should (looking-at (format "\
In \\[111\\]:
some input
%s" regexp))))))

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
  ((:latex "some output \\LaTeX")))

(when (image-type-available-p 'svg)
  (eintest:gene-test-cell-insert-output-pyout-and-display-data
   svg
   (" ")
   ((:text "some output text" :svg ein:testing-example-svg))))

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

(when (image-type-available-p 'svg)
  (eintest:gene-test-cell-insert-output-pyout-and-display-data
   text-latex-svg
   ("first output text" "second output \\\\LaTeX" " ")
   ((:text "first output text")
    (:latex "second output \\LaTeX")
    (:text "some output text" :svg ein:testing-example-svg))))


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
