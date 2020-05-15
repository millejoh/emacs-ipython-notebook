;; Tests for cell function that requires notebook buffer  -*- lexical-binding:t -*-

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
          :source "some input"
          :metadata (list :collapsed json-false :autoscroll json-false)
          :execution_count 111)
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
             :source "some input"
	     :metadata (list :collapsed json-false :autoscroll json-false)
             :execution_count "*")
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
	     :metadata (list :collapsed json-false :autoscroll json-false)
             :source "some input")
       :ewoc (oref ein:%worksheet% :ewoc))
    (goto-char (ein:cell-location cell))
    (should (looking-at "\
In \\[ \\]:
some input
"))))

;; Insert pyout/display_data

(defun eintest:cell-insert-output (outputs regexp)
  (ein:testing-with-one-cell
   (ein:cell-from-json
    (list :cell_type "code"
          :outputs outputs
          :source "some input"
          :metadata (list :collapsed json-false :autoscroll json-false)
          :execution_count 111)
    :ewoc (oref ein:%worksheet% :ewoc))
   (goto-char (ein:cell-location cell))
   ;; (message "%s" (buffer-string))
   (should (looking-at (format "\
In \\[111\\]:
some input
%s" regexp)))))

(defmacro eintest:gene-test-cell-insert-output-pyout-and-display-data (name regexps outputs)
  (declare (indent defun))
  (let ((test-pyout
         (intern (format "ein:cell-insert-output-pyout-%s" name)))
        (test-display-data
         (intern (format "ein:cell-insert-output-display-data-%s" name)))
        (outputs-pyout
         (cl-loop for i from 1
                  for x in outputs
                  collect
                  ;; ein:cell--handle-output doesn't get called
                  ;; so can't use :execution_count here although that is preferable
                  (append x (list :output_type "execute_result" :prompt_number i :metadata nil))))
        (outputs-display-data
         (mapcar (lambda (x) (append '(:output_type "display_data" :metadata nil) x))
                 outputs))
        (regexp-pyout
         (ein:join-str
          ""
          (cl-loop for i from 1
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
  text ("some output") ((:data (:text/plain "some output"))))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  latex
  ("some output \\\\LaTeX")
  ((:data (:application/latex "some output \\LaTeX"))))

(when (image-type-available-p 'svg)
  (eintest:gene-test-cell-insert-output-pyout-and-display-data
   svg
   ("\\.")
   ((:data (:text/plain "some output text" :image/svg+xml ein:testing-example-svg)))))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  html
  ("some output text")
  ((:data (:text/plain "some output text" :text/html "<b>not shown</b>"))))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  javascript
  ("some output text")
  ((:data (:text/plain "some output text" :application/javascript "$.do.something()"))))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  text-two
  ("first output text" "second output text")
  ((:data (:text/plain "first output text")) (:data (:text/plain "second output text"))))

(eintest:gene-test-cell-insert-output-pyout-and-display-data
  text-javascript
  ("first output text" "second output text")
  ((:data (:text/plain "first output text"))
   (:data (:text/plain "second output text" :application/javascript "$.do.something()"))))

(when (image-type-available-p 'svg)
  (eintest:gene-test-cell-insert-output-pyout-and-display-data
   text-latex-svg
   ("first output text" "second output \\\\LaTeX")
   ((:data (:text/plain "first output text"))
    (:data (:application/latex "second output \\LaTeX"))
    (:data (:text/plain "some output text" :image/svg+xml ein:testing-example-svg)))))

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
