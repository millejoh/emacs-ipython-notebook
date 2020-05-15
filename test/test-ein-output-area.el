;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-output-area)

(defun ein:testing-insert-html--fix-urls-do-test (source desired)
  (let ((s (ein:xml-parse-html-string source))
        (d (ein:xml-parse-html-string desired)))
    (ein:insert-html--fix-urls s 8888)
    (should (equal s d))))

(defmacro ein:testing-insert-html--fix-urls-deftests (args-list)
  `(progn
     ,@(cl-loop for i from 0
             for args in args-list
             for test = (intern (format "ein:insert-html--fix-urls/%s" i))
             collect
             `(ert-deftest ,test ()
                (ein:testing-insert-html--fix-urls-do-test ,@args)))))

(ein:testing-insert-html--fix-urls-deftests
 (;; Simple replaces
  ("<a href=files/spam>text</a>"
   "<a href=http://127.0.0.1:8888/files/spam>text</a>")
  ("<a href=/files/spam>text</a>"
   "<a href=http://127.0.0.1:8888/files/spam>text</a>")
  ("<img src=files/sample.png />"
   "<img src=http://127.0.0.1:8888/files/sample.png />")
  ("<img src=/files/sample.png />"
   "<img src=http://127.0.0.1:8888/files/sample.png />")
  ;; Do not modify dom in these cases:
  ("<a>text</a>"
   "<a>text</a>")
  ("<a href=http://example>text</a>"
   "<a href=http://example>text</a>")
  ("<img src=http://example />"
   "<img src=http://example />")
  ;; Bit more complicated cases:
  ("<p><a href=files/spam>link</a> normal</p>"
   "<p><a href=http://127.0.0.1:8888/files/spam>link</a> normal</p>")
  ("<img><img src=files/sample.png /> normal</p>"
   "<img><img src=http://127.0.0.1:8888/files/sample.png /> normal</p>")))
