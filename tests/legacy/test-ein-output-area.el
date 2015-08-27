(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-output-area)

(defun ein:testing-insert-html--fix-urls-do-test (source desired)
  (setq source (ein:xml-parse-html-string source))
  (setq desired (ein:xml-parse-html-string desired))
  (ein:insert-html--fix-urls source 8888)
  (should (equal source desired)))

(defmacro ein:testing-insert-html--fix-urls-deftests (args-list)
  `(progn
     ,@(loop for i from 0
             for args in args-list
             for test = (intern (format "ein:insert-html--fix-urls/%s" i))
             collect
             `(ert-deftest ,test ()
                (ein:testing-insert-html--fix-urls-do-test ,@args)))))

(when (require 'shr nil t)
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
     "<img><img src=http://127.0.0.1:8888/files/sample.png /> normal</p>")
    )))
