(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein)  ; for `ein:version'
(require 'ein-utils)

(ert-deftest ein-url-simple ()
  (should (equal (ein:url 8888) "http://127.0.0.1:8888"))
  (should (equal (ein:url "http://localhost") "http://localhost")))

(ert-deftest ein-url-slashes ()
  (loop for a in '("a" "a/" "/a")
        do (loop for b in '("b" "/b")
                 do (should (equal (ein:url 8888 a b)
                                   "http://127.0.0.1:8888/a/b")))
        do (should (equal (ein:url 8888 a "b/")
                          "http://127.0.0.1:8888/a/b/"))))

(ert-deftest ein-trim-simple ()
  (should (equal (ein:trim "a") "a"))
  (should (equal (ein:trim "  a  ") "a"))
  (should (equal (ein:trim "\na\n") "a")))

(ert-deftest ein-trim-middle-spaces ()
  (should (equal (ein:trim "a  b") "a  b"))
  (should (equal (ein:trim "  a  b  ") "a  b"))
  (should (equal (ein:trim "\na  b\n") "a  b")))

(ert-deftest ein-trim-left-simple ()
  (should (equal (ein:trim-left "a") "a"))
  (should (equal (ein:trim-left "  a  ") "a  "))
  (should (equal (ein:trim-left "\na\n") "a\n")))

(ert-deftest ein-trim-right-simple ()
  (should (equal (ein:trim-right "a") "a"))
  (should (equal (ein:trim-right "  a  ") "  a"))
  (should (equal (ein:trim-right "\na\n") "\na")))

(ert-deftest ein:trim-indent-empty ()
  (should (equal (ein:trim-indent "") "")))

(ert-deftest ein:trim-indent-one-line ()
  (should (equal (ein:trim-indent "one line") "one line")))

(ert-deftest ein:trim-indent-one-newline ()
  (should (equal (ein:trim-indent "one line\n") "one line\n")))

(ert-deftest ein:trim-indent-multi-lines-no-trim ()
  (let ((original "\
def func():
    pass
")
        (trimmed "\
def func():
    pass
"))
    (should (equal (ein:trim-indent original) trimmed))))

(ert-deftest ein:trim-indent-multi-lines-one-trim ()
  (let ((original "\
    def func():
        pass
")
        (trimmed "\
def func():
    pass
"))
    (should (equal (ein:trim-indent original) trimmed))))

(ert-deftest ein:trim-indent-multi-lines-with-empty-lines ()
  (let ((original "\

    def func():
        pass

")
        (trimmed "\

def func():
    pass

"))
    (should (equal (ein:trim-indent original) trimmed))))
