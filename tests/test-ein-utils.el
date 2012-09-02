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

(ert-deftest ein:list-insert-after ()
  (should (equal (ein:list-insert-after '(a) 'a 'X) '(a X)))
  (should (equal (ein:list-insert-after '(a b c) 'a 'X) '(a X b c)))
  (should (equal (ein:list-insert-after '(a b c) 'b 'X) '(a b X c)))
  (should (equal (ein:list-insert-after '(a b c) 'c 'X) '(a b c X)))
  (should-error  (ein:list-insert-after '(a b c) 'd 'X)))

(ert-deftest ein:list-insert-before ()
  (should (equal (ein:list-insert-before '(a) 'a 'X) '(X a)))
  (should (equal (ein:list-insert-before '(a b c) 'a 'X) '(X a b c)))
  (should (equal (ein:list-insert-before '(a b c) 'b 'X) '(a X b c)))
  (should (equal (ein:list-insert-before '(a b c) 'c 'X) '(a b X c)))
  (should-error  (ein:list-insert-before '(a b c) 'd 'X)))

(ert-deftest ein:list-move-left ()
  (should (equal (ein:list-move-left '(a) 'a) '(a)))
  (should (equal (ein:list-move-left '(a b) 'a) '(b a)))
  (should (equal (ein:list-move-left '(a b) 'b) '(b a)))
  (should (equal (ein:list-move-left '(a b c d) 'a) '(b c d a)))
  (should (equal (ein:list-move-left '(a b c d) 'b) '(b a c d)))
  (should (equal (ein:list-move-left '(a b c d) 'c) '(a c b d)))
  (should (equal (ein:list-move-left '(a b c d) 'd) '(a b d c))))
