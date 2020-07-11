;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein)
(require 'ein-utils)

(ert-deftest ein-url-simple ()
  (should (null (ein:url nil)))
  (should (equal (ein:url 8888) "http://127.0.0.1:8888"))
  (should (equal (ein:url "http://:8000") "http://127.0.0.1:8000"))
  (should (equal (ein:url "http://localhost") "http://127.0.0.1"))
  (should (equal (ein:url "https://localhost:8888") "https://127.0.0.1:8888"))
  (should (equal (ein:url "http://localhost:8000" "" "" "" "Untitled.ipynb") "http://127.0.0.1:8000/Untitled.ipynb"))
  (should (equal (ein:url "http://localhost:8000" "") "http://127.0.0.1:8000"))
  (should (equal (ein:url "http://localhost:8000") "http://127.0.0.1:8000"))
  (should (equal (ein:url "localhost" "foo" "bar") "http://127.0.0.1/foo/bar"))
  (should (equal (ein:url "https://localhost" "foo" "bar") "https://127.0.0.1/foo/bar"))
  (should (equal (ein:url "datasci-1:8888") "https://datasci-1:8888"))
  (should (equal (ein:url "datasci-1" "foo" "bar") "https://datasci-1/foo/bar"))
  (should (equal (ein:glom-paths "" nil "Untitled1.ipynb") "Untitled1.ipynb"))
  (should (equal (ein:glom-paths) ""))
  (cl-loop for url in '("http://127.0.0.1:8888" "http://localhost:8888" "http://127.0.0.1:8888/" "http://localhost:8888/" "8888" 8888)
        do (should (equal (ein:url "http://localhost:8888") (ein:url url)))))

(ert-deftest ein-url-slashes ()
  (cl-loop for a in '("a" "a/" "/a")
        do (cl-loop for b in '("b" "/b")
                 do (should (equal (ein:url 8888 a b)
                                   "http://127.0.0.1:8888/a/b")))
        do (should (equal (ein:url 8888 a "b/")
                          "http://127.0.0.1:8888/a/b"))))

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


;;; Text manipulation on buffer

(ert-deftest ein:find-leftmost-column-simple-cases ()
  (cl-loop for (indent text) in
        '(;; No indent
          (0 "\
def f():
    pass")
          ;; Indented python code
          (4 "\
    def f():
        pass")
          ;; Deeper indent can come first
          (4 "\
        # indent = 8
    # indent 4")
          ;; With empty lines
          (4 "\

        # indent = 8

    # indent 4

")
          )
        do (with-temp-buffer
             (insert text)
             (should (= (ein:find-leftmost-column (point-min) (point-max))
                        indent)))))


;;; Misc

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
  (should (equal (ein:list-move-left '(a b c d) 'd) '(a b d c)))
  (should-error  (ein:list-move-left '(a b c d) 'X)))

(ert-deftest ein:list-move-right ()
  (should (equal (ein:list-move-right '(a) 'a) '(a)))
  (should (equal (ein:list-move-right '(a b) 'a) '(b a)))
  (should (equal (ein:list-move-right '(a b) 'b) '(b a)))
  (should (equal (ein:list-move-right '(a b c d) 'a) '(b a c d)))
  (should (equal (ein:list-move-right '(a b c d) 'b) '(a c b d)))
  (should (equal (ein:list-move-right '(a b c d) 'c) '(a b d c)))
  (should (equal (ein:list-move-right '(a b c d) 'd) '(d a b c)))
  (should-error  (ein:list-move-right '(a b c d) 'X)))

(defvar test-ein-utils-setting)
(defun ein:testing-choose-setting-should-equal (setting value desired &optional single-p)
  (setq test-ein-utils-setting setting)
  (should (equal (ein:choose-setting 'test-ein-utils-setting value single-p) desired)))

(ert-deftest ein:choose-setting-single-string ()
  (let ((test 'ein:testing-choose-setting-should-equal))
    (funcall test "a" nil "a")
    (funcall test "a" 'whatever "a")))

(ert-deftest ein:choose-setting-single-int ()
  (let ((test #'ein:testing-choose-setting-should-equal))
    (funcall test 1 nil 1 #'integerp)
    (funcall test 1 'whatever 1 #'integerp)))

(ert-deftest ein:choose-setting-alist ()
  (let ((test (lambda (&rest args)
                (apply #'ein:testing-choose-setting-should-equal
                       '(("a" . 1) ("b" . 2) ("c" . 3))
                       args))))
    (funcall test "a" 1)
    (funcall test "b" 2)))

(ert-deftest ein:choose-setting-func ()
  (let* ((test (lambda (&rest args)
                 (apply #'ein:testing-choose-setting-should-equal
                        (lambda (_x) 1)
                        args))))
    (funcall test nil 1)
    (funcall test 'whatever 1)))
