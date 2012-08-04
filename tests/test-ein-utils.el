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

(ert-deftest ein:version ()
  "Check if `ein:version' can be parsed by `version-to-list'."
  (version-to-list ein:version))


;;; File name translation

;; Requiring `tramp' during (inside of) tests yields error from
;; MuMaMo.  Although I don't understand the reason, requiring it
;; before running tests workarounds this problem.
(require 'tramp)

(ert-deftest ein:filename-translations-from-to-tramp ()
  (loop with ein:filename-translations =
        `((8888 . ,(ein:tramp-create-filename-translator "HOST" "USER")))
        with filename = "/file/name"
        for port in '(7777 8888)    ; check for the one w/o translation
        for emacs-filename = (ein:filename-from-python port filename)
        do (message "emacs-filename = %s" emacs-filename)
        do (should
            (equal (ein:filename-to-python port emacs-filename)
                   filename))))

(ert-deftest ein:filename-translations-to-from-tramp ()
  (loop with ein:filename-translations =
        `((8888 . ,(ein:tramp-create-filename-translator "HOST" "USER")))
        with filename = "/USER@HOST:/filename"
        for port in '(8888)
        do (should
            (equal (ein:filename-from-python
                    port (ein:filename-to-python port filename))
                   filename))))

(ert-deftest ein:filename-to-python-tramp ()
  (let* ((port 8888)
         (ein:filename-translations
          `((,port . ,(ein:tramp-create-filename-translator "DUMMY")))))
    (loop with python-filename = "/file/name"
          for emacs-filename in '("/scpc:HOST:/file/name"
                                  "/USER@HOST:/file/name")
          do (should
              (equal (ein:filename-to-python port emacs-filename)
                     python-filename)))
    ;; Error: Not a Tramp file name: /file/name
    (should-error (ein:filename-to-python port "/file/name"))))

(ert-deftest ein:filename-from-python-tramp ()
  (loop with ein:filename-translations =
        `((8888 . ,(ein:tramp-create-filename-translator "HOST" "USER")))
        with python-filename = "/file/name"
        for emacs-filename in '("/USER@HOST:/file/name" "/file/name")
        for port in '(8888 7777)
        do (should
            (equal (ein:filename-from-python port python-filename)
                    emacs-filename))))
