(require 'ein-console)

(ert-deftest ein:console-security-dir-string ()
  (let ((ein:console-security-dir "/some/dir/"))
    (should (equal (ein:console-security-dir-get "DUMMY-URL-OR-PORT")
                   ein:console-security-dir))))

(ert-deftest ein:console-security-dir-list ()
  (let ((ein:console-security-dir
         '((8888 . "/dir/8888/")
           ("htttp://dummy.org" . "/dir/http/")
           (7777 . my-secret-directory)
           (default . "/dir/default/")))
        (my-secret-directory "/dir/secret/"))
    (should (equal (ein:console-security-dir-get 8888) "/dir/8888/"))
    (should (equal (ein:console-security-dir-get "htttp://dummy.org")
                   "/dir/http/"))
    (should (equal (ein:console-security-dir-get 7777) "/dir/secret/"))
    (should (equal (ein:console-security-dir-get 9999) "/dir/default/"))))

(ert-deftest ein:console-security-dir-func ()
  (let ((ein:console-security-dir
         '(lambda (x) (should (equal x "DUMMY-URL-OR-PORT")) "/dir/")))
    (should (equal (ein:console-security-dir-get "DUMMY-URL-OR-PORT")
                   "/dir/"))))
