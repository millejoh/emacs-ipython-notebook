(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-core)



;;; `ein:version'

(ert-deftest ein:version ()
  "Check if `ein:version' can be parsed by `version-to-list'."
  (version-to-list ein:version))

(ert-deftest ein:version-func-prefix-is-the-variable ()
  (should (string-prefix-p ein:version (ein:version)))
  (let ((default-directory "/tmp/"))
    (should (string-prefix-p ein:version (ein:version)))))

(ert-deftest ein:version-func-outside-of-git-repo ()
  (flet ((ein:git-root-p (dir) nil))
    (should (equal (ein:version) ein:version)))
  (flet ((ein:git-revision-dirty () nil))
    (should (equal (ein:version) ein:version))))



;; Generic getter

(defmacro eintest:generic-getter-should-return-nil (func)
  "In an \"empty\" context, generic getter should return nil."
  `(ert-deftest ,(intern (format "%s--nil name" func)) ()
     (with-temp-buffer
       (should (not (,func))))))

(eintest:generic-getter-should-return-nil ein:get-url-or-port)
(eintest:generic-getter-should-return-nil ein:get-notebook)
(eintest:generic-getter-should-return-nil ein:get-kernel)
(eintest:generic-getter-should-return-nil ein:get-cell-at-point)
(eintest:generic-getter-should-return-nil ein:get-traceback-data)



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
