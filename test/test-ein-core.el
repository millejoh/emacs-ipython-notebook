;; -*- lexical-binding:t -*-
(require 'ert)

(require 'tramp)
(require 'ein-core)

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
(ert-deftest ein:filename-translations-from-to-tramp ()
  ;; I really don't understand this https://github.com/magit/with-editor/issues/29
  (cl-loop with ein:filename-translations =
        `((8888 . ,(ein:tramp-create-filename-translator "HOST" "USER")))
        with filename = "/file/name"
        for port in '(7777 8888)    ; check for the one w/o translation
        for emacs-filename = (ein:filename-from-python port filename)
        do (message "emacs-filename = %s" emacs-filename)
        do (should
            (equal (ein:filename-to-python port emacs-filename)
                   filename))))

(ert-deftest ein:filename-translations-to-from-tramp ()
  (cl-loop with ein:filename-translations =
        `((8888 . ,(ein:tramp-create-filename-translator "HOST" "USER")))
        with filename = "/ssh:USER@HOST:/filename"
        for port in '(8888)
        do (should
            (equal (ein:filename-from-python
                    port (ein:filename-to-python port filename))
                   filename))))

(ert-deftest ein:filename-to-python-tramp ()
  (let* ((port 8888)
         (ein:filename-translations
          `((,port . ,(ein:tramp-create-filename-translator "DUMMY")))))
    (cl-loop with python-filename = "/file/name"
          for emacs-filename in '("/scp:HOST:/file/name"
                                  "/ssh:USER@HOST:/file/name")
          do (should
              (equal (ein:filename-to-python port emacs-filename)
                     python-filename)))
    ;; Error: Not a Tramp file name: /file/name
    (should-error (ein:filename-to-python port "/file/name"))))

(ert-deftest ein:filename-from-python-tramp ()
  (cl-loop with ein:filename-translations =
        `((8888 . ,(ein:tramp-create-filename-translator "HOST" "USER")))
        with python-filename = "/file/name"
        for emacs-filename in '("/ssh:USER@HOST:/file/name" "/file/name")
        for port in '(8888 7777)
        do (should
            (equal (ein:filename-from-python port python-filename)
                    emacs-filename))))
