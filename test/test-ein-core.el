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
