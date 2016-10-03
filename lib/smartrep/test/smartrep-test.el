;;; smartrep-test.el --- Test smartrep.el

;; Copyright (C) 2014 by myuhe all rights reserved.

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/smartrep.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

(require 'ert)
(require 'undercover)
(undercover "smartrep.el")

(require 'smartrep)

(defun smartrep-test-func (&optional arg)
  (or arg 1))

(defun smartrep-test-command ()
  (interactive)
  ;; NOTE: In noninteractive(batch) mode,
  ;; (called-interactively-p 'interactive) never return `t'
  (if (called-interactively-p 'interactive) 2 1))

(ert-deftest smartrep-unquote ()
  "Tests for `smartrep-unquote'"
  (should (eq (smartrep-unquote '(quote hoge)) 'hoge))
  (should (eq (smartrep-unquote '(function hoge)) 'hoge))
  (should (eq (smartrep-unquote 'hoge) 'hoge)))

(ert-deftest smartrep-extract-fun ()
  "Tests for `smartrep-extract-fun'"
  (should (= (smartrep-extract-fun ?a '(("a" . smartrep-test-func))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . (lambda () (smartrep-test-func))))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . (smartrep-test-func)))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . (smartrep-test-func 2)))) 2))
  (unless noninteractive
    (should (= (smartrep-extract-fun ?a '(("a" . smartrep-test-command))) 2))))

(ert-deftest smartrep-extract-fun-with-quote ()
  "Test for `smartrep-extract-fun' with quote"
  (should (= (smartrep-extract-fun ?a '(("a" . 'smartrep-test-func))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . '(lambda () (smartrep-test-func))))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . #'(lambda () (smartrep-test-func))))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . '(smartrep-test-func)))) 1))
  (should (= (smartrep-extract-fun ?a '(("a" . '(smartrep-test-func 2)))) 2))
  (unless noninteractive
    (should (= (smartrep-extract-fun ?a '(("a" . 'smartrep-test-command))) 2))))

;;; smartrep-test.el end here
