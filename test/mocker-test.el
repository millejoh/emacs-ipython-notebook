;;; mocker-tests.el --- tests for mocker.el

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, testing

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains tests for mocker.el

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'mocker)

(eval-and-compile
  (when (null (ignore-errors (require 'ert)))
    (defmacro* ert-deftest (name () &body docstring-keys-and-body)
      (message "Skipping tests, ERT is not available"))))

(ert-deftest mocker-let-basic ()
  (should
   (eq t (mocker-let () t))))

(ert-deftest mocker-let-single ()
  (should
   (eq t
       (mocker-let ((foo () :records ((:output t))))
         (foo)))))

(ert-deftest mocker-let-single-implicit-records ()
  (should
   (eq t
       (mocker-let ((foo ()
                         ((:output t))))
         (foo)))))

(ert-deftest mocker-let-no-record ()
  (should-error
   (mocker-let ((foo () :records ()))
     (foo))
   :type 'mocker-mock-error))

(ert-deftest mocker-let-multiple ()
  (should
   (eq 42
       (mocker-let ((foo () :records ((:output 6)))
                    (bar () :records ((:output 7))))
         (* (foo) (bar))))))

(ert-deftest mocker-let-nested ()
  (should
   (eq 42
       (mocker-let ((foo () :records ((:output 6))))
         (mocker-let ((bar () :records ((:output 7))))
           (* (foo) (bar)))))))

(ert-deftest mocker-let-multiple-inputs ()
  (should
   (eq 42
       (mocker-let ((foo (x) :records ((:input '(1) :output 6)))
                    (bar (x) :records ((:input '(2) :output 7))))
         (* (foo 1) (bar 2))))))

(ert-deftest mocker-let-multiple-inputs-invalid ()
  (should-error
   (mocker-let ((foo (x) :records ((:input '(1) :output 6)))
                (bar (x) :records ((:input '(2) :output 7))))
     (* (foo 2) (bar 2)))
   :type 'mocker-record-error))

(ert-deftest mocker-let-single-input-matcher ()
  (should
   (eq t
       (mocker-let ((foo (x)
                         :records ((:input-matcher 'integerp :output t))))
         (foo 4)))))

(ert-deftest mocker-let-single-input-matcher-invalid ()
  (should-error
   (mocker-let ((foo (x)
                     :records ((:input-matcher 'integerp :output t))))
     (foo t))
   :type 'mocker-record-error))

(ert-deftest mocker-let-multiple-output-generator ()
  (should
   (eq 2
       (mocker-let ((foo (x)
                         :records ((:input '(2)
                                    :output-generator (function identity))
                                   (:input '(4)
                                    :output-generator (lambda (x) 0)))))
         (+ (foo 2) (foo 4))))))

(ert-deftest mocker-let-multiple-calls-min ()
  (should
   (eq 4
       (mocker-let ((foo (x)
                         :records ((:input '(2)
                                    :output-generator (function identity)
                                    :min-occur 2))))
         (+ (foo 2) (foo 2))))))

(ert-deftest mocker-let-multiple-calls-illimited ()
  (should
   (eq 8
       (mocker-let ((foo (x)
                         :records ((:input '(2)
                                    :output-generator (function identity)
                                    :max-occur nil))))
         (+ (foo 2) (foo 2) (foo 2) (foo 2))))))

(ert-deftest mocker-let-multiple-calls-exact ()
  (should
   (eq 8
       (mocker-let ((foo (x)
                         :records ((:input '(2)
                                    :output-generator (function identity)
                                    :occur 4))))
         (+ (foo 2) (foo 2) (foo 2) (foo 2))))))

(ert-deftest mocker-let-multiple-calls-multiple-records ()
  (should
   (eq 12
       (mocker-let ((foo (x)
                         :records ((:input '(2)
                                    :output-generator (function identity)
                                    :max-occur 2)
                                   (:input '(2)
                                    :output-generator (lambda (x) (* 2 x))
                                    :occur 2))))
         (+ (foo 2) (foo 2) (foo 2) (foo 2))))))

(ert-deftest mocker-let-multiple-calls-multiple-same-records ()
  (should
   (eq 8
       (mocker-let ((foo (x)
                         :records ((:input '(2)
                                    :output-generator (function identity)
                                    :max-occur 2)
                                   (:input '(2)
                                    :output-generator (function identity)
                                    :max-occur 2))))
         (+ (foo 2) (foo 2) (foo 2) (foo 2))))))

(ert-deftest mocker-let-multiple-calls-unexpected ()
  (should-error
   (mocker-let ((foo (x)
                     :records ((:input '(2)
                                :output-generator (function identity)
                                :max-occur 2))))
     (+ (foo 2) (foo 2) (foo 2) (foo 2))))
  :type 'mocker-record-error)

(ert-deftest mocker-let-multiple-calls-unexpected-exact ()
  (should-error
   (mocker-let ((foo (x)
                     :records ((:input '(2)
                                :output-generator (function identity)
                                :occur 2))))
     (+ (foo 2) (foo 2) (foo 2) (foo 2))))
  :type 'mocker-record-error)

(ert-deftest mocker-let-stub-simple ()
  (should
   (let ((mocker-mock-default-record-cls 'mocker-stub-record))
     (eq t
         (mocker-let ((foo (x) :records ((:output t))))
           (and (foo 1) (foo 42) (foo 666)))))))

(ert-deftest mocker-let-stub-simple-explicit ()
  (should
   (eq t
       (mocker-let ((foo (x) :records ((:record-cls mocker-stub-record
                                                    :output t))))
         (and (foo 1) (foo 42) (foo 666))))))

(ert-deftest mocker-let-stub-limited ()
  (should-error
   (let ((mocker-mock-default-record-cls 'mocker-stub-record))
     (mocker-let ((foo (x) :records ((:output t :max-occur 2))))
       (and (foo 1) (foo 42) (foo 666))))
   :type 'mocker-mock-error))

(ert-deftest mocker-let-multiple-calls-unordered ()
  (should
   (eq 18
       (mocker-let ((foo (x y z)
                         :ordered nil
                         ((:input '(1 2 3) :output 4)
                          (:input '(4 5 6) :output 10)))
                    (bar (x)
                         ((:input '(42) :output 4))))
         (+ (foo 4 5 6)
            (foo 1 2 3)
            (bar 42))))))

(ert-deftest mocker-passthrough-basic ()
  (should
   (not
    (mocker-let ((ignore (x)
                         :records ((:record-cls mocker-passthrough-record
                                                :input '(42)))))
      (ignore 42)))))

(ert-deftest mocker-passthrough-mixed ()
  (should
   (mocker-let ((ignore (x)
                        :records ((:record-cls mocker-passthrough-record
                                               :input '(42))
                                  (:input '(58) :output t))))
     (or (ignore 42)
         (ignore 58)))))

(ert-deftest mocker-passthrough-mixed-error ()
  (should-error
   (mocker-let ((ignore (x)
                        :records ((:record-cls mocker-passthrough-record
                                               :input '(42))
                                  (:input '(58) :output t))))
     (or (ignore 42)
         (ignore 42)))
   :type 'mocker-record-error))

(ert-deftest mocker-passthrough-multiple ()
  (should
   (mocker-let ((ignore (x)
                        ((:input-matcher (lambda (x) t)
                                         :output t :max-occur 2)
                         (:record-cls mocker-passthrough-record
                                      :input '(42) :max-occur nil))))
     (and (ignore 1)
          (ignore 2)
          (not (or
                (ignore 42) (ignore 42) (ignore 42) (ignore 42)))))))

(ert-deftest mocker-inhibit-mock-not-consumed ()
  (should-error
   (let ((mocker-inhibit t))
     (mocker-let ((ignore (x)
                          ((:input '(42) :output t))))
       (ignore 42)))
   :type 'mocker-record-error))

(ert-deftest mocker-inhibit-mocking ()
  (should
   (not
    (mocker-let ((ignore (x)
                         ((:input '(42) :output t))))
      (and (ignore 42)
           (let ((mocker-inhibit t))
             (ignore 42)))))))

(ert-deftest mocker-rest-args ()
  (should
   (mocker-let ((f (a b &rest args) ((:input '(1 2 3 4) :output t))))
     (f 1 2 3 4))))

(provide 'mocker-tests)
;;; mocker-tests.el ends here
