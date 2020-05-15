;;; ein-testing-kernel.el --- Testing utilities for kernel module  -*- lexical-binding:t -*-

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-testing-kernel.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-testing-kernel.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-testing-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)

(require 'ein-kernel)



;;; Test `ein:kernel-construct-help-string'

(defvar ein:testing-kernel-construct-help-string-pcallsig-list
  '(nil :call_def :init_definition :definition))

(defvar ein:testing-kernel-construct-help-string-pdocstring-list
  '(nil :call_docstring :init_docstring :docstring))

(defun ein:testing-kernel-construct-help-string-test-func (content result)
  (should (equal (ein:kernel-construct-help-string content) result)))

(defun ein:testing-kernel-construct-help-string-loop
  (&optional test pcallsig-list pdocstring-list)
  "Run tests for `ein:kernel-construct-help-string-loop'.

TEST
   A function takes two arguments, namely CONTENT and RESULT.
   CONTENT is the argument to `ein:kernel-construct-help-string' and
   RESULT must match to its returned value.  Use `should' to test
   equality.
PCALLSIG-LIST
   `nil' or (subset of) `ein:testing-kernel-construct-help-string-pcallsig-list'.
PDOCSTRING-LIST
   `nil' or (subset of) `ein:testing-kernel-construct-help-string-pdocstring-list'.

All combinations of PCALLSIG-LIST and PDOCSTRING-LIST are used to
construct CONTENT and RESULT."
  (unless test
    (setq test #'ein:testing-kernel-construct-help-string-test-func))
  (unless pcallsig-list
    (setq pcallsig-list
          ein:testing-kernel-construct-help-string-pcallsig-list))
  (unless pdocstring-list
    (setq pdocstring-list
          ein:testing-kernel-construct-help-string-pdocstring-list))
  (cl-loop with callsig = "function(a=1, b=2, c=d)"
           with docstring = "This function does what."
           for pcallsig in pcallsig-list
           do (cl-loop for pdoc in pdocstring-list
                       for content = (append
                                      (when pcallsig (list pcallsig callsig))
                                      (when pdoc (list pdoc docstring)))
                       for result = (aif (append
                                          (when pcallsig (list callsig))
                                          (when pdoc (list docstring)))
                                        (ein:join-str "\n" it))
                       do (funcall test content result))))

(provide 'ein-testing-kernel)

;;; ein-testing-kernel.el ends here
