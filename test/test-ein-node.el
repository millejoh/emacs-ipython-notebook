;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-node)

(defun ein:testing-node-dummy-ewco-node (data)
  `[nil nil ,data])

(defun ein:testing-node-ewoc-data (ewoc-node)
  (ein:$node-data (ewoc-data ewoc-node)))

(ert-deftest ein:testing-node-dummy-ewco-node ()
  (let* ((obj "some-object")
         (ewoc-node (ein:testing-node-dummy-ewco-node obj)))
    (should (eq (ewoc-data ewoc-node) obj))))

(ert-deftest ein:node-filter-is ()
  (let ((en-list (mapcar
                  #'ein:testing-node-dummy-ewco-node
                  (list (ein:node-new nil "s" '(spam sag))
                        (ein:node-new nil "p" '(spam))
                        (ein:node-new nil "e" '(egg))
                        (ein:node-new nil "a" '(spam sag))
                        (ein:node-new nil "g" '(egg sag))
                        (ein:node-new nil "m" '(spam))
                        (ein:node-new nil "g" '(egg))))))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'spam))
                   '("s" "p" "a" "m")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'egg))
                   '("e" "g" "g")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'sag))
                   '("s" "a" "g")))))

(ert-deftest ein:node-filter-not ()
  (let ((en-list (mapcar
                  #'ein:testing-node-dummy-ewco-node
                  (list (ein:node-new nil "s" '(spam sag))
                        (ein:node-new nil "p" '(spam))
                        (ein:node-new nil "e" '(egg))
                        (ein:node-new nil "a" '(spam sag))
                        (ein:node-new nil "g" '(egg sag))
                        (ein:node-new nil "m" '(spam))
                        (ein:node-new nil "g" '(egg))))))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :not 'spam))
                   '("e" "g" "g")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :not 'egg))
                   '("s" "p" "a" "m")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :not 'sag))
                   '("p" "e" "m" "g")))))

(ert-deftest ein:node-filter-is-and-not ()
  (let ((en-list (mapcar
                  #'ein:testing-node-dummy-ewco-node
                  (list (ein:node-new nil "s" '(spam sag))
                        (ein:node-new nil "p" '(spam))
                        (ein:node-new nil "e" '(egg))
                        (ein:node-new nil "a" '(spam sag))
                        (ein:node-new nil "g" '(egg sag))
                        (ein:node-new nil "m" '(spam))
                        (ein:node-new nil "g" '(egg))))))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :not 'spam :is 'sag))
                   '("g")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'sag :not 'spam))
                   '("g")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'spam :is 'sag))
                   '("s" "a")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'sag :not 'spam
                                            :not 'not-existing))
                   '("g")))
    (should (equal (mapcar #'ein:testing-node-ewoc-data
                           (ein:node-filter en-list :is 'sag :is 'spam))
                   '("s" "a")))))
