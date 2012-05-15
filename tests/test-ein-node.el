(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-node)

(defmacro eintest:with-ewoc-mock (&rest body)
  (declare (indent 0))
  `(flet ((ewoc-data (x) x))
     ,@body))

(ert-deftest ein:node-filter-is ()
  (eintest:with-ewoc-mock
    (let ((en-list (list (ein:node-new nil "s" '(spam sag))
                         (ein:node-new nil "p" '(spam))
                         (ein:node-new nil "e" '(egg))
                         (ein:node-new nil "a" '(spam sag))
                         (ein:node-new nil "g" '(egg sag))
                         (ein:node-new nil "m" '(spam))
                         (ein:node-new nil "g" '(egg)))))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'spam))
                     '("s" "p" "a" "m")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'egg))
                     '("e" "g" "g")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'sag))
                     '("s" "a" "g"))))))

(ert-deftest ein:node-filter-not ()
  (eintest:with-ewoc-mock
    (let ((en-list (list (ein:node-new nil "s" '(spam sag))
                         (ein:node-new nil "p" '(spam))
                         (ein:node-new nil "e" '(egg))
                         (ein:node-new nil "a" '(spam sag))
                         (ein:node-new nil "g" '(egg sag))
                         (ein:node-new nil "m" '(spam))
                         (ein:node-new nil "g" '(egg)))))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :not 'spam))
                     '("e" "g" "g")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :not 'egg))
                     '("s" "p" "a" "m")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :not 'sag))
                     '("p" "e" "m" "g"))))))

(ert-deftest ein:node-filter-is-and-not ()
  (eintest:with-ewoc-mock
    (let ((en-list (list (ein:node-new nil "s" '(spam sag))
                         (ein:node-new nil "p" '(spam))
                         (ein:node-new nil "e" '(egg))
                         (ein:node-new nil "a" '(spam sag))
                         (ein:node-new nil "g" '(egg sag))
                         (ein:node-new nil "m" '(spam))
                         (ein:node-new nil "g" '(egg)))))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :not 'spam :is 'sag))
                     '("g")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'sag :not 'spam))
                     '("g")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'spam :is 'sag))
                     '("s" "a")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'sag :not 'spam
                                              :not 'not-existing))
                     '("g")))
      (should (equal (mapcar #'ein:$node-data
                             (ein:node-filter en-list :is 'sag :is 'spam))
                     '("s" "a"))))))
