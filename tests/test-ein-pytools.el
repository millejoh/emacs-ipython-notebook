(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (concat (file-name-directory load-file-name) "mocker")))
(require 'mocker)

(require 'test-ein-kernel)


(ert-deftest ein:pytools-finish-tooltip ()
  (eintest:kernel-construct-help-string-loop
   (lambda (content result)
     (if result
         (mocker-let
             ((featurep
               (feature)
               ((:input '(pos-tip) :output t)))
              (pos-tip-show
               (string &optional tip-color pos window timeout)
               ((:input (list result 'ein:pos-tip-face nil nil 0)))))
           (let ((window-system t))
             (ein:pytools-finish-tooltip '-not-used- content '-not-used-)))
       (mocker-let
           ((featurep (feature) ()))
         (ein:pytools-finish-tooltip '-not-used- content '-not-used-))))))
