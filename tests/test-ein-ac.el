(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-ac)
(require 'ein-testing-kernel)


(ert-deftest ein:ac-set-document ()
  (let ((string "candidate string"))
    (should-not (get-text-property 0 'document string))
    (ein:testing-kernel-construct-help-string-loop
     (lambda (content result)
       (ein:ac-set-document string content '-not-used-)
       (let ((props (text-properties-at 0 string)))
         ;; document property may be nil, but must be set.
         (should (member 'document props))
         (should (equal (plist-get props 'document) result)))))))
