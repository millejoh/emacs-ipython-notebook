(require 'ein-notebooklist)

(defun eintest:notebooklist-make-empty (&optional url-or-port)
  "Make empty notebook list buffer."
  (ein:notebooklist-url-retrieve-callback (or url-or-port "DUMMY-URL")))

(defmacro eintest:notebooklist-is-empty-context-of (func)
  `(ert-deftest ,(intern (format "%s--notebooklist" func)) ()
     (with-current-buffer (eintest:notebooklist-make-empty)
       (should-not (,func)))))


;; Generic getter

(ert-deftest ein:get-url-or-port--notebooklist ()
  (with-current-buffer (eintest:notebooklist-make-empty "DUMMY-URL")
    (should (equal (ein:get-url-or-port) "DUMMY-URL"))))

(eintest:notebooklist-is-empty-context-of ein:get-notebook)
(eintest:notebooklist-is-empty-context-of ein:get-kernel)
(eintest:notebooklist-is-empty-context-of ein:get-cell-at-point)
(eintest:notebooklist-is-empty-context-of ein:get-traceback-data)
