(require 'ein-notebooklist)
(require 'ein-testing-notebook)

(defun eintest:notebooklist-make-empty (&optional url-or-port)
  "Make empty notebook list buffer."
  (flet ((ein:query-kernelspecs (url-or-port &optional force-refresh))
         (ein:content-query-sessions (session-hash url-or-port &optional force-sync)))
    (ein:notebooklist-url-retrieve-callback 
     (make-ein:$content :url-or-port (or url-or-port ein:testing-notebook-dummy-url)
                        :ipython-version 3
                        :path ""))))

(defmacro eintest:notebooklist-is-empty-context-of (func)
  `(ert-deftest ,(intern (format "%s--notebooklist" func)) ()
     (with-current-buffer (eintest:notebooklist-make-empty)
       (should-not (,func)))))


;; Generic getter

(ert-deftest ein:get-url-or-port--notebooklist ()
  (with-current-buffer (eintest:notebooklist-make-empty)
    (should (equal (ein:get-url-or-port) ein:testing-notebook-dummy-url))))

(eintest:notebooklist-is-empty-context-of ein:get-notebook)
(eintest:notebooklist-is-empty-context-of ein:get-kernel)
(eintest:notebooklist-is-empty-context-of ein:get-cell-at-point)
(eintest:notebooklist-is-empty-context-of ein:get-traceback-data)
