(require 'dizzee)
(require 'ein-utils)
(require 'ein-dev)

(defvar eintest:notebook-dir
  (ein:join-path (list ein:source-dir "tests" "notebook")))

(dz-defservice eintest:dz-ipython
               "python"
               :args ((executable-find "ipython")
                      "notebook" "--port=8889" "--debug"
                      (concat "--notebook-dir=" eintest:notebook-dir)
                      "--no-browser"))

;; (eintest:dz-ipython-start)
;; (eintest:dz-ipython-stop)
;; (eintest:dz-ipython-restart)
;; (eintest:dz-ipython-running-p)
