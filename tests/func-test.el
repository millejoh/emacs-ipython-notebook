(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebooklist)

;; Execute `eintest:dz-ipython-start' before starting the following
;; test to setup server.
;; See: ./setup-server.el


(defvar eintest:port 8889)
(defvar eintest:time-before-notebooklist-ready 1)
(defvar eintest:time-before-notebook-new 1)
(defvar eintest:time-before-notebook-ready 1)


(defun eintest:get-notebook-by-name (url-or-port notebook-name)
  (with-current-buffer (ein:notebooklist-open url-or-port nil)
    (sit-for eintest:time-before-notebooklist-ready)
    (sleep-for eintest:time-before-notebooklist-ready)
    (loop for note in (ein:$notebooklist-data ein:notebooklist)
          for name = (plist-get note :name)
          for notebook-id = (plist-get note :notebook_id)
          when (equal name notebook-name)
          return
          (ein:notebook-open
           (ein:$notebooklist-url-or-port ein:notebooklist)
           notebook-id))))

(defun eintest:get-untitled0-or-create (url-or-port)
  (let ((notebook (eintest:get-notebook-by-name url-or-port "Untitled0")))
    (if notebook
        notebook
      (with-current-buffer (ein:notebooklist-open url-or-port nil)
        (sit-for eintest:time-before-notebooklist-ready)
        (sleep-for eintest:time-before-notebooklist-ready)
        (ein:notebooklist-new-notebook))
      (sit-for eintest:time-before-notebook-new)
      (sleep-for eintest:time-before-notebook-new)
      (eintest:get-notebook-by-name url-or-port "Untitled0"))))

(ert-deftest eintest:get-untitled0-or-create ()
  (let ((notebook (eintest:get-untitled0-or-create eintest:port)))
    (sit-for eintest:time-before-notebook-ready)
    (sleep-for eintest:time-before-notebook-ready)
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:notebook) "Untitled0")))))
