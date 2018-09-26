(require 'f)
(require 'cl)
(require 'espuds)
(require 'ert)

(let* ((support-path (f-dirname load-file-name))
       (root-path (f-parent (f-parent support-path))))
  (add-to-list 'load-path (concat root-path "/lisp"))
  (add-to-list 'load-path (concat root-path "/test")))

(require 'ein-loaddefs)
(require 'ein-notebooklist)
(require 'ein-jupyter)
(require 'ein-dev)
(require 'ein-testing)

(defvar ein:testing-jupyter-server-root (f-parent (f-dirname load-file-name)))
(ein:deflocal ein:%testing-port% nil)

(defun ein:testing-wait-until (predicate &optional predargs ms)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait."
  (let* ((subms 300)
         (count (max 1 (if ms (truncate (/ ms subms)) 25))))
    (unless (loop repeat count
                  when (apply predicate predargs)
                  return t
                  do (sleep-for 0 subms))
      (error "Timeout: %s" predicate))))

(Setup
 (setq ein:force-sync t)
 (ein:dev-start-debug)
 (setq ein:notebook-autosave-frequency 10000)
 (setq ein:testing-dump-file-log "./log/ecukes.log")
 (setq ein:testing-dump-file-messages "./log/ecukes.messages")
 (setq ein:testing-dump-server-log  "./log/ecukes.server")

 (setq ein:jupyter-server-args '("--no-browser" "--debug"))
 (deferred:sync! (ein:jupyter-server-start (executable-find "jupyter") ein:testing-jupyter-server-root))
 (assert (processp %ein:jupyter-server-session%) t "notebook server defunct")
 (setq ein:%testing-url% (car (ein:jupyter-server-conn-info))
))

(After
 (with-current-buffer (ein:notebooklist-get-buffer ein:%testing-url%)
   (loop for buffer in (ein:notebook-opened-buffers)
         do (let ((kill-buffer-query-functions nil))
              (with-current-buffer buffer (not-modified))
              (kill-buffer buffer)))
   (let ((sessions #s(hash-table test equal data (:pending t)))
         (urlport (ein:$notebooklist-url-or-port ein:%notebooklist%)))
     (ein:content-query-sessions sessions urlport)
     (loop repeat 4
           until (null (gethash :pending sessions))
           do (sleep-for 0 50))
     (loop for note in (ein:$notebooklist-data ein:%notebooklist%)
           for path = (plist-get note :path)
           for notebook = (ein:notebook-get-opened-notebook urlport path)
           if (not (null notebook))
             do (ein:notebook-kill-kernel-then-close-command notebook t)
                (if (search "Untitled" path) 
                    (ein:notebooklist-delete-notebook path))
           end))))

(Teardown
 (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
   (ein:jupyter-server-stop t))
 (ein:testing-dump-logs)
 (assert (not (processp %ein:jupyter-server-session%)) t "notebook server orphaned"))

(Fail
 (if (not noninteractive)
     (keyboard-quit))) ;; useful to prevent emacs from quitting
