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

(defun ein:testing-after-scenario ()
 (with-current-buffer (ein:notebooklist-get-buffer ein:%testing-url%)
   (loop for buffer in (ein:notebook-opened-buffers)
         do (let ((kill-buffer-query-functions nil))
              (with-current-buffer buffer (not-modified))
              (kill-buffer buffer)))
   (let ((urlport (ein:$notebooklist-url-or-port ein:%notebooklist%)))
     (loop for note in (ein:$notebooklist-data ein:%notebooklist%)
           for path = (plist-get note :path)
           for notebook = (ein:notebook-get-opened-notebook urlport path)
           if (not (null notebook))
             do (ein:notebook-kill-kernel-then-close-command notebook t)
                (if (search "Untitled" path)
                    (ein:notebooklist-delete-notebook path))
           end)))
)
(Setup
 (ein:dev-start-debug)
 (setq ein:notebook-autosave-frequency 0)
 (setq ein:populate-hierarchy-on-notebooklist-open t)
 (setq ein:testing-dump-file-log (concat default-directory "log/ecukes.log"))
 (setq ein:testing-dump-file-messages (concat default-directory "log/ecukes.messages"))
 (setq ein:testing-dump-file-server  (concat default-directory  "log/ecukes.server"))
 (setq ein:testing-dump-file-request  (concat default-directory "log/ecukes.request"))
 (setq ein:jupyter-server-args '("--no-browser" "--debug"))
 (setq ein:%testing-url% nil)
 (deferred:sync! (ein:jupyter-server-start (executable-find "jupyter") ein:testing-jupyter-server-root))
 (assert (processp %ein:jupyter-server-session%) t "notebook server defunct")
 (setq ein:%testing-url% (car (ein:jupyter-server-conn-info))))

(After
 (ein:testing-after-scenario))

(Teardown
 (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
   (ein:jupyter-server-stop t))
; (ein:testing-dump-logs) ; taken care of by ein-testing.el kill-emacs-hook?
 (assert (not (processp %ein:jupyter-server-session%)) t "notebook server orphaned"))

(Fail
 (if noninteractive
     (ein:testing-after-scenario)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
