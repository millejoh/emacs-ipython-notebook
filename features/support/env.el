(eval-when-compile (require 'cl))
(require 'f)
(require 'espuds)
(require 'ert)

(let* ((support-path (f-dirname load-file-name))
       (root-path (f-parent (f-parent support-path))))
  (add-to-list 'load-path (concat root-path "/lisp"))
  (add-to-list 'load-path (concat root-path "/test")))

(require 'ein-notebooklist)
(require 'ein-jupyter)
(require 'ein-dev)
(require 'ein-testing)
(require 'ein-ipynb-mode)
(require 'ein-contents-api)

(if (member "timestamp" ecukes-include-tags)
    (require 'ein-timestamp)
  (!cons "timestamp" ecukes-exclude-tags))

(unless (member "jupyterhub" ecukes-include-tags)
  (!cons "jupyterhub" ecukes-exclude-tags))

(if (eq system-type 'darwin)
    (!cons "switch" ecukes-exclude-tags))

(defvar ein:testing-jupyter-server-root (f-parent (f-dirname load-file-name)))

(defun ein:testing-after-scenario ()
  (ein:testing-flush-queries)
  (with-current-buffer (ein:notebooklist-get-buffer (car (ein:jupyter-server-conn-info)))
    (if ein:%notebooklist%
        (loop for notebook in (ein:notebook-opened-notebooks)
              for path = (ein:$notebook-notebook-path notebook)
              do (ein:notebook-kill-kernel-then-close-command notebook t)
              do (loop repeat 8
                       until (not (ein:notebook-live-p notebook))
                       do (sleep-for 0 500)
                       finally do (when (ein:notebook-live-p notebook)
                                    (ein:display-warning (format "cannot close %s" path))))
              do (when (search "Untitled" path)
                   (ein:notebooklist-delete-notebook path)
                   (loop repeat 8
                         with fullpath = (concat (file-name-as-directory ein:testing-jupyter-server-root) path)
                         for extant = (file-exists-p fullpath)
                         until (not extant)
                         do (sleep-for 0 500)
                         finally do (when extant 
                                      (ein:display-warning (format "cannot del %s" path))))))))
  (ein:aif (ein:notebook-opened-notebooks)
      (loop for nb in it
            for path = (ein:$notebook-notebook-path nb)
            do (ein:log 'debug "Notebook %s still open" path)
            finally do (assert nil))))

(Setup
 (ein:dev-start-debug)
 (setq ein:notebook-autosave-frequency 0)
 (setq ein:notebook-create-checkpoint-on-save nil)
 (setq ein:testing-dump-file-log (concat default-directory "log/ecukes.log"))
 (setq ein:testing-dump-file-messages (concat default-directory "log/ecukes.messages"))
 (setq ein:testing-dump-file-server  (concat default-directory  "log/ecukes.server"))
 (setq ein:testing-dump-file-request  (concat default-directory "log/ecukes.request"))
 (Given "I start and login to the server configured \"\\n\"")
)

(After
 (ein:testing-after-scenario))

(Teardown
 (Given "I finally stop the server"))

(Fail
 (if noninteractive
     (ein:testing-after-scenario)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
