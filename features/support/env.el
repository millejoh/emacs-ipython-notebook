(eval-when-compile (require 'cl))
(require 'f)
(require 'espuds)
(require 'ert)
(require 'undo-tree)
(require 'python)
(require 'julia-mode)
(require 'ess-r-mode)
(require 'markdown-mode)

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
(require 'poly-ein)
(require 'ob-ein)

(!cons "timestamp" ecukes-exclude-tags)

(unless (member "jupyterhub" ecukes-include-tags)
  (!cons "jupyterhub" ecukes-exclude-tags))

(when (file-exists-p (concat default-directory "features/support/test-poly.el"))
  (load-file (concat default-directory "features/support/test-poly.el")))

(when ein:polymode
  (!cons "evil" ecukes-exclude-tags))

(cond ((not ein:polymode)
       (!cons "julia" ecukes-exclude-tags)
       (!cons "memory" ecukes-exclude-tags))
      ((string= (getenv "TRAVIS_OS_NAME") "linux")
       (!cons "memory" ecukes-exclude-tags)))

(defvar ein:testing-jupyter-server-root (f-parent (f-dirname load-file-name)))

(defun ein:testing-after-scenario ()
  (ein:testing-flush-queries)
  (with-current-buffer (ein:notebooklist-get-buffer (car (ein:jupyter-server-conn-info)))
    (loop for notebook in (ein:notebook-opened-notebooks)
          for path = (ein:$notebook-notebook-path notebook)
          do (ein:notebook-kill-kernel-then-close-command notebook)
          do (loop repeat 16
                   until (not (ein:notebook-live-p notebook))
                   do (sleep-for 0 1000)
                   finally do (when (ein:notebook-live-p notebook)
                                (ein:display-warning (format "cannot close %s" path))))
          do (when (or (ob-ein-anonymous-p path)
                       (search "Untitled" path)
                       (search "Renamed" path))
               (ein:notebooklist-delete-notebook path)
               (loop repeat 16
                     with fullpath = (concat (file-name-as-directory ein:testing-jupyter-server-root) path)
                     for extant = (file-exists-p fullpath)
                     until (not extant)
                     do (sleep-for 0 1000)
                     finally do (when extant
                                  (ein:display-warning (format "cannot del %s" path)))))))
  (ein:aif (ein:notebook-opened-notebooks)
      (loop for nb in it
            for path = (ein:$notebook-notebook-path nb)
            do (ein:log 'debug "Notebook %s still open" path)
            finally do (assert nil))))

(Setup
 (ein:dev-start-debug)
 (cl-assert (boundp 'company-frontends))
 (custom-set-variables '(company-frontends nil)
                       '(python-indent-guess-indent-offset-verbose nil))
 (setq ein:jupyter-default-kernel
       (loop with cand = ""
             for (k . spec) in
             (alist-get
              'kernelspecs
              (let ((json-object-type 'alist))
                (json-read-from-string
                 (shell-command-to-string
                  (format "%s kernelspec list --json"
                          ein:jupyter-default-server-command)))))
             if (let ((lang (alist-get 'language (alist-get 'spec spec))))
                  (and (string= "python" lang)
                       (string> (symbol-name k) cand)))
             do (setq cand (symbol-name k))
             end
             finally return (intern cand)))
 (setq ein:testing-dump-file-log (concat default-directory "log/ecukes.log"))
 (setq ein:testing-dump-file-messages (concat default-directory "log/ecukes.messages"))
 (setq ein:testing-dump-file-server (concat default-directory  "log/ecukes.server"))
 (setq ein:testing-dump-file-websocket (concat default-directory  "log/ecukes.websocket"))
 (setq ein:testing-dump-file-request  (concat default-directory "log/ecukes.request"))
 (setq org-confirm-babel-evaluate nil)
 (setq transient-mark-mode t)
 (Given "I start and login to the server configured \"\\n\""))

(After
 (ein:testing-after-scenario))

(Teardown
 (Given "I finally stop the server"))

(Fail
 (if noninteractive
     (ein:testing-after-scenario)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
