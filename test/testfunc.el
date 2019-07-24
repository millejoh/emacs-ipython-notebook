(prefer-coding-system 'utf-8)

(require 'ein-dev)
(require 'ein-testing)
(require 'ein-jupyter)
(require 'ein-notebooklist)
(require 'deferred)

(ein:log 'info "Starting jupyter notebook server.")

(defvar *ein:testing-jupyter-server-command* (or (getenv "JUPYTER_TESTING_COMMAND")
                                                 (executable-find "jupyter"))
  "Path to command that starts the jupyter notebook server.")

(defvar *ein:testing-jupyter-server-directory*  (or (getenv "JUPYTER_TESTING_DIR") (concat default-directory "test"))
  "Location where to start the jupyter notebook server.")

(setq ein:testing-dump-file-log (concat default-directory "log/testfunc.log"))
(setq ein:testing-dump-file-messages (concat default-directory "log/testfunc.messages"))
(setq ein:testing-dump-file-server  (concat default-directory  "log/testfunc.server"))
(setq ein:testing-dump-file-websocket (concat default-directory  "log/testfunc.websocket"))
(setq ein:testing-dump-file-request  (concat default-directory "log/testfunc.request"))
(with-eval-after-load "python"
  (setq python-indent-guess-indent-offset-verbose nil))
(ein:dev-start-debug)
(ein:jupyter-server-start *ein:testing-jupyter-server-command* *ein:testing-jupyter-server-directory*)
(ein:testing-wait-until (lambda () (ein:notebooklist-list)) nil 15000 1000)
(defvar *ein:testing-port* (car (ein:jupyter-server-conn-info)))
(fset 'y-or-n-p (lambda (prompt) nil))
