;; Load all test-ein-*.el files for interactive/batch use.

;; Usage:
;;   emacs -Q -batch -L ... -l tests/test-load.el -f ert-run-tests-batch
;; You will need to set load paths using `-L' switch.

(prefer-coding-system 'utf-8)

(require 'ein-dev)
(require 'ein-testing)
(require 'ein-jupyter)
(require 'deferred)

(ein:log 'info "Starting jupyter notebook server.")

(defvar *ein:testing-jupyter-server-command* (or (getenv "JUPYTER_TESTING_COMMAND")
                                                 (executable-find "jupyter"))
  "Path to command that starts the jupyter notebook server.")

(defvar *ein:testing-jupyter-server-directory*  (or (getenv "JUPYTER_TESTING_DIR") (concat default-directory "test"))
  "Location where to start the jupyter notebook server.")

(defvar *ein:testing-port* nil)
(defvar *ein:testing-token* nil)

(ein:setq-if-not ein:testing-dump-file-log "test-batch-log.log")
(ein:setq-if-not ein:testing-dump-file-messages "test-batch-messages.log")
(setq message-log-max t)
(setq ein:force-sync t)

(ein:log 'info "Staring local jupyter notebook server.")

(setq ein:jupyter-server-args '("--no-browser" "--debug"))

(deferred:sync! (ein:jupyter-server-start *ein:testing-jupyter-server-command* *ein:testing-jupyter-server-directory*))
(multiple-value-bind (url token) (ein:jupyter-server-conn-info)
  (ein:log 'info (format "testing-start-server url: %s, token: %s" url token))
  (setq *ein:testing-port* url)
  (setq *ein:testing-token* token)
  (ein:log 'info "testing-start-server succesfully logged in."))

(ein:load-files "^test-ein-.*\\.el$"
                "./" ;(file-name-directory load-file-name)
                t)                      ; ignore-compiled
