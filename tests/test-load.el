;; Load all test-ein-*.el files for interactive/batch use.

;; Usage:
;;   emacs -Q -batch -L ... -l tests/test-load.el -f ert-run-tests-batch
;; You will need to set load paths using `-L' switch.

(require 'ein-dev)
(require 'ein-testing)

(ein:setq-if-not ein:testing-dump-file-log "test-batch-log.log")
(ein:setq-if-not ein:testing-dump-file-messages "test-batch-messages.log")
(setq message-log-max t)


(ein:load-files "^test-ein-.*\\.el$"
                (file-name-directory load-file-name)
                t)                      ; ignore-compiled
