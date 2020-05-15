;; -*- lexical-binding:t -*-
(custom-set-variables '(python-indent-guess-indent-offset-verbose nil)
                      '(ein:jupyter-use-containers nil)
                      '(ein:output-area-inlined-images t))

(require 'python)
(require 'ein-dev)
(require 'ein-testing)

(ein:setq-if-not ein:testing-dump-file-log "./log/testein.log")
(ein:setq-if-not ein:testing-dump-file-messages "./log/testein.messages")

(setq message-log-max t)
