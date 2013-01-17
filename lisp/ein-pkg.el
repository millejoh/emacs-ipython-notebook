(define-package "ein"
  "0.2.0alpha0"
  "Emacs IPython Notebook"
  '((websocket "0.9")
    (request "0.2")
    ;; `auto-complete' is not really a dependency, but who use EIN w/o AC?
    (auto-complete "1.4")))
