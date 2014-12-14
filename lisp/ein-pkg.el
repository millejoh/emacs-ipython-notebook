(define-package "ein"
  "0.3.0"
  "Emacs IPython Notebook"
  '((websocket "1.3")
    (request "0.2")
    ;; `auto-complete' is not really a dependency, but who use EIN w/o AC?
    (auto-complete "1.4")))
