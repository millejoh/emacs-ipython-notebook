(define-package "ein2"
  "2.0"
  "Emacs IPython Notebook with support for IPython 2.x and beyond."
  '((websocket "1.3")
    (request "0.2")
    ;; `auto-complete' is not really a dependency, but who use EIN w/o AC?
    (auto-complete "1.4")))
