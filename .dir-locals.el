;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (require-final-newline . t)
  ;; not tabs in code
  (indent-tabs-mode)
  ;; checkdoc, one space is enough
  (sentence-end-double-space . nil)
  ;; checkdoc, don't botch English grammar
  (checkdoc-arguments-in-order-flag . nil)
  ;; checkdoc, we don't want docs for internal vars
  (checkdoc-force-docstrings-flag . nil))
 (emacs-lisp-mode
  ;; remove trailing whitespace
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))
