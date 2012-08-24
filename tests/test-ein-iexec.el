(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-iexec)
(require 'ein-testing-notebook)


;;; `ein:iexec-should-execute-p'

(defun* eintest:iexec-should-execute-p (cell &key (this-command t) beg end)
  "Simple wrapper for `ein:iexec-should-execute-p' which
returns `t' by default, if the CELL is code cell."
  (unless beg (setq beg (ein:cell-input-pos-min cell)))
  (unless end (setq end (ein:cell-input-pos-max cell)))
  (ein:iexec-should-execute-p cell beg end))

;; cell types

(ert-deftest ein:iexec-should-execute-p-codecell ()
  (ein:testing-with-one-cell 'code
    (should (eintest:iexec-should-execute-p cell))))

(ert-deftest ein:iexec-should-execute-p-markdowncell ()
  (ein:testing-with-one-cell 'markdown
    (should-not (eintest:iexec-should-execute-p cell))))

(ert-deftest ein:iexec-should-execute-p-dead-cell ()
  (ein:testing-with-one-cell 'code
    (should-not (eintest:iexec-should-execute-p (ein:cell-copy cell)))))

;; other

(ert-deftest ein:iexec-should-execute-p-non-interactive ()
  (ein:testing-with-one-cell 'code
    (should-not (eintest:iexec-should-execute-p cell :this-command nil))))

(ert-deftest ein:iexec-should-execute-p-beg-too-small ()
  (ein:testing-with-one-cell 'code
    (should-not (eintest:iexec-should-execute-p cell :beg (point-min)))))

(ert-deftest ein:iexec-should-execute-p-end-too-big ()
  (ein:testing-with-one-cell 'code
    (should-not (eintest:iexec-should-execute-p cell :end (point-max)))))
