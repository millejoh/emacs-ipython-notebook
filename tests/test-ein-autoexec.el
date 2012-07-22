(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-autoexec)
(require 'test-ein-cell-notebook)


;;; `ein:autoexec-should-execute-p'

(defun* eintest:autoexec-should-execute-p (cell &key (this-command t) beg end)
  "Simple wrapper for `ein:autoexec-should-execute-p' which
returns `t' by default, if the CELL is code cell."
  (unless beg (setq beg (ein:cell-input-pos-min cell)))
  (unless end (setq end (ein:cell-input-pos-max cell)))
  (ein:autoexec-should-execute-p cell beg end))

;; cell types

(ert-deftest ein:autoexec-should-execute-p-codecell ()
  (eintest:with-one-cell 'code
    (should (eintest:autoexec-should-execute-p cell))))

(ert-deftest ein:autoexec-should-execute-p-markdowncell ()
  (eintest:with-one-cell 'markdown
    (should-not (eintest:autoexec-should-execute-p cell))))

(ert-deftest ein:autoexec-should-execute-p-dead-cell ()
  (eintest:with-one-cell 'code
    (should-not (eintest:autoexec-should-execute-p (ein:cell-copy cell)))))

;; other

(ert-deftest ein:autoexec-should-execute-p-non-interactive ()
  (eintest:with-one-cell 'code
    (should-not (eintest:autoexec-should-execute-p cell :this-command nil))))

(ert-deftest ein:autoexec-should-execute-p-beg-too-small ()
  (eintest:with-one-cell 'code
    (should-not (eintest:autoexec-should-execute-p cell :beg (point-min)))))

(ert-deftest ein:autoexec-should-execute-p-end-too-big ()
  (eintest:with-one-cell 'code
    (should-not (eintest:autoexec-should-execute-p cell :end (point-max)))))
