(require 'checkdoc)

(defun markdown-test--checkdoc-file (file)
  "Check FILE for document, comment, error style, and rogue spaces.
Taken from Emacs 25 source."
  (with-current-buffer (find-file-noselect file)
    (let ((checkdoc-diagnostic-buffer "*warn*"))
      (checkdoc-current-buffer t))))

(markdown-test--checkdoc-file "../markdown-mode.el")
