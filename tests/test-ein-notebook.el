(eval-when-compile (require 'cl))
(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (concat (file-name-directory load-file-name) "mocker")))
(require 'mocker)

(require 'ein-notebook)


;; Test utils

(defvar eintest:notebook-data-simple-json
  "{
 \"metadata\": {
  \"name\": \"Untitled0\"
 },
 \"name\": \"Untitled0\",
 \"nbformat\": 2,
 \"worksheets\": [
  {
   \"cells\": [
    {
     \"cell_type\": \"code\",
     \"collapsed\": false,
     \"input\": \"1 + 1\",
     \"language\": \"python\",
     \"outputs\": [
      {
       \"output_type\": \"pyout\",
       \"prompt_number\": 1,
       \"text\": \"2\"
      }
     ],
     \"prompt_number\": 1
    }
   ]
  }
 ]
}
")


(defun eintest:notebook-from-json (json-string &optional notebook-id)
  (unless notebook-id (setq notebook-id "NOTEBOOK-ID"))
  (flet ((pop-to-buffer (buf) buf)
         (ein:notebook-start-kernel ()))
    (with-current-buffer (ein:notebook-request-open-callback
                          (ein:notebook-new "DUMMY-URL" notebook-id)
                          :data (ein:json-read-from-string json-string))
      (let ((events (ein:$notebook-events ein:notebook)))
        (setf (ein:$notebook-kernel ein:notebook)
              (ein:kernel-new 8888 "/kernels" events)))
      (current-buffer))))

(defun eintest:notebook-make-data (cells &optional name)
  (unless name (setq name "Dummy Name"))
  `((metadata . ((name . ,name)))
    (nbformat . 2)
    (name . ,name)
    (worksheets . [((cells . ,(apply #'vector cells)))])))

(defun eintest:notebook-make-empty (&optional name notebook-id)
  "Make empty notebook and return its buffer."
  (eintest:notebook-from-json
   (json-encode (eintest:notebook-make-data nil name)) notebook-id))

(defun eintest:notebook-enable-mode (buffer)
  (with-current-buffer buffer (ein:notebook-plain-mode) buffer))

(defun eintest:kernel-fake-execute-reply (kernel msg-id execution-count)
  (let* ((payload nil)
         (content (list :execution_count 1 :payload payload))
         (packet (list :header (list :msg_type "execute_reply")
                       :parent_header (list :msg_id msg-id)
                       :content content)))
    (ein:kernel--handle-shell-reply kernel (json-encode packet))))

(defun eintest:kernel-fake-stream (kernel msg-id data)
  (let* ((content (list :data data
                        :name "stdout"))
         (packet (list :header (list :msg_type "stream")
                       :parent_header (list :msg_id msg-id)
                       :content content)))
    (ein:kernel--handle-iopub-reply kernel (json-encode packet))))

(defun eintest:check-search-forward-from (start string &optional null-string)
  "Search STRING from START and check it is found.
When non-`nil' NULL-STRING is given, it is searched from the
position where the search of the STRING ends and check that it
is not found."
  (save-excursion
    (goto-char start)
    (should (search-forward string nil t))
    (when null-string
      (should-not (search-forward null-string nil t)))))

(defun eintest:cell-check-output (cell regexp)
  (save-excursion
    (goto-char (ein:cell-location cell :after-input))
    (should (looking-at-p (concat "\\=" regexp "\n")))))


;; from-json

(ert-deftest ein:notebook-from-json-simple ()
  (with-current-buffer (eintest:notebook-from-json
                        eintest:notebook-data-simple-json)
    (should (ein:$notebook-p ein:notebook))
    (should (equal (ein:$notebook-notebook-id ein:notebook) "NOTEBOOK-ID"))
    (should (equal (ein:$notebook-notebook-name ein:notebook) "Untitled0"))
    (should (equal (ein:notebook-ncells ein:notebook) 1))
    (let ((cell (car (ein:notebook-get-cells ein:notebook))))
      (should (ein:codecell-p cell))
      (should (equal (oref cell :input) "1 + 1"))
      (should (equal (oref cell :input-prompt-number) 1))
      (let ((outputs (oref cell :outputs)))
        (should (equal (length outputs) 1))
        (let ((o1 (car outputs)))
          (should (equal (plist-get o1 :output_type) "pyout"))
          (should (equal (plist-get o1 :prompt_number) 1))
          (should (equal (plist-get o1 :text) "2")))))))

(ert-deftest ein:notebook-from-json-empty ()
  (with-current-buffer (eintest:notebook-make-empty)
    (should (ein:$notebook-p ein:notebook))
    (should (equal (ein:$notebook-notebook-id ein:notebook) "NOTEBOOK-ID"))
    (should (equal (ein:$notebook-notebook-name ein:notebook) "Dummy Name"))
    (should (equal (ein:notebook-ncells ein:notebook) 0))))


;; Notebook commands

(ert-deftest ein:notebook-insert-cell-below-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-below-command)
    (ein:notebook-insert-cell-below-command)
    (ein:notebook-insert-cell-below-command)
    (should (equal (ein:notebook-ncells ein:notebook) 3))))

(ert-deftest ein:notebook-insert-cell-above-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-above-command)
    (ein:notebook-insert-cell-above-command)
    (ein:notebook-insert-cell-above-command)
    (should (equal (ein:notebook-ncells ein:notebook) 3))))

(ert-deftest ein:notebook-delete-cell-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (loop repeat 3
          do (ein:notebook-insert-cell-above-command))
    (should (equal (ein:notebook-ncells ein:notebook) 3))
    (loop repeat 3
          do (ein:notebook-delete-cell-command))
    (should (equal (ein:notebook-ncells ein:notebook) 0))))

(ert-deftest ein:notebook-delete-cell-command-no-undo ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-above-command)
    (insert "some text")
    (should (equal (buffer-string) "
In [ ]:
some text

"))
    (ein:notebook-kill-cell-command)
    (should (equal (buffer-string) "\n"))
    (should-error (undo))               ; should be ignore-error?
    (should (equal (buffer-string) "\n"))))

(ert-deftest ein:notebook-kill-cell-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (let (ein:kill-ring ein:kill-ring-yank-pointer)
      (loop repeat 3
            do (ein:notebook-insert-cell-above-command))
      (should (equal (ein:notebook-ncells ein:notebook) 3))
      (loop for i from 1 to 3
            do (ein:notebook-kill-cell-command)
            do (should (equal (length ein:kill-ring) i))
            do (should (equal (ein:notebook-ncells ein:notebook) (- 3 i)))))))

(ert-deftest ein:notebook-copy-cell-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (let (ein:kill-ring ein:kill-ring-yank-pointer)
      (loop repeat 3
            do (ein:notebook-insert-cell-above-command))
      (should (equal (ein:notebook-ncells ein:notebook) 3))
      (loop repeat 3
            do (ein:notebook-copy-cell-command))
      (should (equal (ein:notebook-ncells ein:notebook) 3))
      (should (equal (length ein:kill-ring) 3)))))

(ert-deftest ein:notebook-yank-cell-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (let (ein:kill-ring ein:kill-ring-yank-pointer)
      (loop repeat 3
            do (ein:notebook-insert-cell-above-command))
      (should (equal (ein:notebook-ncells ein:notebook) 3))
      (loop repeat 3
            do (ein:notebook-kill-cell-command))
      (should (equal (ein:notebook-ncells ein:notebook) 0))
      (should (equal (length ein:kill-ring) 3))
      (loop repeat 3
            do (ein:notebook-yank-cell-command))
      (should (equal (ein:notebook-ncells ein:notebook) 3))
      (loop for cell in (ein:notebook-get-cells ein:notebook)
            do (should (ein:codecell-p cell))
            do (should (slot-boundp cell :kernel))
            do (should (slot-boundp cell :events))))))

(ert-deftest ein:notebook-yank-cell-command-two-buffers ()
  (let (ein:kill-ring ein:kill-ring-yank-pointer)
    (with-current-buffer (eintest:notebook-make-empty "NB1")
      (ein:notebook-insert-cell-above-command)
      (should (equal (ein:notebook-ncells ein:notebook) 1))
      (ein:notebook-kill-cell-command)
      (should (equal (ein:notebook-ncells ein:notebook) 0))
      (flet ((y-or-n-p (&rest ignore) t)
             (ein:notebook-del (&rest ignore)))
        ;; FIXME: are there anyway to skip confirmation?
        (kill-buffer)))
    (with-current-buffer (eintest:notebook-make-empty "NB2")
      (ein:notebook-yank-cell-command)
      (should (equal (ein:notebook-ncells ein:notebook) 1)))))

(ert-deftest ein:notebook-toggle-cell-type-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-above-command)
    (insert "some text")
    (should (ein:codecell-p (ein:notebook-get-current-cell)))
    (should (slot-boundp (ein:notebook-get-current-cell) :kernel))
    ;; toggle to markdown
    (ein:notebook-toggle-cell-type)
    (should (ein:markdowncell-p (ein:notebook-get-current-cell)))
    (should (looking-back "some text"))
    ;; toggle to code
    (ein:notebook-toggle-cell-type)
    (should (ein:codecell-p (ein:notebook-get-current-cell)))
    (should (slot-boundp (ein:notebook-get-current-cell) :kernel))
    (should (looking-back "some text"))))

(ert-deftest ein:notebook-change-cell-type-cycle-through ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-above-command)
    (insert "some text")
    ;; start with code cell
    (should (ein:codecell-p (ein:notebook-get-current-cell)))
    (should (slot-boundp (ein:notebook-get-current-cell) :kernel))
    (let ((check
           (lambda (type &optional level)
             (let ((cell-p (intern (format "ein:%scell-p" type))))
               (ein:notebook-change-cell-type type level)
               (should (funcall cell-p (ein:notebook-get-current-cell)))
               (should (looking-back "some text"))))))
      ;; change type: code (no change) -> markdown -> raw
      (loop for type in '("code" "markdown" "raw")
            do (funcall check type))
      ;; change level: 1 to 6
      (loop for level from 1 to 6
            do (funcall check "heading" level))
      ;; back to code
      (funcall check "code")
      (should (slot-boundp (ein:notebook-get-current-cell) :kernel)))))

(defun eintest:notebook-split-cell-at-point
  (insert-text search-text head-text tail-text &optional no-trim)
  "Test `ein:notebook-split-cell-at-point' by the following procedure.

1. Insert, INSERT-TEXT.
2. Split cell just before SEARCH-TEXT.
3. Check that head cell has HEAD-TEXT.
4. Check that tail cell has TAIL-TEXT.

NO-TRIM is passed to `ein:notebook-split-cell-at-point'."
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-above-command)
    (insert insert-text)
    (search-backward search-text)
    ;; do it
    (ein:notebook-split-cell-at-point no-trim)
    ;; check the "tail" cell
    (let ((cell (ein:notebook-get-current-cell)))
      (ein:cell-goto cell)
      (should (equal (ein:cell-get-text cell) tail-text))
      (should (ein:codecell-p cell))
      (should (slot-boundp cell :kernel)))
    ;; check the "head" cell
    (ein:notebook-goto-prev-input-command)
    (let ((cell (ein:notebook-get-current-cell)))
      (ein:cell-goto cell)
      (should (equal (ein:cell-get-text cell) head-text))
      (should (ein:codecell-p cell))
      (should (slot-boundp cell :kernel)))))

(ert-deftest ein:notebook-split-cell-at-point-before-newline ()
  (eintest:notebook-split-cell-at-point
   "some\ntext" "text" "some" "text"))

(ert-deftest ein:notebook-split-cell-at-point-after-newline ()
  (eintest:notebook-split-cell-at-point
   "some\ntext" "\ntext" "some" "text"))

(ert-deftest ein:notebook-split-cell-at-point-before-newline-no-trim ()
  (eintest:notebook-split-cell-at-point
   "some\ntext" "text" "some\n" "text" t))

(ert-deftest ein:notebook-split-cell-at-point-after-newline-no-trim ()
  (eintest:notebook-split-cell-at-point
   "some\ntext" "\ntext" "some" "\ntext" t))

(ert-deftest ein:notebook-merge-cell-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-above-command)
    (insert "Cell 1")
    (ein:notebook-insert-cell-above-command)
    (insert "Cell 0")
    (ein:notebook-merge-cell-command)
    (ein:cell-goto (ein:notebook-get-current-cell))
    (should (looking-at "Cell 0\nCell 1"))))

(ert-deftest ein:notebook-merge-cell-command-prev ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-below-command)
    (insert "Cell 0")
    (ein:notebook-insert-cell-below-command)
    (insert "Cell 1")
    (ein:notebook-merge-cell-command t)
    (ein:cell-goto (ein:notebook-get-current-cell))
    (should (looking-at "Cell 0\nCell 1"))))

(ert-deftest ein:notebook-goto-next-input-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (loop for i downfrom 2 to 0
          do (ein:notebook-insert-cell-above-command)
          do (insert (format "Cell %s" i)))
    (should (equal (ein:notebook-ncells ein:notebook) 3))
    ;; (message "%s" (buffer-string))
    (loop for i from 0 below 2
          do (beginning-of-line) ; This is required, I need to check why
          do (should (looking-at (format "Cell %s" i)))
          do (ein:notebook-goto-next-input-command)
          do (should (looking-at (format "Cell %s" (1+ i)))))))

(ert-deftest ein:notebook-goto-prev-input-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (loop for i from 0 below 3
          do (ein:notebook-insert-cell-below-command)
          do (insert (format "Cell %s" i)))
    (should (equal (ein:notebook-ncells ein:notebook) 3))
    ;; (message "%s" (buffer-string))
    (loop for i downfrom 2 to 1
          do (beginning-of-line) ; This is required, I need to check why
          do (should (looking-at (format "Cell %s" i)))
          do (ein:notebook-goto-prev-input-command)
          do (should (looking-at (format "Cell %s" (1- i)))))))

(ert-deftest ein:notebook-move-cell-up-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (loop for i from 0 below 3
          do (ein:notebook-insert-cell-below-command)
          do (insert (format "Cell %s" i)))
    (beginning-of-line)
    (should (looking-at "Cell 2"))
    (loop repeat 2
          do (ein:notebook-move-cell-up-command))
    ;; (message "%s" (buffer-string))
    (beginning-of-line)
    (should (looking-at "Cell 2"))
    (should (search-forward "Cell 0" nil t))
    (should (search-forward "Cell 1" nil t))
    (should-not (search-forward "Cell 2" nil t))))

(ert-deftest ein:notebook-move-cell-down-command-simple ()
  (with-current-buffer (eintest:notebook-make-empty)
    (loop for i from 0 below 3
          do (ein:notebook-insert-cell-above-command)
          do (insert (format "Cell %s" i)))
    (loop repeat 2
          do (ein:notebook-move-cell-down-command))
    (beginning-of-line)
    (should (looking-at "Cell 2"))
    (should (search-backward "Cell 0" nil t))
    (should (search-backward "Cell 1" nil t))
    (should-not (search-backward "Cell 2" nil t))))


;; Kernel related things

(defun eintest:notebook-check-kernel-and-codecell (kernel cell)
  (should (ein:$kernel-p kernel))
  (should (ein:codecell-p cell))
  (should (ein:$kernel-p (oref cell :kernel))))

(defun eintest:notebook-fake-execution (kernel text msg-id callbacks)
  (mocker-let ((ein:kernel-execute
                (kernel code callbacks kwd-silent silent)
                ((:input (list kernel text callbacks :silent nil))))
               (ein:kernel-live-p
                (kernel)
                ((:input (list kernel) :output t))))
    (ein:notebook-execute-current-cell))
  (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks))

(ert-deftest ein:notebook-execute-current-cell ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-below-command)
    (let* ((text "print 'Hello World'")
           (cell (ein:notebook-get-current-cell))
           (kernel (ein:$notebook-kernel ein:notebook))
           (msg-id "DUMMY-MSG-ID")
           (callbacks (ein:cell-make-callbacks cell)))
      (eintest:notebook-check-kernel-and-codecell kernel cell)
      ;; Execute
      (insert text)
      (eintest:notebook-fake-execution kernel text msg-id callbacks)
      ;; Execute reply
      (should-error (eintest:check-search-forward-from (point-min) "In [1]:"))
      (eintest:kernel-fake-execute-reply kernel msg-id 1)
      (should (= (oref cell :input-prompt-number) 1))
      (eintest:check-search-forward-from (point-min) "In [1]:")
      ;; Stream output
      (eintest:kernel-fake-stream kernel msg-id "Hello World")
      (should (= (ein:cell-num-outputs cell) 1))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "In [1]:" nil t))
        (should (search-forward "print 'Hello World'" nil t))
        (should (search-forward "\nHello World\n" nil t)) ; stream output
        (should-not (search-forward "Hello World" nil t))))))


;; Notebook undo

(defun eintest:notebook-undo-after-insert-above ()
  (with-current-buffer (eintest:notebook-make-empty)
    (let ((text "some text"))
      (ein:notebook-insert-cell-above-command)
      (insert text)
      (undo-boundary)
      (ein:notebook-insert-cell-above-command)
      (ein:notebook-goto-next-input-command)
      (should (equal (ein:cell-get-text (ein:notebook-get-current-cell)) text))
      (if (eq ein:notebook-enable-undo 'full)
          (undo)
        (should-error (undo)))
      (when (eq ein:notebook-enable-undo 'full)
        ;; FIXME: Known bug. (this must succeed.)
        (should-error (should (equal (buffer-string) "
In [ ]:


In [ ]:


")))))))

(defun eintest:notebook-undo-after-split ()
  (with-current-buffer (eintest:notebook-make-empty)
    (let ((line-1 "first line")
          (line-2 "second line"))
      (ein:notebook-insert-cell-below-command)
      (insert line-1 "\n" line-2)
      (undo-boundary)
      (move-beginning-of-line 1)
      (ein:notebook-split-cell-at-point)
      (should (equal (ein:cell-get-text (ein:notebook-get-current-cell))
                     line-2))
      (if (eq ein:notebook-enable-undo 'full)
          (undo)
        (should-error (undo)))
      (when (eq ein:notebook-enable-undo 'full)
        (should (equal (buffer-string) "
In [ ]:


In [ ]:


"))))))

(defun eintest:notebook-undo-after-merge ()
  (with-current-buffer (eintest:notebook-make-empty)
    (let ((line-1 "first line")
          (line-2 "second line"))
      (ein:notebook-insert-cell-below-command)
      (ein:notebook-insert-cell-below-command)
      ;; Extra cells to avoid "Changes to be undone are outside visible
      ;; portion of buffer" user-error:
      (ein:notebook-insert-cell-below-command)
      (ein:notebook-insert-cell-below-command)
      (goto-char (point-min))
      (ein:notebook-goto-next-input-command)

      (insert line-1)
      (undo-boundary)

      (ein:notebook-goto-next-input-command)
      (insert line-2)
      (undo-boundary)

      (ein:notebook-goto-prev-input-command)
      (ein:notebook-merge-cell-command)
      (undo-boundary)

      (should (equal (ein:cell-get-text (ein:notebook-get-current-cell))
                     (concat line-1 "\n" line-2)))
      (if (eq ein:notebook-enable-undo 'no)
          (should-error (undo))
        (undo)
        (should (equal (buffer-string) "
In [ ]:
first line

In [ ]:


In [ ]:


")))
      (when (eq ein:notebook-enable-undo 'yes)
        ;; FIXME: `undo' should work...
        (should-error (undo-more 1)))
      (when (eq ein:notebook-enable-undo 'full)
        (undo)
        ;; FIXME: Known bug... What should the result be?
        (should-error (should (equal (buffer-string) "
In [ ]:


In [ ]:


In [ ]:


")))))))

(defun eintest:notebook-undo-after-execution-1-cell ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-below-command)
    (let* ((text "print 'Hello World'")
           (output-text "Hello World\n")
           (cell (ein:notebook-get-current-cell))
           (kernel (ein:$notebook-kernel ein:notebook))
           (msg-id "DUMMY-MSG-ID")
           (callbacks (ein:cell-make-callbacks cell))
           (check-output
            (lambda ()
              (eintest:cell-check-output cell output-text))))
      (eintest:notebook-check-kernel-and-codecell kernel cell)
      ;; Execute
      (insert text)
      (undo-boundary)
      (eintest:notebook-fake-execution kernel text msg-id callbacks)
      (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
      ;; Stream output
      (eintest:kernel-fake-stream kernel msg-id output-text)
      (funcall check-output)
      ;; Undo
      (should (equal (ein:cell-get-text cell) text))
      (if (eq ein:notebook-enable-undo 'full)
          (undo)
        (should-error (undo)))
      (when (eq ein:notebook-enable-undo 'full)
        (should (equal (ein:cell-get-text cell) ""))
        ;; FIXME: Known bug. (it must succeed.)
        (should-error (funcall check-output))))))

(defun eintest:notebook-undo-after-execution-2-cells ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-below-command)
    (ein:notebook-insert-cell-above-command)
    (let* ((text "print 'Hello World\\n' * 10")
           (next-text "something")
           (output-text
            (apply #'concat (loop repeat 10 collect "Hello World\n")))
           (cell (ein:notebook-get-current-cell))
           (next-cell (ein:cell-next cell))
           (kernel (ein:$notebook-kernel ein:notebook))
           (msg-id "DUMMY-MSG-ID")
           (callbacks (ein:cell-make-callbacks cell))
           (check-output
            (lambda ()
              (eintest:cell-check-output cell output-text))))
      (eintest:notebook-check-kernel-and-codecell kernel cell)
      ;; Execute
      (insert text)
      (undo-boundary)
      (let ((pos (point)))
        ;; Do not use `save-excursion' because it does not record undo.
        (ein:notebook-goto-next-input-command)
        (insert next-text)
        (undo-boundary)
        (goto-char pos))
      (eintest:notebook-fake-execution kernel text msg-id callbacks)
      (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
      ;; Stream output
      (eintest:kernel-fake-stream kernel msg-id output-text)
      (funcall check-output)
      ;; Undo
      (should (equal (ein:cell-get-text cell) text))
      (should (equal (ein:cell-get-text next-cell) next-text))
      (if (eq ein:notebook-enable-undo 'full)
          (undo)
        (should-error (undo)))
      (when (eq ein:notebook-enable-undo 'full)
        (should (equal (ein:cell-get-text cell) text))
        ;; FIXME: Known bug. (these two must succeed.)
        (should-error (should (equal (ein:cell-get-text next-cell) "")))
        (should-error (funcall check-output))))))

(defmacro eintest:notebook-undo-make-tests (name)
  "Define three tests ein:NANE/no, ein:NANE/yes and ein:NANE/full
from a function named eintest:NAME where `no'/`yes'/`full' is the
value of `ein:notebook-enable-undo'."
  (let ((func (intern (format "eintest:%s" name)))
        (test/no (intern (format "ein:%s/no" name)))
        (test/yes (intern (format "ein:%s/yes" name)))
        (test/full (intern (format "ein:%s/full" name))))
    `(progn
       (ert-deftest ,test/no ()
         (let ((ein:notebook-enable-undo 'no))
           (,func)))
       (ert-deftest ,test/yes ()
         (let ((ein:notebook-enable-undo 'yes))
           (,func)))
       (ert-deftest ,test/full ()
         (let ((ein:notebook-enable-undo 'full))
           (,func))))))

(eintest:notebook-undo-make-tests notebook-undo-after-insert-above)
(eintest:notebook-undo-make-tests notebook-undo-after-split)
(eintest:notebook-undo-make-tests notebook-undo-after-merge)
(eintest:notebook-undo-make-tests notebook-undo-after-execution-1-cell)
(eintest:notebook-undo-make-tests notebook-undo-after-execution-2-cells)


;; Notebook mode

(ert-deftest ein:notebook-ask-before-kill-emacs-simple ()
  (let ((ein:notebook-opened-map (make-hash-table :test 'equal)))
    (should (ein:notebook-ask-before-kill-emacs))
    (with-current-buffer
        (eintest:notebook-enable-mode
         (eintest:notebook-make-empty "Modified Notebook" "NOTEBOOK-ID-1"))
      (ein:notebook-insert-cell-below-command)
      (should (ein:notebook-modified-p)))
    (with-current-buffer
        (eintest:notebook-enable-mode
         (eintest:notebook-make-empty "Unmodified Notebook" "NOTEBOOK-ID-2"))
      (should-not (ein:notebook-modified-p)))
    (flet ((y-or-n-p (&rest ignore) t)
           (ein:notebook-del (&rest ignore)))
      (kill-buffer
       (eintest:notebook-enable-mode
        (eintest:notebook-make-empty "Killed Notebook" "NOTEBOOK-ID-3"))))
    (should (= (hash-table-count ein:notebook-opened-map) 3))
    (mocker-let ((y-or-n-p
                  (prompt)
                  ((:input '("You have 1 unsaved notebook(s). Discard changes?")
                           :output t))))
      (should (ein:notebook-ask-before-kill-emacs)))))


;; Misc unit tests

(ert-deftest ein:notebook-test-notebook-name-simple ()
  (should-not (ein:notebook-test-notebook-name nil))
  (should-not (ein:notebook-test-notebook-name ""))
  (should-not (ein:notebook-test-notebook-name "/"))
  (should-not (ein:notebook-test-notebook-name "\\"))
  (should-not (ein:notebook-test-notebook-name "a/b"))
  (should-not (ein:notebook-test-notebook-name "a\\b"))
  (should (ein:notebook-test-notebook-name "This is a OK notebook name")))

(ert-deftest ein:notebook-console-security-dir-string ()
  (let ((ein:notebook-console-security-dir "/some/dir/")
        (notebook (ein:notebook-new "DUMMY-URL-OR-PORT" "DUMMY-NOTEBOOK-ID")))
    (should (equal (ein:notebook-console-security-dir-get notebook)
                   ein:notebook-console-security-dir))))

(ert-deftest ein:notebook-console-security-dir-list ()
  (let ((ein:notebook-console-security-dir
         '((8888 . "/dir/8888/")
           ("htttp://dummy.org" . "/dir/http/")
           (7777 . my-secret-directory)
           (default . "/dir/default/")))
        (my-secret-directory "/dir/secret/"))
    (let ((notebook (ein:notebook-new 8888 "DUMMY-NOTEBOOK-ID")))
      (should (equal (ein:notebook-console-security-dir-get notebook)
                     "/dir/8888/")))
    (let ((notebook (ein:notebook-new "htttp://dummy.org" "DUMMY-NOTEBOOK-ID")))
      (should (equal (ein:notebook-console-security-dir-get notebook)
                     "/dir/http/")))
    (let ((notebook (ein:notebook-new 7777 "DUMMY-NOTEBOOK-ID")))
      (should (equal (ein:notebook-console-security-dir-get notebook)
                     "/dir/secret/")))
    (let ((notebook (ein:notebook-new 9999 "DUMMY-NOTEBOOK-ID")))
      (should (equal (ein:notebook-console-security-dir-get notebook)
                     "/dir/default/")))))

(ert-deftest ein:notebook-console-security-dir-func ()
  (let ((ein:notebook-console-security-dir
         '(lambda (x) (should (equal x "DUMMY-URL-OR-PORT")) "/dir/"))
        (notebook (ein:notebook-new "DUMMY-URL-OR-PORT" "DUMMY-NOTEBOOK-ID")))
    (should (equal (ein:notebook-console-security-dir-get notebook) "/dir/"))))


(provide 'test-ein-notebook)
