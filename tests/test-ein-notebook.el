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
      (let ((events (ein:events-new (current-buffer))))
        (setf (ein:$notebook-events ein:notebook) events)
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
            do (should (slot-boundp cell :kernel))))))

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
    (should (looking-at "some text"))
    ;; toggle to code
    (ein:notebook-toggle-cell-type)
    (should (ein:codecell-p (ein:notebook-get-current-cell)))
    (should (slot-boundp (ein:notebook-get-current-cell) :kernel))
    (should (looking-at "some text"))))

(defun eintest:notebook-split-cell-at-point
  (insert-text search-text head-text tail-text &optional no-trim)
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

(ert-deftest ein:notebook-execute-current-cell ()
  (with-current-buffer (eintest:notebook-make-empty)
    (ein:notebook-insert-cell-below-command)
    (let* ((text "print 'Hello World'")
           (cell (ein:notebook-get-current-cell))
           (kernel (ein:$notebook-kernel ein:notebook))
           (msg-id "DUMMY-MSG-ID")
           (callbacks
            (list :execute_reply (cons #'ein:cell--handle-execute-reply cell)
                  :output        (cons #'ein:cell--handle-output        cell)
                  :clear_output  (cons #'ein:cell--handle-clear-output  cell)
                  :set_next_input (cons #'ein:cell--handle-set-next-input cell))))
      (should (ein:$kernel-p kernel))
      (should (ein:codecell-p cell))
      (should (ein:$kernel-p (oref cell :kernel)))
      (insert text)
      (mocker-let ((ein:kernel-execute
                    (kernel code callbacks kwd-silent silent)
                    ((:input (list kernel text callbacks :silent nil))))
                   (ein:kernel-ready-p
                    (kernel)
                    ((:input (list kernel) :output t))))
        (ein:notebook-execute-current-cell))
      (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks)
      (save-excursion
        (goto-char (point-min))
        (should-not (search-forward "In [1]:" nil t)))
      (let* ((payload nil)
             (content (list :execution_count 1 :payload payload))
             (packet (list :header (list :msg_type "execute_reply")
                           :parent_header (list :msg_id msg-id)
                           :content content)))
        (ein:kernel--handle-shell-reply kernel (json-encode packet)))
      (should (= (oref cell :input-prompt-number) 1))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "In [1]:" nil t)))
      (let* ((content (list :data "'Hello World'"
                            :name "stdout"))
             (packet (list :header (list :msg_type "stream")
                           :parent_header (list :msg_id msg-id)
                           :content content)))
        (ein:kernel--handle-iopub-reply kernel (json-encode packet)))
      (should (= (ein:cell-num-outputs cell) 1))
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "In [1]:" nil t))
        (should (search-forward "print 'Hello World'" nil t))
        (should (search-forward "Hello World" nil t)) ; stream output
        (should-not (search-forward "Hello World" nil t))))))


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
