;; -*- lexical-binding:t -*-

(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (concat (file-name-directory load-file-name) "mocker")))
(require 'mocker)

(require 'ein-notebook)
(require 'ein-testing-notebook)
(require 'ein-testing-cell)

;; Test utils

;;; This is the content portion of a response fromt he content API.
(defvar eintest:notebook-data-simple-json
  "{
 \"name\": \"Untitled0.ipynb\",
 \"path\": \"\",
 \"type\": \"notebook\",
 \"content\" : {
   \"metadata\": {
   },
   \"nbformat\": 3,
   \"nbformat_minor\": 0,

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
}
")

(defvar eintest:notebook-data-simple-json-v4
  "{
 \"metadata\": {
  \"name\": \"Untitled0.ipynb\"
 },
 \"nbformat\": 4,
 \"nbformat_minor\": 0,
 \"cells\": [
  {
   \"cell_type\": \"code\",
   \"metadata\" : {
        \"collapsed\": false,
   },
   \"source\": \"1 + 1\",
   \"outputs\": [
    {
     \"name\": \"stdout\",
     \"output_type\": \"stream\",
     \"text\": \"2\"
    }
   ],
   \"prompt_number\": 1
  }
 ]
}
")


(defun eintest:kernel-fake-execute-reply (kernel msg-id _execution-count)
  (let* ((payload nil)
         (content (list :execution_count 1 :payload payload))
         (packet (list :header (list :msg_type "execute_reply")
                       :parent_header (list :msg_id msg-id)
                       :content content)))
    (ein:kernel--handle-shell-reply kernel (json-encode packet))))

(defun eintest:kernel-fake-stream (kernel msg-id data)
  (let* ((content (list :text data
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
  (with-current-buffer (ein:testing-notebook-from-json
                        eintest:notebook-data-simple-json)
    (should (ein:$notebook-p ein:%notebook%))
    (should (equal (ein:$notebook-notebook-name ein:%notebook%) "Untitled0.ipynb"))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 1))
    (let ((cell (car (ein:worksheet-get-cells ein:%worksheet%))))
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
  (with-current-buffer (ein:testing-notebook-make-empty)
    (should (ein:$notebook-p ein:%notebook%))
    (should (equal (ein:$notebook-notebook-name ein:%notebook%) ein:testing-notebook-dummy-name))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 0))))

(ert-deftest ein:notebook-from-json-all-cell-types ()
  (with-current-buffer
      (ein:testing-notebook-make-new
       ein:testing-notebook-dummy-name
       nil
       (list (ein:testing-codecell-data "import numpy")
             (ein:testing-markdowncell-data "*markdown* text")
             (ein:testing-rawcell-data "`raw` cell text")
             (ein:testing-htmlcell-data "<b>HTML</b> text")))
    (should (ein:$notebook-p ein:%notebook%))
    (should (equal (ein:$notebook-notebook-name ein:%notebook%)
                   ein:testing-notebook-dummy-name))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 4))
    (let ((cells (ein:worksheet-get-cells ein:%worksheet%)))
      (should (ein:codecell-p     (nth 0 cells)))
      (should (ein:markdowncell-p (nth 1 cells)))
      (should (ein:rawcell-p      (nth 2 cells)))
      (should (ein:htmlcell-p     (nth 3 cells)))
      (should (equal (ein:cell-get-text (nth 0 cells)) "import numpy"))
      (should (equal (ein:cell-get-text (nth 1 cells)) "*markdown* text"))
      (should (equal (ein:cell-get-text (nth 2 cells)) "`raw` cell text"))
      (should (equal (ein:cell-get-text (nth 3 cells)) "<b>HTML</b> text")))))

;;; Destructor

(defun ein:testing-notebook-close-scratchsheet-open-and-close
  (num-open num-close)
  "Test for closing scratch sheet using `ein:notebook-close-worksheet'.

1. Open NUM-OPEN scratch sheets.
2. Close an existing worksheet.
3. Close NUM-CLOSE scratch sheets.

When NUM-OPEN = NUM-CLOSE, notebook should be closed."
  (should (> num-open 0))
  (with-current-buffer (ein:testing-notebook-make-empty)
    (cl-symbol-macrolet ((ss-list (ein:$notebook-scratchsheets ein:%notebook%)))
      ;; Add scratchsheets.  They can be just empty instance for this test.
      (dotimes (_ num-open)
        (ein:notebook-scratchsheet-render-new ein:%notebook%))
      (let ((ss (car ss-list)))
        (kill-buffer (ein:worksheet-buffer ss)))
      (should (= (length ss-list) (1- num-open)))
      (dotimes (_ (1- num-close))
        (kill-buffer (ein:worksheet-buffer (car ss-list))))
      (should (= (length ss-list) (- num-open num-close)))
      (let ((my-buffer (buffer-name)))
        (kill-buffer (current-buffer))
        (should-not (seq-some (lambda (b)
                                (cl-search my-buffer (buffer-name b)))
                              (buffer-list)))))))

(ert-deftest ein:notebook-close-scratchsheet/open-one-close-one ()
  (ein:testing-notebook-close-scratchsheet-open-and-close 1 1))

(ert-deftest ein:notebook-close-scratchsheet/open-two-close-two ()
  (ein:testing-notebook-close-scratchsheet-open-and-close 2 2))

(ert-deftest ein:notebook-close-scratchsheet/open-two-close-one ()
  (ein:testing-notebook-close-scratchsheet-open-and-close 2 1))

;;; Insertion and deletion of cells

(ert-deftest ein:notebook-insert-cell-below-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))))

(ert-deftest ein:notebook-insert-cell-above-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))))

(ert-deftest ein:notebook-delete-cell-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (cl-loop repeat 3
          do (call-interactively #'ein:worksheet-insert-cell-above))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
    (cl-loop repeat 3
          do (call-interactively #'ein:worksheet-delete-cell))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 0))))

(ert-deftest ein:notebook-delete-cell-command-no-undo ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (insert "some text")
    (should (equal (buffer-string) "
In [ ]:
some text

"))
    (call-interactively #'ein:worksheet-delete-cell)
    (should (equal (buffer-string) "\n"))
    (should-error (undo))               ; should be ignore-error?
    (should (equal (buffer-string) "\n"))))

(ert-deftest ein:notebook-kill-cell-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let (ein:kill-ring ein:kill-ring-yank-pointer)
      (cl-loop repeat 3
            do (call-interactively #'ein:worksheet-insert-cell-above))
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
      (cl-loop for i from 1 to 3
            do (call-interactively #'ein:worksheet-kill-cell)
            do (should (equal (length ein:kill-ring) i))
            do (should (equal (ein:worksheet-ncells ein:%worksheet%) (- 3 i)))))))

(ert-deftest ein:notebook-copy-cell-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let (ein:kill-ring ein:kill-ring-yank-pointer)
      (cl-loop repeat 3
            do (call-interactively #'ein:worksheet-insert-cell-above))
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
      (cl-loop repeat 3
            do (call-interactively #'ein:worksheet-copy-cell))
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
      (should (equal (length ein:kill-ring) 3)))))

(ert-deftest ein:notebook-yank-cell-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let (ein:kill-ring ein:kill-ring-yank-pointer)
      (cl-loop repeat 3
            do (call-interactively #'ein:worksheet-insert-cell-above))
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
      (cl-loop repeat 3
            do (call-interactively #'ein:worksheet-kill-cell))
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 0))
      (should (equal (length ein:kill-ring) 3))
      (cl-loop repeat 3
            do (call-interactively #'ein:worksheet-yank-cell))
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
      (cl-loop for cell in (ein:worksheet-get-cells ein:%worksheet%)
            do (should (ein:codecell-p cell))
            do (should (slot-boundp cell :kernel))
            do (should (slot-boundp cell :events))))))

(ert-deftest ein:notebook-yank-cell-command-two-buffers ()
  (let (ein:kill-ring ein:kill-ring-yank-pointer)
    (with-current-buffer (ein:testing-notebook-make-empty "NB1")
      (call-interactively #'ein:worksheet-insert-cell-above)
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 1))
      (call-interactively #'ein:worksheet-kill-cell)
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 0))
      (kill-buffer))
    (with-current-buffer (ein:testing-notebook-make-empty "NB2")
      (call-interactively #'ein:worksheet-yank-cell)
      (should (equal (ein:worksheet-ncells ein:%worksheet%) 1)))))

(ert-deftest ein:notebook-toggle-cell-type-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (insert "some text")
    (should (ein:codecell-p (ein:worksheet-get-current-cell)))
    (should (slot-boundp (ein:worksheet-get-current-cell) :kernel))
    ;; toggle to markdown
    (call-interactively #'ein:worksheet-toggle-cell-type)
    (should (ein:markdowncell-p (ein:worksheet-get-current-cell)))
    (should (looking-back "some text"))
    ;; toggle to raw
    (call-interactively #'ein:worksheet-toggle-cell-type)
    (should (ein:rawcell-p (ein:worksheet-get-current-cell)))
    (should (looking-back "some text"))
    (when (< (ein:$notebook-nbformat ein:%notebook%) 4)
      ;; toggle to heading
      (call-interactively #'ein:worksheet-toggle-cell-type)
      (should (looking-back "some text"))
      ;; toggle to code
      (call-interactively #'ein:worksheet-toggle-cell-type)
      (should (ein:codecell-p (ein:worksheet-get-current-cell)))
      (should (slot-boundp (ein:worksheet-get-current-cell) :kernel))
      (should (looking-back "some text")))))

(ert-deftest ein:notebook-change-cell-type-cycle-through ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (insert "some text")
    ;; start with code cell
    (should (ein:codecell-p (ein:worksheet-get-current-cell)))
    (should (slot-boundp (ein:worksheet-get-current-cell) :kernel))
    (let ((check
           (lambda (type)
             (let ((cell-p (intern (format "ein:%scell-p" type)))
                   (cell (ein:worksheet-get-current-cell)))
               (ein:worksheet-change-cell-type ein:%worksheet% cell t)
               (let ((new (ein:worksheet-get-current-cell)))
                 (should-not (eq new cell))
                 (should (funcall cell-p new)))
               (should (looking-back "some text"))))))
      ;; change type: code (no change) -> markdown -> raw
      (cl-loop for type in '("markdown" "raw" "code")
               do (funcall check type))
      ;; back to code
      (should (slot-boundp (ein:worksheet-get-current-cell) :kernel)))))

(defun eintest:notebook-split-cell-at-point
  (insert-text search-text head-text tail-text &optional no-trim)
  "Test `ein:notebook-split-cell-at-point' by the following procedure.

1. Insert, INSERT-TEXT.
2. Split cell just before SEARCH-TEXT.
3. Check that head cell has HEAD-TEXT.
4. Check that tail cell has TAIL-TEXT.

NO-TRIM is passed to `ein:notebook-split-cell-at-point'."
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (insert insert-text)
    (when search-text
      (search-backward search-text))
    ;; do it
    (let ((current-prefix-arg no-trim))
      (call-interactively #'ein:worksheet-split-cell-at-point))
    ;; check the "tail" cell
    (let ((cell (ein:worksheet-get-current-cell)))
      (ein:cell-goto cell)
      (should (equal (ein:cell-get-text cell) tail-text))
      (should (ein:codecell-p cell))
      (should (slot-boundp cell :kernel)))
    ;; check the "head" cell
    (call-interactively #'ein:worksheet-goto-prev-input)
    (let ((cell (ein:worksheet-get-current-cell)))
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

(ert-deftest ein:notebook-split-cell-at-point-no-head ()
  (eintest:notebook-split-cell-at-point
   "some" "some" "" "some"))

(ert-deftest ein:notebook-split-cell-at-point-no-tail ()
  (eintest:notebook-split-cell-at-point
   "some" nil "some" ""))

(ert-deftest ein:notebook-merge-cell-command-next ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (insert "Cell 1")
    (call-interactively #'ein:worksheet-insert-cell-above)
    (insert "Cell 0")
    (let ((current-prefix-arg t))
      (call-interactively #'ein:worksheet-merge-cell))
    (ein:cell-goto (ein:worksheet-get-current-cell))
    (should (looking-at "Cell 0\nCell 1"))))

(ert-deftest ein:notebook-merge-cell-command-prev ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (insert "Cell 0")
    (call-interactively #'ein:worksheet-insert-cell-below)
    (insert "Cell 1")
    (call-interactively #'ein:worksheet-merge-cell)
    (ein:cell-goto (ein:worksheet-get-current-cell))
    (should (looking-at "Cell 0\nCell 1"))))


;;; Cell selection.

(ert-deftest ein:notebook-goto-next-input-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (cl-loop for i downfrom 2 to 0
          do (call-interactively #'ein:worksheet-insert-cell-above)
          do (insert (format "Cell %s" i)))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
    ;; (message "%s" (buffer-string))
    (cl-loop for i from 0 below 2
          do (beginning-of-line) ; This is required, I need to check why
          do (should (looking-at (format "Cell %s" i)))
          do (call-interactively #'ein:worksheet-goto-next-input)
          do (should (looking-at (format "Cell %s" (1+ i)))))))

(ert-deftest ein:notebook-goto-prev-input-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (cl-loop for i from 0 below 3
          do (call-interactively #'ein:worksheet-insert-cell-below)
          do (insert (format "Cell %s" i)))
    (should (equal (ein:worksheet-ncells ein:%worksheet%) 3))
    ;; (message "%s" (buffer-string))
    (cl-loop for i downfrom 2 to 1
          do (beginning-of-line) ; This is required, I need to check why
          do (should (looking-at (format "Cell %s" i)))
          do (call-interactively #'ein:worksheet-goto-prev-input)
          do (should (looking-at (format "Cell %s" (1- i)))))))

(defun ein:testing-beginning-of-cell-input (num-cells
                                            initial-point
                                            before-callback
                                            arg
                                            should-looking-at-this)
  (with-current-buffer (ein:testing-notebook-make-empty)
    (ein:testing-insert-cells-with-format num-cells)
    (goto-char (point-min))
    (search-forward initial-point)
    (when before-callback (funcall before-callback))
    (should-not (looking-at-p should-looking-at-this))
    (ein:worksheet-beginning-of-cell-input arg)
    (should (looking-at-p should-looking-at-this))))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-no-arg ()
  (ein:testing-beginning-of-cell-input 1 "Cell 0" nil nil "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-arg-two ()
  (ein:testing-beginning-of-cell-input 2 "Cell 1" nil 2 "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-arg-minus-one ()
  (ein:testing-beginning-of-cell-input 2 "Cell 0" nil -1 "Cell 1"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-no-arg-at-footer ()
  (ein:testing-beginning-of-cell-input 1 "Cell 0"
                                       #'forward-line
                                       nil "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-arg-two-at-footer ()
  (ein:testing-beginning-of-cell-input 2 "Cell 1"
                                       #'forward-line
                                       2 "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-arg-minus-one-at-footer
    ()
  (ein:testing-beginning-of-cell-input 2 "Cell 0"
                                       #'forward-line
                                        -1 "Cell 1"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-no-arg-at-prompt ()
  (ein:testing-beginning-of-cell-input 2 "Cell 1"
                                       (lambda () (forward-line -1))
                                       nil "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-arg-two-at-prompt ()
  (ein:testing-beginning-of-cell-input 2 "Cell 1"
                                       (lambda () (forward-line -1))
                                       2 "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-with-arg-minus-one-at-prompt
    ()
  (ein:testing-beginning-of-cell-input 2 "Cell 0"
                                       ;; I need two cells to make it fail
                                       ;; without (forward-line -1)
                                       (lambda () (forward-line -1))
                                       -1 "Cell 0"))

(ert-deftest ein:worksheet-beginning-of-cell-input-repeat ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (ein:testing-insert-cells-with-format 3)
    (goto-char (point-min))
    (search-forward "Cell 2")
    (should-not (looking-at-p "Cell 2"))
    (ein:worksheet-beginning-of-cell-input)
    (should (looking-at-p "Cell 2"))
    (should-not (looking-at-p "Cell 1"))
    (ein:worksheet-beginning-of-cell-input)
    (should (looking-at-p "Cell 1"))
    (should-not (looking-at-p "Cell 0"))
    (ein:worksheet-beginning-of-cell-input)
    (should (looking-at-p "Cell 0"))))

(defun ein:testing-end-of-cell-input (num-cells
                                      initial-point
                                      before-callback
                                      arg
                                      should-looking-back-this)
  (with-current-buffer (ein:testing-notebook-make-empty)
    (ein:testing-insert-cells-with-format num-cells)
    (goto-char (point-min))
    (search-forward initial-point)
    (beginning-of-line)
    (when before-callback (funcall before-callback))
    (should-not (looking-back should-looking-back-this))
    (ein:worksheet-end-of-cell-input arg)
    (should (looking-back should-looking-back-this))))

(ert-deftest ein:worksheet-end-of-cell-input-with-no-arg ()
  (ein:testing-end-of-cell-input 1 "Cell 0" nil nil "Cell 0"))

(ert-deftest ein:worksheet-end-of-cell-input-with-arg-two ()
  (ein:testing-end-of-cell-input 2 "Cell 0" nil 2 "Cell 1"))

(ert-deftest ein:worksheet-end-of-cell-input-with-arg-minus-one ()
  (ein:testing-end-of-cell-input 2 "Cell 1" nil -1 "Cell 0"))

(ert-deftest ein:worksheet-end-of-cell-input-with-no-arg-at-footer ()
  (ein:testing-end-of-cell-input 2 "Cell 0"
                                 #'forward-line
                                 nil "Cell 1"))

(ert-deftest ein:worksheet-end-of-cell-input-with-arg-two-at-footer ()
  (ein:testing-end-of-cell-input 3 "Cell 0"
                                 #'forward-line
                                 2 "Cell 2"))

(ert-deftest ein:worksheet-end-of-cell-input-with-arg-minus-one-at-footer
    ()
  (ein:testing-end-of-cell-input 2 "Cell 0"
                                 #'forward-line
                                 -1 "Cell 0"))

(ert-deftest ein:worksheet-end-of-cell-input-with-no-arg-at-prompt ()
  (ein:testing-end-of-cell-input 2 "Cell 1"
                                 (lambda () (forward-line -1))
                                 nil "Cell 1"))

(ert-deftest ein:worksheet-end-of-cell-input-with-arg-two-at-prompt ()
  (ein:testing-end-of-cell-input 3 "Cell 0"
                                 (lambda () (forward-line -1))
                                 2 "Cell 1"))

(ert-deftest ein:worksheet-end-of-cell-input-with-arg-minus-one-at-prompt
    ()
  (ein:testing-end-of-cell-input 2 "Cell 1"
                                 (lambda () (forward-line -1))
                                 -1 "Cell 0"))


;;; Cell movement

(ert-deftest ein:notebook-move-cell-up-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (cl-loop for i from 0 below 3
          do (call-interactively #'ein:worksheet-insert-cell-below)
          do (insert (format "Cell %s" i)))
    (beginning-of-line)
    (should (looking-at "Cell 2"))
    (cl-loop repeat 2
          do (call-interactively #'ein:worksheet-move-cell-up))
    ;; (message "%s" (buffer-string))
    (beginning-of-line)
    (should (looking-at "Cell 2"))
    (should (search-forward "Cell 0" nil t))
    (should (search-forward "Cell 1" nil t))
    (should-not (search-forward "Cell 2" nil t))))

(ert-deftest ein:notebook-move-cell-down-command-simple ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (cl-loop for i from 0 below 3
          do (call-interactively #'ein:worksheet-insert-cell-above)
          do (insert (format "Cell %s" i)))
    (cl-loop repeat 2
          do (call-interactively #'ein:worksheet-move-cell-down))
    (beginning-of-line)
    (should (looking-at "Cell 2"))
    (should (search-backward "Cell 0" nil t))
    (should (search-backward "Cell 1" nil t))
    (should-not (search-backward "Cell 2" nil t))))


;;; Cell collapsing and output clearing

(ert-deftest ein:worksheet-toggle-output ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let ((cell (call-interactively #'ein:worksheet-insert-cell-below)))
      (should-not (oref cell :collapsed))
      (call-interactively #'ein:worksheet-toggle-output)
      (should (oref cell :collapsed))
      (call-interactively #'ein:worksheet-toggle-output)
      (should-not (oref cell :collapsed)))))

(defun ein:testing-insert-cells (list-type-or-cell &optional pivot callback)
  (cl-loop with ws = ein:%worksheet%
        with cell = pivot
        for type in list-type-or-cell
        for i from 0
        do (setq cell (ein:worksheet-insert-cell-below ws type cell t))
        if callback
        do (funcall callback i cell)))

(cl-defun ein:testing-insert-cells-with-format (num &optional
                                                    (format "Cell %s")
                                                    (type 'code))
  (ein:testing-insert-cells (cl-loop repeat num collect type)
                            nil
                            (lambda (i &rest _) (insert (format format i))))
  (should (equal (ein:worksheet-ncells ein:%worksheet%) num)))

(defun ein:testing-test-output-visibility-all (collapsed)
  (mapc (lambda (cell) (should (eq (oref cell :collapsed) collapsed)))
        (ein:worksheet-get-cells ein:%worksheet%)))

(ert-deftest ein:worksheet-set-output-visibility-all/visible-from-all-hidden ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    ;; Prepare cells
    (ein:testing-insert-cells '(code code code))
    (mapc (lambda (cell) (ein:cell-set-collapsed cell nil))
          (ein:worksheet-get-cells ein:%worksheet%))
    ;; Call the command
    (call-interactively #'ein:worksheet-set-output-visibility-all)
    ;; Check it worked
    (ein:testing-test-output-visibility-all nil)))

(ert-deftest ein:worksheet-set-output-visibility-all/hidden-from-all-visible ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    ;; Prepare cells
    (ein:testing-insert-cells '(code code code))
    ;; Call the command
    (let ((current-prefix-arg t))
      (call-interactively #'ein:worksheet-set-output-visibility-all))
    ;; Check it worked
    (ein:testing-test-output-visibility-all t)))

(ert-deftest ein:worksheet-set-output-visibility-all/visible-from-part-hidden ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    ;; Prepare cells
    (ein:testing-insert-cells '(code code code))
    (ein:cell-set-collapsed
     (nth 1 (ein:worksheet-get-cells ein:%worksheet%)) nil)
    ;; Call the command
    (call-interactively #'ein:worksheet-set-output-visibility-all)
    ;; Check it worked
    (ein:testing-test-output-visibility-all nil)))

(ert-deftest ein:worksheet-set-output-visibility-all/hidden-from-part-visible ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    ;; Prepare cells
    (ein:testing-insert-cells '(code code code))
    (ein:cell-set-collapsed
     (nth 1 (ein:worksheet-get-cells ein:%worksheet%)) nil)
    ;; Call the command
    (call-interactively #'ein:worksheet-set-output-visibility-all)
    (let ((current-prefix-arg t))
      (call-interactively #'ein:worksheet-set-output-visibility-all))
    ;; Check it worked
    (ein:testing-test-output-visibility-all t)))

(defun ein:testing-assert-cell-output-num (cell num-outputs)
  (should (ein:codecell-p cell))
  (should (= (length (oref cell :outputs)) num-outputs)))

(ert-deftest ein:worksheet-clear-output/simple ()
  (with-current-buffer
      (ein:testing-make-notebook-with-outputs '(("'cell output'")
                                                ("'cell output'")))
    (should (= (ein:worksheet-ncells ein:%worksheet%) 2))
    (let* ((cells (ein:worksheet-get-cells ein:%worksheet%)))
      (ein:testing-assert-cell-output-num (nth 0 cells) 1)
      (ein:testing-assert-cell-output-num (nth 1 cells) 1)
      (ein:cell-goto (nth 0 cells))
      (call-interactively #'ein:worksheet-clear-output)
      (ein:testing-assert-cell-output-num (nth 0 cells) 0)  ; cleared
      (ein:testing-assert-cell-output-num (nth 1 cells) 1))))

(ert-deftest ein:worksheet-clear-output/preserve-input-prompt ()
  (with-current-buffer
      (ein:testing-make-notebook-with-outputs '(("'cell output'")
                                                ("'cell output'")
                                                ("'cell output'")))
    (should (= (ein:worksheet-ncells ein:%worksheet%) 3))
    (let* ((cells (ein:worksheet-get-cells ein:%worksheet%)))
      (ein:cell-set-input-prompt (nth 0 cells) 111)
      (ein:cell-set-input-prompt (nth 1 cells) 222)
      (ein:cell-set-input-prompt (nth 2 cells) 333)
      ;; Call `ein:worksheet-clear-output' with/without prefix argument.
      (ein:cell-goto (nth 0 cells))
      (call-interactively #'ein:worksheet-clear-output)
      (ein:cell-goto (nth 2 cells))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'ein:worksheet-clear-output))
      ;; Check cells' prompt number
      (should (eq (oref (nth 0 cells) :input-prompt-number) nil))
      (should (eq (oref (nth 1 cells) :input-prompt-number) 222))
      (should (eq (oref (nth 2 cells) :input-prompt-number) 333)))))

(ert-deftest ein:worksheet-clear-all-output/simple ()
  (with-current-buffer
      (ein:testing-make-notebook-with-outputs '(("'cell output'")
                                                ("'cell output'")))
    (should (= (ein:worksheet-ncells ein:%worksheet%) 2))
    (let* ((cells (ein:worksheet-get-cells ein:%worksheet%)))
      (ein:testing-assert-cell-output-num (nth 0 cells) 1)
      (ein:testing-assert-cell-output-num (nth 1 cells) 1)
      (call-interactively #'ein:worksheet-clear-all-output)
      (ein:testing-assert-cell-output-num (nth 0 cells) 0)
      (ein:testing-assert-cell-output-num (nth 1 cells) 0))))


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
    (call-interactively #'ein:worksheet-execute-cell))
  (ein:kernel-set-callbacks-for-msg kernel msg-id callbacks))

(ert-deftest ein:notebook-execute-current-cell ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (let* ((text "print 'Hello World'")
           (cell (ein:worksheet-get-current-cell))
           (kernel (ein:$notebook-kernel ein:%notebook%))
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

(defmacro eintest:worksheet-execute-cell-and-*-deftest
  (do-this cell-type has-next-p insert-p)
  "Define:
ein:worksheet-execute-cell-and-{DO-THIS}/on-{CELL-TYPE}cell-{no,with}-next

For example, when `goto-next', \"code\", `nil', `nil' is given,
`ein:worksheet-execute-cell-and-goto-next/on-codecell-no-next' is
defined."
  (let ((test-name
         (intern (format "ein:worksheet-execute-cell-and-%s/on-%scell-%s"
                         do-this cell-type
                         (if has-next-p "with-next" "no-next"))))
        (command
         (intern (format "ein:worksheet-execute-cell-and-%s" do-this))))
    `(ert-deftest ,test-name ()
       (with-current-buffer (ein:testing-notebook-make-empty)
         (let* ((ws ein:%worksheet%)
                (current (ein:worksheet-insert-cell-below ws ,cell-type nil t))
                ,@(when has-next-p
                    '((next
                       (ein:worksheet-insert-cell-below ws "code" current)))))
           (mocker-let ((ein:worksheet-execute-cell
                         (ws cell)
                         (,@(when (equal cell-type "code")
                              '((:input (list ein:%worksheet% current)))))))
             (call-interactively #',command)
             (let ((cell (ein:worksheet-get-current-cell)))
               (should (eq (ein:cell-prev cell) current))
               ,(when has-next-p
                  (if insert-p
                      '(should-not (eq cell next))
                    '(should (eq cell next)))))))))))

(eintest:worksheet-execute-cell-and-*-deftest goto-next    "code"     nil t  )
(eintest:worksheet-execute-cell-and-*-deftest goto-next    "code"     t   nil)
(eintest:worksheet-execute-cell-and-*-deftest goto-next    "markdown" nil t  )
(eintest:worksheet-execute-cell-and-*-deftest goto-next    "markdown" t   nil)
(eintest:worksheet-execute-cell-and-*-deftest insert-below "code"     nil t  )
(eintest:worksheet-execute-cell-and-*-deftest insert-below "code"     t   t  )
(eintest:worksheet-execute-cell-and-*-deftest insert-below "markdown" nil t  )
(eintest:worksheet-execute-cell-and-*-deftest insert-below "markdown" t   t  )


;;; Persistence and loading

(defun ein:testin-notebook-close (num-ws num-ss)
  (should (= num-ws 1))             ; currently EIN only supports 1 WS
  (should (>= num-ss 0))
  (let* ((buf (ein:testing-notebook-make-empty))
         (buffers (with-current-buffer buf
                    (ein:notebook-buffer-list ein:%notebook%))))
    (with-current-buffer buf
      (dotimes (_ num-ss)
        (ein:notebook-scratchsheet-render-new ein:%notebook%))
      (should (= (+ num-ws num-ss)
                 (length (seq-filter
                          (lambda (b) (not (buffer-base-buffer (get-buffer b))))
                          (ein:notebook-buffer-list ein:%notebook%))))))

    (kill-buffer buf)
    (mapc (lambda (b) (should-not (get-buffer b))) buffers)))

(ert-deftest ein:notebook-close/one-ws-no-ss ()
  (ein:testin-notebook-close 1 0))

(ert-deftest ein:notebook-close/one-ws-one-ss ()
  (ein:testin-notebook-close 1 1))

(ert-deftest ein:notebook-close/one-ws-five-ss ()
  (ein:testin-notebook-close 1 5))

(defun ein:testing-notebook-data-assert-nb3-worksheet-contents (notebook &optional text)
  (let* ((data (ein:notebook-to-json notebook))
         (worksheets (assoc-default 'worksheets data #'eq))
         (cells (assoc-default 'cells (elt worksheets 0) #'eq)))
    (should (= (length worksheets) 1))
    (if text
        (let* ((cell-0 (elt cells 0))
               (input (assoc-default 'input cell-0 #'eq)))
          (should (equal input text)))
      (should (= (length cells) 0)))))

(defun ein:testing-notebook-data-assert-nb4-worksheet-contents (notebook &optional text)
  (let* ((data (ein:notebook-to-json notebook))
         (cells (assoc-default 'cells data)))
    (if text
        (progn
          (should (= (length cells) 1))
          (should (equal (assoc-default 'source (elt cells 0) #'eq) text)))
      (should (zerop (length cells))))))

(ert-deftest ein:notebook-saves-latin ()
  (with-current-buffer (ein:testing-notebook-make-new)
    (let ((latin "«utf-8 cannot handle these»"))
      ;; Edit notebook.
      (ein:cell-goto (ein:get-cell-at-point))
      (insert latin)
      (should (ein:notebook-save-notebook (ein:get-notebook))))))

(ert-deftest ein:notebook-to-json-after-closing-a-worksheet ()
  (with-current-buffer (ein:testing-notebook-make-new)
    (let ((notebook ein:%notebook%))
      ;; Edit notebook.
      (ein:cell-goto (ein:get-cell-at-point))
      (insert "some text")
      (if (< (ein:$notebook-nbformat notebook) 4)
          (ein:testing-notebook-data-assert-nb3-worksheet-contents notebook "some text")
        (ein:testing-notebook-data-assert-nb4-worksheet-contents notebook "some text"))
      (should (ein:notebook-modified-p notebook))
      (ein:notebook-save-notebook-success notebook)
      (should-not (ein:notebook-modified-p notebook))
      ;; Kill scratch sheet.
      (kill-buffer (ein:worksheet-buffer (ein:notebook-scratchsheet-open notebook)))
      (should (ein:notebook-live-p notebook))
      ;; to-json should still work
      (if (< (ein:$notebook-nbformat notebook) 4)
          (ein:testing-notebook-data-assert-nb3-worksheet-contents notebook "some text")
        (ein:testing-notebook-data-assert-nb4-worksheet-contents notebook "some text")))))

(ert-deftest ein:notebook-to-json-after-discarding-a-worksheet ()
  (with-current-buffer (ein:testing-notebook-make-new)
    (let (asked
          (buffer (current-buffer))
          (notebook ein:%notebook%))
      ;; Edit notebook.
      (ein:cell-goto (ein:get-cell-at-point))
      (insert "some text")
      (if (< (ein:$notebook-nbformat notebook) 4)
          (ein:testing-notebook-data-assert-nb3-worksheet-contents notebook "some text")
        (ein:testing-notebook-data-assert-nb4-worksheet-contents notebook "some text"))
      (should (ein:notebook-modified-p notebook))
      (let ((ss-buf (save-current-buffer (ein:notebook-scratchsheet-open notebook))))
        ;; Discard a worksheet buffer
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _args) (setq asked t) nil)))
          (should-not (kill-buffer buffer)))
        (should asked)
        (should-not (buffer-live-p buffer))
        (should-not (ein:notebook-live-p notebook))
        (should-not (buffer-live-p ss-buf))))))

(defun ein:testing-notebook-should-be-closed (notebook buffer)
  (should-not (buffer-live-p buffer))
  (should-not (ein:notebook-live-p notebook)))

(ert-deftest ein:notebook-kill-kernel-then-close-when-its-alive ()
  (with-current-buffer (ein:testing-notebook-make-new)
    (let ((buffer (current-buffer))
          (notebook ein:%notebook%))
      (cl-letf (((symbol-function 'ein:kernel-live-p) (lambda (&rest _args) t))
                ((symbol-function 'ein:kernel-delete-session)
                 (cl-function (lambda (callback &key kernel) (funcall callback kernel)))))
        (call-interactively #'ein:notebook-kill-kernel-then-close-command))
      (ein:testing-notebook-should-be-closed notebook buffer))))

(ert-deftest ein:notebook-kill-kernel-then-close-when-already-dead ()
  (with-current-buffer (ein:testing-notebook-make-new)
    (let ((buffer (current-buffer))
          (notebook ein:%notebook%)
          (kernel (ein:$notebook-kernel ein:%notebook%)))
      (mocker-let
          ((ein:kernel-live-p
            (kernel)
            ((:input (list kernel) :output nil))))
        (call-interactively #'ein:notebook-kill-kernel-then-close-command))
      (ein:testing-notebook-should-be-closed notebook buffer))))


;; Notebook undo

(defun eintest:notebook-undo-after-insert-above ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let ((text "some text"))
      (call-interactively #'ein:worksheet-insert-cell-above)
      (insert text)
      (undo-boundary)
      (call-interactively #'ein:worksheet-insert-cell-above)
      (call-interactively #'ein:worksheet-goto-next-input)
      (should (equal (ein:cell-get-text (ein:worksheet-get-current-cell)) text))
      (if ein:worksheet-enable-undo
          (undo)
        (should-error (undo)))
      (when ein:worksheet-enable-undo
        (should (equal (buffer-string) "
In [ ]:


In [ ]:


"))))))

(defun eintest:notebook-undo-after-split ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let ((line-1 "first line")
          (line-2 "second line"))
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert line-1 "\n" line-2)
      (undo-boundary)
      (move-beginning-of-line 1)
      (call-interactively #'ein:worksheet-split-cell-at-point)
      (undo-boundary)
      (should (equal (ein:cell-get-text (ein:worksheet-get-current-cell))
                     line-2))
      (if ein:worksheet-enable-undo
          (undo)
        (should-error (undo)))
      (when ein:worksheet-enable-undo
        (should (equal (buffer-string) "
In [ ]:


In [ ]:
first line
second line

"))))))

(defun eintest:notebook-undo-after-merge ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let ((line-1 "first line")
          (line-2 "second line"))
      (call-interactively #'ein:worksheet-insert-cell-below)
      (call-interactively #'ein:worksheet-insert-cell-below)
      ;; Extra cells to avoid "Changes to be undone are outside visible
      ;; portion of buffer" user-error:
      (call-interactively #'ein:worksheet-insert-cell-below)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (goto-char (point-min))
      (call-interactively #'ein:worksheet-goto-next-input)

      (insert line-1)
      (undo-boundary)

      (call-interactively #'ein:worksheet-goto-next-input)
      (insert line-2)
      (undo-boundary)

      (call-interactively #'ein:worksheet-merge-cell)
      (undo-boundary)

      (should (equal (ein:cell-get-text (ein:worksheet-get-current-cell))
                     (concat line-1 "\n" line-2)))
      (if (not ein:worksheet-enable-undo)
          (should-error (undo))
        (undo)
        (should (equal (buffer-string) "
In [ ]:
second line

In [ ]:


In [ ]:


"))
        (undo-more 1)
        (should (equal (buffer-string) "
In [ ]:


In [ ]:


In [ ]:


"))))))

(defun eintest:notebook-undo-after-execution-1-cell ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (let* ((text "print 'Hello World'")
           (output-text "Hello World\n")
           (cell (ein:worksheet-get-current-cell))
           (kernel (ein:$notebook-kernel ein:%notebook%))
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
      (if ein:worksheet-enable-undo
          (undo)
        (should-error (undo)))
      (when ein:worksheet-enable-undo
        (should (equal (ein:cell-get-text cell) ""))
        (should (funcall check-output))))))

(defun eintest:notebook-undo-after-execution-2-cells ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (call-interactively #'ein:worksheet-insert-cell-below)
    (call-interactively #'ein:worksheet-insert-cell-above)
    (let* ((text "print 'Hello World\\n' * 10")
           (next-text "something")
           (output-text
            (apply #'concat (cl-loop repeat 10 collect "Hello World\n")))
           (cell (ein:worksheet-get-current-cell))
           (next-cell (ein:cell-next cell))
           (kernel (ein:$notebook-kernel ein:%notebook%))
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
        (call-interactively #'ein:worksheet-goto-next-input)
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
      (if ein:worksheet-enable-undo
          (undo)
        (should-error (undo)))
      (when ein:worksheet-enable-undo
        (should (equal (ein:cell-get-text cell) text))
        (should (equal (ein:cell-get-text next-cell) ""))
        (should (funcall check-output))))))

(defmacro eintest:notebook-undo-make-tests (name)
  "Define two tests ein:NANE/no, ein:NANE/yes
from a function named eintest:NAME where `no'/`yes' is the
value of `ein:worksheet-enable-undo'."
  (let ((func (intern (format "eintest:%s" name)))
        (test/no (intern (format "ein:%s/no" name)))
        (test/yes (intern (format "ein:%s/yes" name))))
    `(progn
       (ert-deftest ,test/no ()
         (let ((ein:worksheet-enable-undo nil))
           (,func)))
       (ert-deftest ,test/yes ()
         (let ((ein:worksheet-enable-undo t))
           (,func)))
       )))

(eintest:notebook-undo-make-tests notebook-undo-after-insert-above)
(eintest:notebook-undo-make-tests notebook-undo-after-split)
(eintest:notebook-undo-make-tests notebook-undo-after-merge)
(eintest:notebook-undo-make-tests notebook-undo-after-execution-1-cell)
(eintest:notebook-undo-make-tests notebook-undo-after-execution-2-cells)


;; Generic getter

(ert-deftest ein:get-url-or-port--notebook ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (should (equal (ein:get-url-or-port) ein:testing-notebook-dummy-url))))

(ert-deftest ein:get-notebook--notebook ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (should (eq (ein:get-notebook) ein:%notebook%))))

(ert-deftest ein:get-kernel--notebook ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    (let ((kernel (ein:$notebook-kernel ein:%notebook%)))
      (should (ein:$kernel-p kernel))
      (should (eq (ein:get-kernel) kernel)))))

(ert-deftest ein:get-cell-at-point--notebook ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    ;; FIXME: write test with non-empty worksheet
    (should-not (ein:get-cell-at-point))))

(ert-deftest ein:get-traceback-data--notebook ()
  (with-current-buffer (ein:testing-notebook-make-empty)
    ;; FIXME: write test with non-empty TB
    (should-not (ein:get-traceback-data))))


;; Notebook mode

(ert-deftest ein:notebook-ask-before-kill-emacs-simple ()
  (let ((ein:notebook--opened-map (make-hash-table :test 'equal)))
    (with-current-buffer
        (ein:testing-notebook-make-empty "Modified Notebook.ipynb")
      (call-interactively #'ein:worksheet-insert-cell-below)
      (should (ein:notebook-modified-p)))
    (with-current-buffer
        (ein:testing-notebook-make-empty "Saved Notebook.ipynb")
      (ein:notebook-save-notebook-success ein:%notebook%)
      (should-not (ein:notebook-modified-p)))
    (kill-buffer (ein:testing-notebook-make-empty "Killed Notebook.ipynb"))
    (should (gethash `(,ein:testing-notebook-dummy-url "Modified Notebook.ipynb") ein:notebook--opened-map))
    (should (gethash `(,ein:testing-notebook-dummy-url "Saved Notebook.ipynb") ein:notebook--opened-map))
    (should-not (gethash `(,ein:testing-notebook-dummy-url "Killed Notebook.ipynb") ein:notebook--opened-map))
    (should (= (hash-table-count ein:notebook--opened-map) 2))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) nil)))
      (ein:notebook-close-notebooks t)
      (should-not (ein:notebook-opened-notebooks)))))

;;; Buffer and kill hooks

(ert-deftest ein:notebook-ask-before-kill-buffer/no-ein-buffer ()
  (with-temp-buffer
    (should (kill-buffer))))

(ert-deftest ein:notebook-ask-before-kill-buffer/new-notebook ()
  (with-current-buffer (ein:testing-make-notebook-with-outputs '(nil))
    (let (asked
          (buf (current-buffer)))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) (setq asked t) nil)))
        (should-not (kill-buffer buf))
        (should-not asked)
        (should-not (buffer-live-p buf))))))

(ert-deftest ein:notebook-ask-before-kill-buffer/modified-notebook ()
  (with-current-buffer (ein:testing-make-notebook-with-outputs '(nil))
    (let (asked
          (buffer (current-buffer)))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) (setq asked t) nil)))
        (call-interactively #'ein:worksheet-insert-cell-below)
        (should-not (kill-buffer buffer))
        (should asked)
        (should-not (buffer-live-p buffer))))))

(ert-deftest ein:notebook-ask-before-kill-buffer/modified-scratchsheet ()
  (with-current-buffer (ein:testing-make-notebook-with-outputs '(nil))
    (let (asked
          (buf (current-buffer)))
      (should (buffer-live-p buf))
      (with-current-buffer (ein:worksheet-buffer
                            (ein:notebook-scratchsheet-open ein:%notebook%))
        (let (asked
              (buf2 (current-buffer)))
          (should-not (eq buf buf2))
          (should (= (ein:worksheet-ncells ein:%worksheet%) 1))
          (call-interactively #'ein:worksheet-insert-cell-below)
          (should (= (ein:worksheet-ncells ein:%worksheet%) 2))
          (should (ein:worksheet-modified-p ein:%worksheet%))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _args) (setq asked t) nil)))
            (should-not (kill-buffer buf2)))
          (should-not asked)
          (should-not (buffer-live-p buf2))))
      (should-not (ein:worksheet-modified-p ein:%worksheet%))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) (setq asked t) nil)))
        (should-not (kill-buffer buf)))
      (should-not asked)
      (should-not (buffer-live-p buf)))))

;; Misc unit tests

(ert-deftest ein:notebook-test-notebook-name-simple ()
  (should-not (ein:notebook-test-notebook-name nil))
  (should-not (ein:notebook-test-notebook-name ""))
  (should-not (ein:notebook-test-notebook-name "/"))
  (should-not (ein:notebook-test-notebook-name "\\"))
  (should-not (ein:notebook-test-notebook-name ":"))
  (should-not (ein:notebook-test-notebook-name "a/b"))
  (should-not (ein:notebook-test-notebook-name "a\\b"))
  (should-not (ein:notebook-test-notebook-name "a:b"))
  (should (ein:notebook-test-notebook-name "This is a OK notebook name")))

(cl-defun eintest:notebook--check-nbformat (&optional orig_nbformat
                                                      orig_nbformat_minor
                                                      nbformat
                                                      nbformat_minor
                                                      &key data)
  (let ((data (or data
                  (list :nbformat nbformat :nbformat_minor nbformat_minor
                        :orig_nbformat orig_nbformat
                        :orig_nbformat_minor orig_nbformat_minor))))
    (ein:notebook--check-nbformat data)))

(ert-deftest ein:notebook--check-nbformat-nothing ()
  (mocker-let ((ein:display-warning (message) ()))
    (eintest:notebook--check-nbformat)
    (eintest:notebook--check-nbformat :data nil)
    (eintest:notebook--check-nbformat 2 0)
    (eintest:notebook--check-nbformat 2 0 2)
    (eintest:notebook--check-nbformat 2 0 2 0)))

(defmacro ein:notebook--check-nbformat-assert-match (regexp &rest args)
  `(mocker-let ((ein:display-warning
                 (message)
                 ((:input-matcher
                   (lambda (m) (string-match ,regexp m))))))
     (eintest:notebook--check-nbformat ,@args)))

(ert-deftest ein:notebook--check-nbformat-warn-major ()
  (ein:notebook--check-nbformat-assert-match "v2 -> v3" 2 nil 3)
  (ein:notebook--check-nbformat-assert-match "v2 -> v3" 2 0 3 0))

(ert-deftest ein:notebook--check-nbformat-warn-minor ()
  (ein:notebook--check-nbformat-assert-match
   "version v2\\.1, [^\\.]* up to v2.0" 2 1 2 0))
