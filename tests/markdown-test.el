;;;; markdown-test.el --- Tests for markdown-mode

;; Copyright (C) 2013-2015 Jason R. Blevins <jrblevin@sdf.org>
;; Copyright (C) 2013 Makoto Motohashi <mkt.motohashi@gmail.com>
;; Copyright (C) 2015 Google, Inc. (Contributor: Samuel Freilich <sfreilich@google.com>)

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains the `markdown-mode' test suite.  To run the tests:
;;
;;     M-x load-file RET markdown-test.el RET
;;     M-x markdown-test RET

;;; Code:

(require 'markdown-mode)
(require 'ert)
(require 'cl-lib)

(defconst markdown-test-dir
  (expand-file-name (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst markdown-test-font-lock-function
  (if (and noninteractive (fboundp 'font-lock-ensure))
      #'font-lock-ensure #'font-lock-fontify-buffer))

(defmacro markdown-test-string-mode (mode string &rest body)
  "Run BODY in a temporary buffer containing STRING in MODE."
  (declare (indent 2))
  `(let ((win (selected-window)))
     (unwind-protect
         (with-temp-buffer
           (set-window-buffer win (current-buffer) t)
           (erase-buffer)
           (funcall ,mode)
           (setq-default indent-tabs-mode nil)
           (insert ,string)
           (goto-char (point-min))
           (funcall markdown-test-font-lock-function)
           (prog1 ,@body (kill-buffer))))))

(defmacro markdown-test-file-mode (mode file &rest body)
  "Open FILE from `markdown-test-dir' in MODE and execute BODY."
  (declare (indent 2))
  `(let ((fn (concat markdown-test-dir ,file)))
     (save-window-excursion
       (with-temp-buffer
         (insert-file-contents fn)
         (funcall ,mode)
         (goto-char (point-min))
         (funcall markdown-test-font-lock-function)
         ,@body))))

(defmacro markdown-test-string (string &rest body)
  "Run body in a temporary buffer containing STRING in `markdown-mode'."
  (declare (indent 1))
  `(markdown-test-string-mode 'markdown-mode ,string ,@body))
(def-edebug-spec markdown-test-string (form body))

(defmacro markdown-test-file (file &rest body)
  "Open FILE from `markdown-test-dir' in `markdown-mode' and execute BODY."
  (declare (indent 1))
  `(markdown-test-file-mode 'markdown-mode ,file ,@body))
(def-edebug-spec markdown-test-file (form body))

(defmacro markdown-test-string-gfm (string &rest body)
  "Run body in a temporary buffer containing STRING in `gfm-mode'."
  (declare (indent 1))
  `(markdown-test-string-mode 'gfm-mode ,string ,@body))
(def-edebug-spec markdown-test-string-gfm (form body))

(defmacro markdown-test-file-gfm (file &rest body)
  "Open FILE from `markdown-test-dir' in `gfm-mode' and execute BODY."
  (declare (indent 1))
  `(markdown-test-file-mode 'gfm-mode ,file ,@body))
(def-edebug-spec markdown-test-file-gfm (form body))

(defmacro markdown-test-temp-file (file &rest body)
  "Open FILE from `markdown-test-dir' visiting temp file and execute body.
This file is not saved."
  (declare (indent 1))
  `(let ((fn (concat markdown-test-dir ,file))
         (tmp (make-temp-file "markdown-test" nil ".text"))
         buf)
     (save-window-excursion
       (unwind-protect
           (progn
             (setq buf (find-file tmp))
             (insert-file-contents fn)
             (markdown-mode)
             (goto-char (point-min))
             (funcall markdown-test-font-lock-function)
             ,@body
             (set-buffer-modified-p nil))
         (when (buffer-live-p buf) (kill-buffer buf))
         (delete-file tmp)))))
(def-edebug-spec markdown-test-temp-file (form body))

(defun markdown-test-report-property-range (begin end prop)
  "Report buffer substring and property PROP from BEGIN to END."
  (message "Buffer substring: %s" (buffer-substring begin (1+ end)))
  (message "Properties in range are as follows:")
  (dolist (loc (number-sequence begin end))
    (message "%d: %s" loc (get-char-property loc prop))))

(defun markdown-test-range-has-property (begin end prop value)
  "Verify that range BEGIN to END has PROP equal to or containing VALUE."
  (let (vals fail-loc)
    (setq fail-loc
          (catch 'fail
            (dolist (loc (number-sequence begin end))
              (setq vals (get-char-property loc prop))
              (if (and vals (listp vals))
                  (unless (memq value vals)
                    (throw 'fail loc))
                (unless (eq vals value)
                  (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (markdown-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun markdown-test-range-property-equals (begin end prop value)
  "Verify that range BEGIN to END has property PROP equal to VALUE."
  (let ((fail-loc
         (catch 'fail
           (dolist (loc (number-sequence begin end))
             (unless (eq (get-char-property loc prop) value)
               (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (markdown-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun markdown-test-range-has-face (begin end face)
  "Verify that the range from BEGIN to END has face FACE."
  (markdown-test-range-has-property begin end 'face face))

(defun markdown-test-range-face-equals (begin end face)
  "Verify that the range from BEGIN to END has face equal to FACE."
  (markdown-test-range-property-equals begin end 'face face))

(defun markdown-test-goto-heading (title)
  "Move the point to section with TITLE."
  (let ((regexp (format "\\(^#+ %s\\( #+\\)?\\|^%s\n[=-]+\n\\)" title title)))
    (if (re-search-forward regexp nil t)
        (goto-char (match-end 0)))))

(defun markdown-test ()
  "Run all defined tests for `markdown-mode'."
  (interactive)
  (ert "markdown"))

;;; Example tests:

(ert-deftest test-markdown-example/string ()
  "An example string test using the `ert' framework."
  (markdown-test-string "foo *bar* baz"
                        (goto-char 5)
                        (delete-char 1)
                        (should (looking-at "bar"))))

(ert-deftest test-markdown-example/file ()
  "An example file test using the `ert' framework."
  (markdown-test-file "inline.text"
                      (goto-char 9)
                      (should (looking-at "\*"))))

;;; Basic mode tests:

(ert-deftest test-markdown-mode/variables ()
  "Test `markdown-mode' variables."
  (markdown-test-file "inline.text"
                      (should (= tab-width 4))
                      (should (eq font-lock-multiline t))
                      (should (eq major-mode 'markdown-mode))))

;;; Element insertion tests:

(ert-deftest test-markdown-insertion/blank-line-before-1 ()
  "Test function `markdown-ensure-blank-line-before' at beginning of line."
  (markdown-test-file "syntax.text"
                      (search-forward "as plain text")
                      (should (= (point) 1556))
                      (beginning-of-line)
                      (should (= (point) 1505))
                      (should (looking-back "A Markdown-formatted\n" nil))
                      (should (not (markdown-prev-line-blank-p)))
                      (markdown-ensure-blank-line-before)
                      (should (looking-back "A Markdown-formatted\n\n" nil))
                      (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-2 ()
  "Test function `markdown-ensure-blank-line-before' in middle of line."
  (markdown-test-file "syntax.text"
                      (search-forward "as plain text")
                      (should (= (point) 1556))
                      (should (looking-back "as plain text" nil))
                      (should (not (markdown-prev-line-blank-p)))
                      (markdown-ensure-blank-line-before)
                      (should (looking-back "as plain text\n\n" nil))
                      (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-3 ()
  "Test function `markdown-ensure-blank-line-before' with blank line before."
  (markdown-test-file "syntax.text"
                      (search-forward "web.\n\nMarkdown is not a replacement for HTML")
                      (beginning-of-line)
                      (should (= (point) 2704))
                      (should (looking-back "web.\n\n" nil))
                      (should (markdown-prev-line-blank-p))
                      (markdown-ensure-blank-line-before)
                      (should (= (point) 2704))
                      (should (looking-back "web.\n\n" nil))
                      (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-4 ()
  "Test function `markdown-ensure-blank-line-before' at beginning of buffer."
  (markdown-test-string "first line"
                        (beginning-of-line)
                        (should (bobp))
                        (should (= (point-max) 11))
                        (markdown-ensure-blank-line-before)
                        (should (= (point-max) 11))
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "first line"))
                        (forward-word)
                        (markdown-ensure-blank-line-before)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "first\n\n line"))))

(ert-deftest test-markdown-insertion/blank-line-after-1 ()
  "Test function `markdown-ensure-blank-line-after' at end of line."
  (markdown-test-file "syntax.text"
                      (search-forward "as plain text")
                      (should (= (point) 1556))
                      (end-of-line)
                      (should (= (point) 1573))
                      (should (looking-at "\nlike it's been"))
                      (should (not (markdown-next-line-blank-p)))
                      (markdown-ensure-blank-line-after)
                      (should (looking-at "\n\nlike it's been"))
                      (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-after-2 ()
  "Test function `markdown-ensure-blank-line-after' in middle of line."
  (markdown-test-file "syntax.text"
                      (search-forward "as plain text")
                      (should (= (point) 1556))
                      (should (looking-at ", without looking"))
                      (should (not (markdown-next-line-blank-p)))
                      (markdown-ensure-blank-line-after)
                      (should (looking-at "\n\n, without looking"))
                      (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-after-3 ()
  "Test function `markdown-ensure-blank-line-after' with blank line after."
  (markdown-test-file "syntax.text"
                      (search-forward "*writing* for the web.")
                      (should (= (point) 2702))
                      (should (looking-at "\n\nMarkdown is not a replacement for HTML"))
                      (should (markdown-next-line-blank-p))
                      (markdown-ensure-blank-line-after)
                      (should (= (point) 2702))
                      (should (looking-at "\n\nMarkdown is not a replacement for HTML"))
                      (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-after-4 ()
  "Test function `markdown-ensure-blank-line-after' at end of buffer."
  (markdown-test-string "last line"
                        (end-of-line)
                        (should (eobp))
                        (should (= (point-max) 10))
                        (markdown-ensure-blank-line-after)
                        (should (= (point-max) 10))
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "last line"))
                        (backward-word)
                        (markdown-ensure-blank-line-after)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "last \n\nline"))))

(ert-deftest test-markdown-insertion/point-after-unwrap ()
  "Test new point position calculations after unwrap operations."
  (markdown-test-string "line **one**\n"
                        (let ((prefix (cons 6 8)) (suffix (cons 11 13)))
                          ;; Prefix
                          (should (eq (markdown-point-after-unwrap 6 prefix suffix) 6))
                          (should (eq (markdown-point-after-unwrap 7 prefix suffix) 6))
                          ;; Word
                          (should (eq (markdown-point-after-unwrap 8 prefix suffix) 6))
                          (should (eq (markdown-point-after-unwrap 9 prefix suffix) 7))
                          (should (eq (markdown-point-after-unwrap 10 prefix suffix) 8))
                          ;; Suffix
                          (should (eq (markdown-point-after-unwrap 11 prefix suffix) 9))
                          (should (eq (markdown-point-after-unwrap 12 prefix suffix) 9))
                          ;; Immediately after
                          (should (eq (markdown-point-after-unwrap 13 prefix suffix) 9))))
  (markdown-test-string "line _one_\n"
                        (let ((prefix (cons 6 7)) (suffix (cons 10 11)))
                          ;; Prefix
                          (should (eq (markdown-point-after-unwrap 6 prefix suffix) 6))
                          ;; Word
                          (should (eq (markdown-point-after-unwrap 7 prefix suffix) 6))
                          (should (eq (markdown-point-after-unwrap 8 prefix suffix) 7))
                          (should (eq (markdown-point-after-unwrap 9 prefix suffix) 8))
                          ;; Suffix
                          (should (eq (markdown-point-after-unwrap 10 prefix suffix) 9))
                          ;; Immediately after
                          (should (eq (markdown-point-after-unwrap 10 prefix suffix) 9)))))

(ert-deftest test-markdown-insertion/unwrap-thing-at-point-italic ()
  "Test function `markdown-unwrap-thing-at-point' on italics."
  (markdown-test-file "syntax.text"
                      ;; Unwrap *not*
                      (goto-char 2859)
                      (should (thing-at-point-looking-at markdown-regex-italic))
                      (should (equal (markdown-unwrap-thing-at-point
                                      markdown-regex-italic 1 3)
                                     (cons 2859 2862)))
                      (should (= (point) 2859))
                      ;; Unwrap *publishing*
                      (goto-char 3064)
                      (should (thing-at-point-looking-at markdown-regex-italic))
                      (should (equal (markdown-unwrap-thing-at-point
                                      markdown-regex-italic 1 3)
                                     (cons 3060 3070)))
                      (should (= (point) 3063))
                      ;; Unwrap *writing*
                      (goto-char 3101)
                      (should (thing-at-point-looking-at markdown-regex-italic))
                      (should (equal (markdown-unwrap-thing-at-point
                                      markdown-regex-italic 1 3)
                                     (cons 3093 3100)))
                      (should (= (point) 3100))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-italic ()
  "Test function `markdown-unwrap-things-in-region' on italics."
  (markdown-test-file "syntax.text"
                      (should (equal (markdown-unwrap-things-in-region
                                      2704 3207 markdown-regex-italic 1 3)
                                     (cons 2704 3201)))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-bound ()
  "Ensure that `markdown-unwrap-things-in-region' respects end bound"
  (markdown-test-string "**a** **b** **c** **d** **e** **f**"
                        ;; Set region to unrwap a, b, c, and d only.  If endpoint is not
                        ;; respected (i.e, not adjusted for character removal), the
                        ;; function will unwrap e and f also.
                        (should (equal (markdown-unwrap-things-in-region
                                        1 24 markdown-regex-bold 2 4)
                                       (cons 1 8)))
                        (should (string-equal (buffer-string) "a b c d **e** **f**"))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-links ()
  "Test function `markdown-unwrap-things-in-region' on inline links."
  (markdown-test-string "a [link](http://jblevins.org/) or [two](/).\n"
                        (should (equal (markdown-unwrap-things-in-region
                                        (point-min) (point-max) markdown-regex-link-inline 0 3)
                                       (cons 1 16)))
                        (should (string-equal (buffer-string) "a link or two.\n"))))

(ert-deftest test-markdown-insertion/toggle-bold ()
  "Test toggling functionality of `markdown-insert-bold'."
  (markdown-test-string "one **two** three"
                        (forward-word 2)
                        (markdown-insert-bold)
                        (should (string-equal (buffer-string) "one two three"))
                        (should (= (point) 8))
                        (forward-word)
                        (markdown-insert-bold)
                        (should (= (point) 16))
                        (should (string-equal (buffer-string) "one two **three**"))))

(ert-deftest test-markdown-insertion/toggle-italic ()
  "Test toggling functionality of `markdown-insert-italic'."
  (markdown-test-string "one *two* three"
                        (forward-word 2)
                        (markdown-insert-italic)
                        (should (string-equal (buffer-string) "one two three"))
                        (should (= (point) 8))
                        (forward-word)
                        (markdown-insert-italic)
                        (should (string-equal (buffer-string) "one two *three*"))
                        (should (= (point) 15))))

(ert-deftest test-markdown-insertion/toggle-code ()
  "Test toggling functionality of `markdown-insert-code'."
  (markdown-test-string "one `two` three"
                        (forward-word 2)
                        (markdown-insert-code)
                        (should (string-equal (buffer-string) "one two three"))
                        (should (= (point) 8))
                        (forward-word)
                        (markdown-insert-code)
                        (should (string-equal (buffer-string) "one two `three`"))
                        (should (= (point) 15))))

(ert-deftest test-markdown-insertion/toggle-kbd ()
  "Test toggling functionality of `markdown-insert-code'."
  (markdown-test-string "test <kbd>C-c C-s k</kbd> toggle"
                        (forward-word 2)
                        (markdown-insert-kbd)
                        (should (string-equal (buffer-string) "test C-c C-s k toggle"))
                        (should (= (point) 6))
                        (backward-word)
                        (markdown-insert-kbd)
                        (should (string-equal (buffer-string) "<kbd>test</kbd> C-c C-s k toggle"))
                        (should (= (point) 6))))

(ert-deftest test-markdown-insertion/toggle-wiki-link-alias-first ()
  "Test toggling of `markdown-insert-wiki-link' with alias first.
Test point position upon removal and insertion."
  (let ((markdown-wiki-link-alias-first t))
    (markdown-test-string "[[text|page]]"
                          (goto-char 5) ; point in interior of alias text, at 'x'
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 3)) ; leave point at, at 'x'
                          (should (string-equal (buffer-string) "text"))
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 5)) ; leave point at, at 'x'
                          (should (string-equal (buffer-string) "[[text]]")))
    (markdown-test-string "[[text|page]]"
                          (goto-char 10) ; point in interior of link text, at 'g'
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 5)) ; leave point at end of alias text
                          (should (string-equal (buffer-string) "text"))
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 7)) ; leave point at end of alias text
                          (should (string-equal (buffer-string) "[[text]]")))))

(ert-deftest test-markdown-insertion/toggle-wiki-link-alias-last ()
  "Test toggling of `markdown-insert-wiki-link' with alias last.
Test point position upon removal and insertion."
  (let ((markdown-wiki-link-alias-first nil))
    (markdown-test-string "[[page|text]]"
                          (goto-char 10) ; point in interior of alias text, at 'x'
                          (call-interactively 'markdown-insert-wiki-link)
                          (goto-char 3) ; leave point at, at 'x'
                          (should (string-equal (buffer-string) "text"))
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 5)) ; leave point at, at 'x'
                          (should (string-equal (buffer-string) "[[text]]")))
    (markdown-test-string "[[page|text]]"
                          (goto-char 3) ; point in interior of link text, at 'g'
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 1)) ; leave point at beginning of alias text
                          (should (string-equal (buffer-string) "text"))
                          (call-interactively 'markdown-insert-wiki-link)
                          (should (= (point) 3)) ; leave point at beginning of alias text
                          (should (string-equal (buffer-string) "[[text]]")))))

(ert-deftest test-markdown-insertion/bold-region ()
  "Test region functionality of `markdown-insert-bold'."
  (markdown-test-string "one two three"
                        (push-mark (point) t t)
                        (forward-word 2)
                        (markdown-insert-bold)
                        (should (string-equal (buffer-string) "**one two** three"))
                        (should (= (point) 10))))

(ert-deftest test-markdown-insertion/italic-region ()
  "Test region functionality of `markdown-insert-italic'."
  (markdown-test-string "one two three"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (forward-word 2)
                        (markdown-insert-italic)
                        (should (string-equal (buffer-string) "*one two* three"))
                        (should (= (point) 9))))

(ert-deftest test-markdown-insertion/code-region ()
  "Test region functionality of `markdown-insert-code'."
  (markdown-test-string "one two three"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (forward-word 2)
                        (markdown-insert-code)
                        (should (string-equal (buffer-string) "`one two` three"))
                        (should (= (point) 9))))

(ert-deftest test-markdown-insertion/kbd-region ()
  "Test region functionality of `markdown-insert-kbd'."
  (markdown-test-string "one two three"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (forward-word 2)
                        (markdown-insert-kbd)
                        (should (string-equal (buffer-string) "<kbd>one two</kbd> three"))
                        (should (= (point) 13))))

(ert-deftest test-markdown-insertion/atx-line ()
  "Test ATX header insertion without region."
  (markdown-test-string "line one\nline two\n"
                        (forward-word)
                        (markdown-insert-header-atx-1)
                        (should (= (point) 11))
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "# line one #\n\nline two\n"))
                        (forward-line 2)
                        (markdown-insert-header-atx-2)
                        (should (= (point) 26))
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "# line one #\n\n## line two ##\n\n"))))

(ert-deftest test-markdown-insertion/atx-region ()
  "Test ATX header insertion with region."
  (markdown-test-string "line one\nline two\n"
                        (transient-mark-mode)
                        (forward-char 5)
                        (push-mark (point) t t)
                        (forward-word)
                        (should (string-equal (buffer-substring (region-beginning) (region-end))
                                              "one"))
                        (markdown-insert-header-atx-4)
                        (should (= (point) 16))
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line \n\n#### one ####\n\nline two\n"))))

(ert-deftest test-markdown-insertion/atx-blank ()
  "Test ATX header insertion on blank line."
  (markdown-test-string "line one\n\nline two\n"
                        (forward-line)
                        (markdown-insert-header-atx-3)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line one\n\n###  ###\n\nline two\n"))
                        (should (= (point) 15))
                        (should (looking-at " ###\n"))))

(ert-deftest test-markdown-insertion/atx-region-whitespace ()
  "Test ATX header insertion using a region with whitespace."
  (markdown-test-string "  line one\n\nline two\n  \n"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (goto-char (point-max))
                        (markdown-insert-header-atx-2)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "## line one line two ##"))
                        (should (= (point) 21))
                        (should (looking-at " ##"))))

(ert-deftest test-markdown-insertion/atx-line-whitespace ()
  "Test ATX header insertion using current line with whitespace."
  (markdown-test-string "  line one  \n\nline two\n"
                        (goto-char (line-end-position))
                        (markdown-insert-header-atx-3)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "### line one ###\n\nline two\n"))
                        (should (= (point) 13))
                        (should (looking-at " ###\n"))))

(ert-deftest test-markdown-insertion/atx-replace-atx ()
  "Test ATX header insertion when replacing an existing ATX header."
  (markdown-test-string "## replace ##\n"
                        (markdown-insert-header-atx-4)
                        (should (string-equal (buffer-string) "#### replace ####\n\n"))
                        (should (looking-at " ####\n"))))

(ert-deftest test-markdown-insertion/atx-replace-setext-1 ()
  "Test ATX header insertion when replacing an existing setext header."
  (markdown-test-string "replace\n=======\n"
                        (markdown-insert-header-atx-2)
                        (should (string-equal (buffer-string) "## replace ##\n\n"))
                        (should (looking-at " ##\n"))))

(ert-deftest test-markdown-insertion/atx-replace-setext-2 ()
  "Test ATX header insertion when replacing an existing setext header."
  (markdown-test-string "replace\n-------\n"
                        (markdown-insert-header-atx-5)
                        (should (string-equal (buffer-string) "##### replace #####\n\n"))
                        (should (looking-at " #####\n"))))

(ert-deftest test-markdown-insertion/atx-asymmetric-point ()
  "Test point after ATX header insertion with `markdown-asymmetric-header'."
  (markdown-test-string
   "Test"
   (let ((markdown-asymmetric-header t))
     (markdown-insert-header-atx-5)
     (should (= (point) 11))
     (should (string-equal (buffer-string) "##### Test")))))

(ert-deftest test-markdown-insertion/setext-line ()
  "Test setext header insertion without region."
  (markdown-test-string "line one\nline two\n"
                        (forward-word)
                        (markdown-insert-header-setext-1)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line one\n========\n\nline two\n"))
                        (forward-line 3)
                        (markdown-insert-header-setext-2)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line one\n========\n\nline two\n--------\n\n"))))

(ert-deftest test-markdown-insertion/setext-region ()
  "Test setext header insertion with region."
  (markdown-test-string "line one\nline two\n"
                        (transient-mark-mode)
                        (forward-char 5)
                        (push-mark (point) t t)
                        (forward-word)
                        (should (string-equal (buffer-substring (region-beginning) (region-end))
                                              "one"))
                        (markdown-insert-header-setext-1)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line \n\none\n===\n\nline two\n"))))

(ert-deftest test-markdown-insertion/setext-blank ()
  "Test setext header insertion on blank line."
  (markdown-test-string "line one\n\nline two\n"
                        (forward-line)
                        (markdown-insert-header 2 "foo" t)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line one\n\nfoo\n---\n\nline two\n"))
                        (should (= (point) 14))
                        (should (looking-at "\n---"))))

(ert-deftest test-markdown-insertion/setext-region-whitespace ()
  "Test setext header insertion using a region with whitespace."
  (markdown-test-string "  line one\n\nline two\n  \n"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (goto-char (point-max))
                        (markdown-insert-header-setext-1)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line one line two\n================="))
                        (should (= (point) 18))
                        (should (looking-at "\n===="))))

(ert-deftest test-markdown-insertion/setext-line-whitespace ()
  "Test setext header insertion using current line with whitespace."
  (markdown-test-string "  line one  \n\nline two\n"
                        (goto-char (line-end-position))
                        (markdown-insert-header-setext-2)
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "line one\n--------\n\nline two\n"))
                        (should (= (point) 9))
                        (should (looking-at "\n---"))))

(ert-deftest test-markdown-insertion/setext-replace-atx ()
  "Test setext header insertion when replacing an existing ATX header."
  (markdown-test-string "## replace ##\n"
                        (markdown-insert-header-setext-1)
                        (should (string-equal (buffer-string) "replace\n=======\n\n"))
                        (should (looking-at "\n==="))))

(ert-deftest test-markdown-insertion/setext-replace-setext-1 ()
  "Test setext header insertion when replacing an existing setext title."
  (markdown-test-string "replace\n=======\n"
                        (markdown-insert-header-setext-2)
                        (should (string-equal (buffer-string) "replace\n-------\n\n"))
                        (should (looking-at "\n---"))))

(ert-deftest test-markdown-insertion/setext-replace-setext-2 ()
  "Test setext header insertion when replacing an existing setext section."
  (markdown-test-string "replace\n-------\n"
                        (markdown-insert-header-setext-1)
                        (should (string-equal (buffer-string) "replace\n=======\n\n"))
                        (should (looking-at "\n==="))))

(ert-deftest test-markdown-insertion/header-dwim ()
  "Test 'do what I mean' header insertion."
  (markdown-test-file "outline.text"
                      (call-interactively 'markdown-insert-header-dwim)
                      (should (looking-at " #$"))
                      (end-of-defun 2)
                      (call-interactively 'markdown-insert-header-dwim)
                      (beginning-of-line)
                      (should (looking-at "^#  #$"))
                      (end-of-defun 3)
                      (call-interactively 'markdown-insert-header-dwim)
                      (beginning-of-line)
                      (should (looking-at "^###  ###$"))))

(ert-deftest test-markdown-insertion/header-dwim-prefix ()
  "Test 'do what I mean' header insertion with prefix arguments."
  (let ((tests (list '(nil . "## abc ##")
                     '(1 . "# abc #")
                     '(2 . "## abc ##")
                     '(3 . "### abc ###")
                     '(4 . "#### abc ####")
                     '(5 . "##### abc #####")
                     '(6 . "###### abc ######")
                     '((4) . "# abc #")
                     '((16) . "### abc ###"))))
    (dolist (test tests)
      (markdown-test-string "## atx\n\nabc"
                            (goto-char (point-max))
                            (let ((current-prefix-arg (car test)))
                              (call-interactively 'markdown-insert-header-dwim)
                              (should (string-equal
                                       (buffer-substring (line-beginning-position) (line-end-position))
                                       (cdr test))))))))

(ert-deftest test-markdown-insertion/header-setext-dwim-prefix ()
  "Test 'do what I mean' header insertion with prefix arguments."
  (let ((tests (list '(nil . "abc\n---")
                     '(1 . "abc\n===")
                     '(2 . "abc\n---")
                     '(3 . "### abc ###")
                     '(4 . "#### abc ####")
                     '(5 . "##### abc #####")
                     '(6 . "###### abc ######")
                     '((4) . "abc\n===")
                     '((16) . "### abc ###"))))
    (dolist (test tests)
      (markdown-test-string "atx\n---\n\nabc"
                            (goto-char (point-max))
                            (let ((current-prefix-arg (car test)))
                              (call-interactively 'markdown-insert-header-setext-dwim)
                              (should (string-equal
                                       (buffer-substring (line-beginning-position) (line-end-position 2))
                                       (cdr test))))))))

(ert-deftest test-markdown-insertion/header-setext-dwim ()
  "Test 'do what I mean' header insertion with setext headers."
  (markdown-test-string
   "asdfasfasfdsadfasdfasdf\n======="
   (goto-char 12)
   (call-interactively 'markdown-insert-header-dwim)
   (should (string-equal
            (buffer-string)
            "asdfasfasfdsadfasdfasdf\n======================="))))

(ert-deftest test-markdown-insertion/remove-header ()
  "Test ATX and setext header."
  (markdown-test-string
   "# atx1\n\n## atx2 ##\n\nsetext1\n=======\n\nsetext2\n-------\n"
   (should (equal (markdown-remove-header) (cons 1 5)))
   (forward-line)
   (should (not (markdown-remove-header)))
   (forward-line)
   (should (equal (markdown-remove-header) (cons 7 11)))
   (forward-line)
   (should (not (markdown-remove-header)))
   (forward-line)
   (should (equal (markdown-remove-header) (cons 13 20)))
   (forward-line)
   (should (not (markdown-remove-header)))
   (forward-line)
   (should (equal (markdown-remove-header) (cons 22 29)))
   (should (string-equal (buffer-string)
                         "atx1\n\natx2\n\nsetext1\n\nsetext2\n"))))

(ert-deftest test-markdown-insertion/italic-unwrap-region ()
  "A test of inserting italics with italic text in the region."
  (markdown-test-string "*foo* bar *baz*"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (end-of-line)
                        (markdown-insert-italic)
                        (should (string-equal (buffer-string) "*foo bar baz*"))))

(ert-deftest test-markdown-insertion/bold-unwrap-region ()
  "A test of inserting bold with italic text in the region."
  (markdown-test-string "*foo* **bar** *baz*"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (end-of-line)
                        (markdown-insert-bold)
                        (should (string-equal (buffer-string) "***foo* bar *baz***"))))

(ert-deftest test-markdown-insertion/code-unwrap-region ()
  "A test of inserting code with code already in the region."
  (markdown-test-string "`foo` *bar* `baz`"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (end-of-line)
                        (markdown-insert-code)
                        (should (string-equal (buffer-string) "`foo *bar* baz`"))))

(ert-deftest test-markdown-insertion/hr-order ()
  "Test inserting horizontal rules."
  (dotimes (n (length markdown-hr-strings))
    (markdown-test-string ""
                          (let ((current-prefix-arg n))
                            (call-interactively 'markdown-insert-hr))
                          (should (string-equal (buffer-string) (nth (1- n) markdown-hr-strings))))))

(ert-deftest test-markdown-insertion/hr-prefix ()
  "Test inserting horizontal rule with C-u prefix."
  (markdown-test-string ""
                        (let ((current-prefix-arg '(4)))
                          (call-interactively 'markdown-insert-hr))
                        (should (string-equal (buffer-string) (car (last markdown-hr-strings))))))

(ert-deftest test-markdown-insertion/hr-bob ()
  "Test inserting horizontal rule at beginning of buffer."
  (markdown-test-string "one line\n"
                        (call-interactively 'markdown-insert-hr)
                        (should (string-equal (buffer-string)
                                              (concat (car markdown-hr-strings)
                                                      "\n\none line\n")))))

(ert-deftest test-markdown-insertion/hr-eob ()
  "Test inserting horizontal rule at end of buffer."
  (markdown-test-string "one line\n"
                        (forward-line)
                        (call-interactively 'markdown-insert-hr)
                        (should (string-equal (buffer-string)
                                              (concat "one line\n\n" (car markdown-hr-strings))))))

(ert-deftest test-markdown-insertion/hr-mob ()
  "Test inserting horizontal rule in middle of buffer."
  (markdown-test-string "one line\n"
                        (forward-word)
                        (let ((markdown-hr-strings '("----------")))
                          (call-interactively 'markdown-insert-hr)
                          (should (string-equal (buffer-string)
                                                (concat "one\n\n" (car markdown-hr-strings)
                                                        "\n\n line\n"))))))

(ert-deftest test-markdown-insertion/pre-region-1 ()
  "Test `markdown-pre-region'."
  ;; Simple test as non-interactive command
  (markdown-test-string "line one\nline two\n"
                        (markdown-pre-region (line-beginning-position) (line-end-position))
                        (should (string-equal (buffer-string) "    line one\n\nline two\n")))
  ;; Test removal of whitespace before and after region
  (markdown-test-string "line one abc\nline two\n"
                        (markdown-pre-region 6 9)
                        (should (string-equal (buffer-string) "line\n\n    one\n\nabc\nline two\n")))
  ;; Simple test as interactive command
  (markdown-test-string "line one\nline two\n"
                        (push-mark (point) t t)
                        (forward-line 2)
                        (call-interactively 'markdown-pre-region)
                        (should (string-equal (buffer-string) "    line one\n    line two\n\n"))))

(ert-deftest test-markdown-insertion/blockquote-region-1 ()
  "Test `markdown-blockquote-region'."
  ;; Simple test as non-interactive command
  (markdown-test-string "line one\nline two\n"
                        (markdown-blockquote-region (line-beginning-position) (line-end-position))
                        (should (string-equal (buffer-string) "> line one\n\nline two\n")))
  ;; Test removal of whitespace before and after region
  (markdown-test-string "line one abc\nline two\n"
                        (markdown-blockquote-region 6 9)
                        (should (string-equal (buffer-string) "line\n\n> one\n\nabc\nline two\n")))
  ;; Simple test as interactive command
  (markdown-test-string "line one\nline two\n"
                        (push-mark (point) t t)
                        (forward-line 2)
                        (call-interactively 'markdown-blockquote-region)
                        (should (string-equal (buffer-string) "> line one\n> line two\n\n"))))

(ert-deftest test-markdown-insertion/pre-nested-lists ()
  "Test `markdown-pre-indentation' and `markdown-insert-pre' with nested list."
  (markdown-test-string "* item\n    * item\n"
                        ;; before the first item
                        (should (string-equal (markdown-pre-indentation (point)) "    "))
                        (markdown-insert-pre)
                        (beginning-of-line)
                        (should (markdown-prev-line-blank-p))
                        (should (looking-at "^    $"))
                        (should (markdown-next-line-blank-p))
                        ;; before the second item
                        (forward-line 3)
                        (should (string-equal (markdown-pre-indentation (point)) "        "))
                        (markdown-insert-pre)
                        (beginning-of-line)
                        (should (markdown-prev-line-blank-p))
                        (should (looking-at "^        $"))
                        (should (markdown-next-line-blank-p))
                        ;; after the second item
                        (forward-line 3)
                        (should (string-equal (markdown-pre-indentation (point)) "            "))
                        (markdown-insert-pre)
                        (beginning-of-line)
                        (should (markdown-prev-line-blank-p))
                        (should (looking-at "^            $"))
                        (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/pre-faux-list ()
  "Test `markdown-pre-indentation' following a list-marker in a pre block."
  (markdown-test-string "    * pre block, not a list item\n"
                        (should (string-equal (markdown-pre-indentation (point-max)) "    "))))

(ert-deftest test-markdown-insertion/blockquote-nested-lists ()
  "Test blockquote insertion in a nested list context."
  (markdown-test-string "* item\n    * item\n"
                        ;; before the first item
                        (should (string-equal (markdown-blockquote-indentation (point)) ""))
                        (markdown-insert-blockquote)
                        (beginning-of-line)
                        (should (markdown-prev-line-blank-p))
                        (should (looking-at "^> $"))
                        (should (markdown-next-line-blank-p))
                        ;; before the second item
                        (forward-line 3)
                        (should (string-equal (markdown-blockquote-indentation (point)) "    "))
                        (markdown-insert-blockquote)
                        (beginning-of-line)
                        (should (markdown-prev-line-blank-p))
                        (should (looking-at "^    > $"))
                        (should (markdown-next-line-blank-p))
                        ;; after the second item
                        (forward-line 3)
                        (should (string-equal (markdown-blockquote-indentation (point)) "        "))
                        (markdown-insert-blockquote)
                        (beginning-of-line)
                        (should (markdown-prev-line-blank-p))
                        (should (looking-at "^        > $"))
                        (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/empty-italic ()
  "Test `markdown-insert-italic' with no word at point and no region."
  (markdown-test-string ""
                        (call-interactively 'markdown-insert-italic)
                        (should (string-equal (buffer-string) "**"))
                        (should (= (point) 2))))

(ert-deftest test-markdown-insertion/empty-bold ()
  "Test `markdown-insert-bold' with no word at point and no region."
  (markdown-test-string ""
                        (call-interactively 'markdown-insert-bold)
                        (should (string-equal (buffer-string) "****"))
                        (should (= (point) 3))))

(ert-deftest test-markdown-insertion/uri ()
  "Test `markdown-insert-uri'."
  (markdown-test-string "http://jblevins.org/projects/markdown-mode/"
                        (call-interactively 'markdown-insert-uri)
                        (should (string-equal (buffer-string) "<http://jblevins.org/projects/markdown-mode/>"))
                        (should (= (point) 2))
                        (call-interactively 'markdown-insert-uri)
                        (should (string-equal (buffer-string) "http://jblevins.org/projects/markdown-mode/"))
                        (should (= (point) 1))
                        (erase-buffer)
                        (call-interactively 'markdown-insert-uri)
                        (should (string-equal (buffer-string) "<>"))
                        (should (= (point) 2))))

(ert-deftest test-markdown-insertion/list-item ()
  "Test `markdown-insert-list-item' on several lists."
  ;; No existing list
  (markdown-test-string "abc"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "abc\n  * "))
                        (should (= (point) 9)))
  ;; Following a list item, on the same line
  (markdown-test-string "  * foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "  * foo\n  * ")))
  ;; Following a list item, on the next line
  (markdown-test-string "- foo\n"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "- foo\n- ")))
  ;; Following a list item, after a blank line
  (markdown-test-string "- foo\n\n"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "- foo\n\n- ")))
  ;; Preceding a list item
  (markdown-test-string "- foo\n"
                        (goto-char (point-min))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "- \n- foo\n")))
  ;; Preceding a list item and a blank line
  (markdown-test-string "\n\n- foo\n"
                        (goto-char (point-min))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "- \n\n- foo\n")))
  ;; In the middle of a list item
  (markdown-test-string "- foo bar\n"
                        (forward-word)
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "- foo\n-  bar\n")))
  ;; Before a list marker, but not at beginning of line
  (markdown-test-string "   - foo\n"
                        (forward-char 2)
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "   - \n   - foo\n")))
  ;; Following an ordered list item
  (markdown-test-string "6. foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "6. foo\n7. ")))
  ;; Following a fancy list item, on the next line
  (markdown-test-string "#. foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "#. foo\n#. ")))
  ;; Following a nested ordered list item
  (markdown-test-string "6. foo\n    1. bar"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "6. foo\n    1. bar\n    2. ")))
  ;; Preceding an ordered list item
  (markdown-test-string "\n1. foo\n2. bar"
                        (goto-char (point-min))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "1. \n1. foo\n2. bar")))
  ;; Preserve previous spacing in ordered list
  (markdown-test-string "1.        foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "1.        foo\n2.        ")))
  ;; Adjust spacing for number width changes (e.g., 9 -> 10)
  (markdown-test-string "9.  foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "9.  foo\n10. ")))
  ;; Don't adjust spacing for number width changes if no extra whitespace
  (markdown-test-string "99. foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "99. foo\n100. ")))
  ;; Don't adjust spacing if tabs are used as whitespace
  (markdown-test-string "9.\tfoo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "9.\tfoo\n10.\t"))))

(ert-deftest test-markdown-insertion/nested-list-marker ()
  "Test marker detection for `markdown-insert-list-item'."
  (markdown-test-string
   "1. A\n    * AA\n        1. AAA"
   (goto-char (point-max))
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-list-item))
   (should (eq (point) 36))
   (should (looking-back "\* "))
   (should (string-equal
            (buffer-string)
            "1. A\n    * AA\n        1. AAA\n    * "))
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-list-item))
   (should (eq (point) 40))
   (should (looking-back "2\. "))
   (should (string-equal
            (buffer-string)
            "1. A\n    * AA\n        1. AAA\n    * \n2. "))
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-list-item))
   (should (eq (point) 44))
   (should (looking-back "3\. "))
   (should (string-equal
            (buffer-string)
            "1. A\n    * AA\n        1. AAA\n    * \n2. \n3. "))))

(ert-deftest test-markdown-insertion/reference-link ()
  "Basic tests for `markdown-insert-reference-link'."
  ;; Test optional parameters (leave point after link)
  (markdown-test-string ""
                        (markdown-insert-reference-link "abc" "1")
                        (should (string-equal (buffer-string) "[abc][1]"))
                        (should (= (point) 9)))
  ;; Full link without title (leave point after link)
  (markdown-test-string ""
                        (markdown-insert-reference-link "link" "label" "http://jblevins.org/")
                        (should (string-equal (buffer-string) "[link][label]\n\n[label]: http://jblevins.org/\n"))
                        (should (= (point) 14)))
  ;; Full link without label or title (leave point after link)
  (markdown-test-string ""
                        (markdown-insert-reference-link "link" "" "http://jblevins.org/")
                        (should (string-equal (buffer-string) "[link][]\n\n[link]: http://jblevins.org/\n"))
                        (should (= (point) 9)))
  ;; Link only with no label, URL, or title (leave point after link)
  (markdown-test-string ""
                        (markdown-insert-reference-link "link" "")
                        (should (string-equal (buffer-string) "[link][]"))
                        (should (= (point) 9))))

(ert-deftest test-markdown-insertion/reference-link-end ()
  "Basic reference link insertion test for 'end location."
  (let ((markdown-reference-location 'end))
    (markdown-test-string "first para\n\nsecond para\n"
                          (end-of-line)
                          (markdown-insert-reference-link "link" "" "http://jblevins.org/")
                          (should (= (point) 19))
                          (goto-char (point-min))
                          (forward-line 4)
                          (should (looking-at "\\[link\\]: http://jblevins.org/")))))

(ert-deftest test-markdown-insertion/reference-link-immediately ()
  "Basic reference link insertion test for 'immediately location."
  (let ((markdown-reference-location 'immediately))
    (markdown-test-string "first para\n\nsecond para\n"
                          (end-of-line)
                          (markdown-insert-reference-link "link" "" "http://jblevins.org/")
                          (should (= (point) 19))
                          (goto-char (point-min))
                          (forward-line 2)
                          (should (looking-at "\\[link\\]: http://jblevins.org/")))))

(ert-deftest test-markdown-insertion/reference-link-header ()
  "Basic reference link and definition insertion test for 'header location."
  (let ((markdown-reference-location 'header))
    (markdown-test-string "par one\n\npar two\n\n### header\n"
                          (end-of-line)
                          (markdown-insert-reference-link "link" "")
                          (markdown-insert-reference-definition "link")
                          (should (= (point) 35))
                          (should (looking-back "\\[link\\]: " nil)))))

(ert-deftest test-markdown-insertion/inline-to-reference-link ()
  "Inline link to reference link conversion."
  (markdown-test-string "[text](http://jblevins.org/ \"title\")"
                        (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-reference-link-dwim RET 1 RET"))
                        (should (string-equal (buffer-string) "[text][1]\n\n[1]: http://jblevins.org/ \"title\"\n")))
  (markdown-test-string "[text](http://jblevins.org/)"
                        (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-reference-link-dwim RET 1 RET"))
                        (should (string-equal (buffer-string) "[text][1]\n\n[1]: http://jblevins.org/\n"))))

(ert-deftest test-markdown-insertion/inline-to-reference-link-2 ()
  "Inline link to reference link conversion with existing reference links.
Regression test: adding a new reference link with
`markdown-insert-reference-link-dwim' should not throw an 'args
out of range' error when the existing reference label is a single
character."
  (markdown-test-string "[text](http://jblevins.org/ \"title\")\n\n[1]: https://www.gnu.org"
                        (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-reference-link-dwim RET 2 RET"))))

(ert-deftest test-markdown-insertion/inline-link ()
  "Basic tests for `markdown-insert-link'."
  ;; Test empty markup insertion (leave point in square brackets)
  (markdown-test-string "abc "
                        (end-of-line)
                        (call-interactively 'markdown-insert-link)
                        (should (string-equal (buffer-string) "abc []()"))
                        (should (= (point) 6)))
  ;; Test with word at point (leave point in parentheses)
  (markdown-test-string "abc def ghi"
                        (forward-word 2)
                        (call-interactively 'markdown-insert-link)
                        (should (string-equal (buffer-string) "abc [def]() ghi"))
                        (should (= (point) 11)))
  ;; Test with region (leave point in parentheses)
  (markdown-test-string "abc def ghi"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (forward-word 2)
                        (call-interactively 'markdown-insert-link)
                        (should (string-equal (buffer-string) "[abc def]() ghi"))
                        (should (= (point) 11))))

;;; Footnote tests:

(ert-deftest test-markdown-footnote/basic-end ()
  "Basic footnote insertion and deletion tests for 'end location."
  (let ((markdown-footnote-location 'end))
    (markdown-test-string "first line\nsecond line\n"
                          ;; new buffer with no footnotes
                          (should (= markdown-footnote-counter 0))
                          ;; footnote insertion
                          (end-of-line)
                          (markdown-insert-footnote)
                          (should (= (point) 35))
                          (should (= markdown-footnote-counter 1))
                          (should (looking-back "\\[^1\\]: " nil))
                          ;; kill with point in footnote definition
                          (insert "footnote text")
                          (let (kill-ring)
                            (markdown-footnote-kill))
                          (should (= (point) 24))
                          (should (bolp))
                          (should (string-equal (buffer-string) "first line\nsecond line\n"))
                          ;; insertion, counter should increment
                          (goto-char (point-min))
                          (end-of-line)
                          (markdown-insert-footnote)
                          (should (= (point) 35))
                          (should (= markdown-footnote-counter 2))
                          (should (looking-back "\\[^2\\]: " nil))
                          (insert "footnote text")
                          ;; return to marker
                          (markdown-footnote-return)
                          (should (= (point) 15))
                          (should (looking-back "\\[^2\\]" nil))
                          ;; kill with point at marker
                          (let (kill-ring)
                            (markdown-footnote-kill))
                          (should (= (point) 11))
                          (should (eolp))
                          (should (string-equal (buffer-string) "first line\nsecond line\n")))))

(ert-deftest test-markdown-footnote/basic-immediately ()
  "Basic footnote insertion and deletion tests for 'immediately location."
  (let ((markdown-footnote-location 'immediately))
    (markdown-test-string "first paragraph\n\nsecond paragraph\n"
                          ;; new buffer with no footnotes
                          (should (= markdown-footnote-counter 0))
                          ;; footnote insertion
                          (end-of-line)
                          (markdown-insert-footnote)
                          (should (= (point) 28))
                          (should (= markdown-footnote-counter 1))
                          (should (looking-back "\\[^1\\]: " nil))
                          ;; kill with point in footnote definition
                          (insert "footnote text")
                          (let (kill-ring)
                            (markdown-footnote-kill))
                          (should (= (point) 18))
                          (should (bolp))
                          (should (string-equal (buffer-string)
                                                "first paragraph\n\nsecond paragraph\n")))))

(ert-deftest test-markdown-footnote/basic-header ()
  "Basic footnote insertion and deletion tests for 'header location."
  (let ((markdown-footnote-location 'header))
    (markdown-test-string "par one\n\npar two\n\n### header\n"
                          ;; new buffer with no footnotes
                          (should (= markdown-footnote-counter 0))
                          ;; footnote insertion
                          (end-of-line)
                          (markdown-insert-footnote)
                          (should (= (point) 29))
                          (should (= markdown-footnote-counter 1))
                          (should (looking-back "\\[^1\\]: " nil))
                          ;; kill with point in footnote definition
                          (insert "footnote text")
                          (let (kill-ring)
                            (markdown-footnote-kill))
                          (should (= (point) 19))
                          (should (bolp))
                          (should (string-equal (buffer-string)
                                                "par one\n\npar two\n\n### header\n"))
                          ;; insertion, counter should increment
                          (goto-char (point-min))
                          (end-of-line)
                          (markdown-insert-footnote)
                          (should (= (point) 29))
                          (should (= markdown-footnote-counter 2))
                          (should (looking-back "\\[^2\\]: " nil))
                          (insert "footnote text")
                          ;; return to marker
                          (markdown-footnote-return)
                          (should (= (point) 12))
                          (should (looking-back "\\[^2\\]" nil))
                          ;; kill with point at marker
                          (let (kill-ring)
                            (markdown-footnote-kill))
                          (should (= (point) 8))
                          (should (eolp))
                          (should (string-equal (buffer-string)
                                                "par one\n\npar two\n\n### header\n")))))

(ert-deftest test-markdown-footnote/kill-empty-text ()
  "Test killing a footnote with marker but no text."
  (markdown-test-string "no text[^1]\n\n[^1]: \n"
                        (end-of-line)
                        (markdown-footnote-goto-text)
                        (should (looking-back "\\[^1\\]: " nil))
                        (let (kill-ring)
                          (markdown-footnote-kill))
                        (should (string-equal (buffer-string) "no text\n"))))

(ert-deftest test-markdown-footnote/kill-empty-after ()
  "Test killing an empty footnote after one with text (previously killed the
footnote with text above)."
  (markdown-test-string "[^with-text][^no-text]\n\n[^with-text]: Text\n[^no-text]:"
                        (let (kill-ring)
                          (forward-line 3)
                          (should (looking-at "\\[\\^no-text\\]:$"))
                          (markdown-footnote-kill)
                          (should (string-equal (current-kill 0) "")))))

(ert-deftest test-markdown-footnote/kill-hanging-paras ()
  "Test killing a footnote where block text starts after the label (previously
killed the footnote above)."
  (markdown-test-string "[^1][^2]\n\n[^1]: Foo\n\n[^2]:\n    Text\n\n    More text\n\n\nNot indented"
                        (let (kill-ring)
                          (forward-line 4)
                          (should (looking-at "\\[\\^2\\]:$"))
                          (markdown-footnote-kill)
                          ;; We want to include the leading space on hanging footnote paragraphs,
                          ;; even if a hanging paragraph is the first item in the footnote.
                          (should (string-equal (current-kill 0) "Text\n\n    More text\n")))))

(ert-deftest test-markdown-footnote/text-positions-buffer-top ()
  "Test markdown-footnote-text-positions on footnote adjacent to buffer top
(was infinite loop)."
  (markdown-test-string "[^label]: text\n    more text"
   (should (equal (markdown-footnote-text-positions) (list "^label" 1 29)))))

(ert-deftest test-markdown-footnote/text-positions-buffer-top-one-line ()
  "Test markdown-footnote-text-positions on one-line footnote adjacent to
buffer top (failed to find positions)."
  (markdown-test-string "[^label]: text\n"
                        (should (equal (markdown-footnote-text-positions) (list "^label" 1 16)))))

(ert-deftest test-markdown-footnote/text-positions-buffer-top-not-footnote ()
  "Test markdown-footnote-text-positions on plain paragraph adjacent to buffer
top (was infinite loop)."
  (markdown-test-string "text\n    more text\n"
                        (should (eq (markdown-footnote-text-positions) nil))))

(ert-deftest test-markdown-footnote/text-positions-buffer-bottom ()
  "Test markdown-footnote-text-positions on footnote adjacent to buffer bottom
(was infinite loop)."
  (markdown-test-string "\n[^label]: text\n    more text"
   (forward-line 1)
   (should (equal (markdown-footnote-text-positions) (list "^label" 2 30)))))

(ert-deftest test-markdown-footnote/kill-adjacent-footnote ()
  "Test killing a footnote adjacent to other one-line footnotes (previously
killed the wrong one)."
  (markdown-test-string "Text[^1] with[^2] footnotes[^3]\n\n[^1]: foo\n[^2]: bar\n[^3]: baz"
                        (let (kill-ring)
                          (forward-line 3)
                          (should (looking-at "\\[\\^2\\]: bar"))
                          (markdown-footnote-kill)
                          (should (string-equal (current-kill 0) "bar\n")))))

(ert-deftest test-markdown-footnote/kill-adjacent-markers ()
  "Test killing a footnote where the labels are adjacent (previously, the wrong
footnote would be killed because the attempt to jump to the marker would jump to
the opening bracket of [^2], and then subsequent functions would kill [^2])."
  (markdown-test-string "Text with footnotes[^1][^2]\n\n[^1]: foo\n\n[^2]: bar\n"
                        (let (kill-ring)
                          (forward-line 2)
                          (should (looking-at "\\[\\^1\\]: foo"))
                          (markdown-footnote-kill)
                          (should (string-equal (current-kill 0) "foo\n")))))

(when (version< emacs-version "24.2")
  ;; fix segfault on 24.1 with the normal implementation of this function. isn't
  ;; exactly correct, but should make tests work the same
  (defadvice kill-buffer-and-window (around markdown-test-fix-segfault activate)
    (kill-buffer)
    (select-window (previous-window))))

(ert-deftest test-markdown-footnote-reference/jump ()
  "Test `markdown-jump' for footnotes and reference links."
  (markdown-test-string
      "body[^1], [link 1][ref],
[link 2][ref]

[^1]: footnote

[ref]: https://duckduckgo.com/"
   (goto-char 5) ; start of [^1]
   (markdown-jump) ; markdown-footnote-goto-text
   (should (looking-at "footnote"))
   (markdown-jump) ; markdown-footnote-return
   (should (= (point) 9)) ; just after [^1]
   (markdown-next-link) ; beginning of [link 1][]
   (markdown-jump)
   (should (looking-at "https://duckduckgo.com/"))
   (should (equal (markdown-reference-find-links "ref")
                  (list (list "link 2" 26 2) (list "link 1" 11 1))))
   (markdown-jump) ; opens a reference link buffer
   (should (string= (buffer-string) "Links using reference ref:\n\nlink 1 (line 1)\nlink 2 (line 2)\n"))
   (should (looking-at "link 1")) ; in reference link popop buffer
   (execute-kbd-macro (read-kbd-macro "RET")) ; jump to "link 1"
   (should (looking-at "\\[link 1\\]")) ; back in main buffer
   (should (= (point) 11))))

;;; Element removal tests:

(ert-deftest test-markdown-kill/simple ()
  "Simple tests for `markdown-kill-thing-at-point'."
  (let ((kill-ring nil)
        (tests (list '("`foo`" . "foo")
                     '("## foo ##" . "foo")
                     '("## foo" . "foo")
                     '("foo\n---" . "foo")
                     '("foo\n===" . "foo")
                     '("* * * * *" . "* * * * *")
                     '("[foo](http://bar.com/)" . "foo")
                     '("![foo](http://bar.com/)" . "foo")
                     '("[foo][bar]" . "foo")
                     '("![foo][bar]" . "foo")
                     '("<http://foo.com/>" . "http://foo.com/")
                     '("<foo@bar.com>" . "foo@bar.com")
                     '("**foo**" . "foo")
                     '("__foo__" . "foo")
                     '("*foo*" . "foo")
                     '("_foo_" . "foo")
                     '("  [foo]: http://bar.com/" . "http://bar.com/")
                     '("  [foo]: http://bar.com/ \"title\"" . "http://bar.com/")
                     '("foo[^bar]\n\n[^bar]: baz" . "baz")
                     '("[^bar]: baz" . "baz")
                     '("  * foo\n  bar" . "  * foo\n  bar"))))
    (dolist (test tests)
      ;; Load test string (the car), move to end of first line, kill
      ;; thing at point, and then verify that the kill ring contains cdr.
      (markdown-test-string (car test)
                            (end-of-line)
                            (call-interactively 'markdown-kill-thing-at-point)
                            (should (string-equal (current-kill 0) (cdr test)))))))

(ert-deftest test-markdown-kill/footnote-text ()
  "Test killing a footnote with point at footnote text."
  (markdown-test-string "some text[^1]\n\n[^1]: footnote\n"
                        (end-of-line)
                        (markdown-footnote-goto-text)
                        (let (kill-ring)
                          (markdown-footnote-kill))
                        (should (string-equal (buffer-string) "some text\n"))))

(ert-deftest test-markdown-kill/code ()
  "Test killing with code regex.."
  (let ((kill-ring nil))
    (markdown-test-string "Lorem `ipsum` dolor `sit` `amet`."
                          (goto-char 22) ; position point at s in `sit`
                          (call-interactively 'markdown-kill-thing-at-point)
                          (should (string-equal (current-kill 0) "sit")))))

;;; Completion:

(ert-deftest test-markdown-complete/atx-header-incomplete ()
  "Test `markdown-incomplete-atx-p'."
  (markdown-test-string "###  ###"
                        (should (looking-at markdown-regex-header-atx))
                        (should-not (markdown-incomplete-atx-p)))
  (markdown-test-string "###abc###"
                        (should-not (looking-at markdown-regex-header-atx)))
  (markdown-test-string "###   ###"
                        (should (looking-at markdown-regex-header-atx))
                        (should (markdown-incomplete-atx-p))))

(ert-deftest test-markdown-complete/atx-header ()
  "Test `markdown-complete' for atx headers."
  (markdown-test-string "##### test"
                        (call-interactively 'markdown-complete)
                        (should (string-equal (buffer-string) "##### test #####"))))

(ert-deftest test-markdown-complete/setext-header-incomplete ()
  "Test `markdown-incomplete-setext-p'."
  (markdown-test-string "abc\n===\n"
                        (should (looking-at markdown-regex-header-setext))
                        (should-not (markdown-incomplete-setext-p)))
  (markdown-test-string "abc\n==\n"
                        (should (looking-at markdown-regex-header-setext))
                        (should (markdown-incomplete-setext-p)))
  (markdown-test-string "abc\n====\n"
                        (should (looking-at markdown-regex-header-setext))
                        (should (markdown-incomplete-setext-p))))

(ert-deftest test-markdown-complete/setext-header ()
  "Test `markdown-complete' for setext headers."
  (markdown-test-string "test  \n=="
                        (call-interactively 'markdown-complete)
                        (should (string-equal (buffer-string) "test\n===="))))

(ert-deftest test-markdown-complete/hr-incomplete ()
  "Test `markdown-incomplete-hr-p'."
  (dolist (i (number-sequence 0 (1- (length markdown-hr-strings))))
    (markdown-test-string (nth i markdown-hr-strings)
                          (should (looking-at markdown-regex-hr))
                          (should-not (markdown-incomplete-hr-p))
                          (should-error (call-interactively 'markdown-complete)))))

(ert-deftest test-markdown-complete/hr ()
  "Test completion via `markdown-complete' for horizontal rules."
  (markdown-test-string "- - - - -"
                        (call-interactively 'markdown-complete)
                        (should (string-equal (buffer-string) (car markdown-hr-strings)))))

(ert-deftest test-markdown-complete/buffer-setext-2 ()
  "Test `markdown-complete-buffer' for level two setext heading."
  ;; Ensure markdown-complete-buffer doesn't mistake this for a horizontal rule
  (markdown-test-string "Subheading\n--\n"
                        (call-interactively 'markdown-complete-buffer)
                        (should (string-equal (buffer-string) "Subheading\n----------\n\n")))
  (markdown-test-string "Abc\n--\n\nDef\n--\n"
                        (call-interactively 'markdown-complete-buffer)
                        (should (string-equal (buffer-string) "Abc\n---\n\nDef\n---\n\n"))))

;;; Promotion and demotion tests:

(ert-deftest test-markdown-promote/atx-header ()
  "Test `markdown-promote' for atx headers."
  (markdown-test-string "###### test ######"
                        (markdown-promote)
                        (should (string-equal (buffer-string) "##### test #####"))
                        (markdown-promote)
                        (should (string-equal (buffer-string) "#### test ####"))
                        (markdown-promote)
                        (should (string-equal (buffer-string) "### test ###"))
                        (markdown-promote)
                        (should (string-equal (buffer-string) "## test ##"))
                        (markdown-promote)
                        (should (string-equal (buffer-string) "# test #"))))

(ert-deftest test-markdown-demote/atx-header ()
  "Test `markdown-demote' for atx headers."
  (markdown-test-string "# test #"
                        (markdown-demote)
                        (should (string-equal (buffer-string) "## test ##"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "### test ###"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "#### test ####"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "##### test #####"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "###### test ######"))))

(ert-deftest test-markdown-promote/setext-header ()
  "Test `markdown-promote' for setext headers."
  (markdown-test-string "test\n----"
                        (markdown-promote)
                        (should (string-equal (buffer-string) "test\n===="))))

(ert-deftest test-markdown-demote/setext-header ()
  "Test `markdown-demote' for setext headers."
  (markdown-test-string "test\n===="
                        (markdown-demote)
                        (should (string-equal (buffer-string) "test\n----"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "### test ###"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "#### test ####"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "##### test #####"))
                        (markdown-demote)
                        (should (string-equal (buffer-string) "###### test ######"))))

(ert-deftest test-markdown-promote/hr ()
  "Test `markdown-promote' for horizontal rules."
  (markdown-test-string (car (reverse markdown-hr-strings))
                        (dolist (n (number-sequence 4 0 -1))
                          (markdown-promote)
                          (should (string-equal (buffer-string) (nth n markdown-hr-strings))))))

(ert-deftest test-markdown-demote/hr ()
  "Test `markdown-demote' for horizontal rules."
  (markdown-test-string (car markdown-hr-strings)
                        (dolist (n (number-sequence 1 5))
                          (markdown-demote)
                          (should (string-equal (buffer-string) (nth n markdown-hr-strings))))))

(ert-deftest test-markdown-promote/bold ()
  "Test `markdown-promote' for bold markup."
  (markdown-test-string "__bold__"
                        (call-interactively 'markdown-promote)
                        (should (string-equal (buffer-string) "**bold**"))))

(ert-deftest test-markdown-demote/bold ()
  "Test `markdown-demote' for bold markup."
  (markdown-test-string "**bold**"
                        (call-interactively 'markdown-promote)
                        (should (string-equal (buffer-string) "__bold__"))))

(ert-deftest test-markdown-promote/italic ()
  "Test `markdown-promote' for italic markup."
  (markdown-test-string "_italic_"
                        (call-interactively 'markdown-promote)
                        (should (string-equal (buffer-string) "*italic*"))))

(ert-deftest test-markdown-demote/italic ()
  "Test `markdown-demote' for italic markup."
  (markdown-test-string "*italic*"
                        (call-interactively 'markdown-promote)
                        (should (string-equal (buffer-string) "_italic_"))))

;;; Subtree editing tests:

(ert-deftest test-markdown-subtree/promote ()
  "Test `markdown-promote-subtree'."
  (markdown-test-string "# h1 #\n\n## h2 ##\n\n### h3 ###\n\n## h2 ##\n\n# h1 #\n"
                        ;; The first h1 should get promoted away.
                        ;; The second h1 should not be promoted.
                        (markdown-promote-subtree)
                        (should (string-equal (buffer-string) "h1\n\n# h2 #\n\n## h3 ##\n\n# h2 #\n\n# h1 #\n"))
                        ;; Second call should do nothing since point is no longer at a heading.
                        (markdown-promote-subtree)
                        (should (string-equal (buffer-string) "h1\n\n# h2 #\n\n## h3 ##\n\n# h2 #\n\n# h1 #\n"))
                        ;; Move to h2 and promote again.
                        (forward-line 2)
                        (markdown-promote-subtree)
                        (should (string-equal (buffer-string) "h1\n\nh2\n\n# h3 #\n\n# h2 #\n\n# h1 #\n"))))

(ert-deftest test-markdown-subtree/demote ()
  "Test `markdown-demote-subtree'."
  (markdown-test-string "# h1 #\n\n## h2 ##\n\n### h3 ###\n\n## h2 ##\n\n# h1 #\n"
                        ;; The second h1 should not be demoted
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "## h1 ##\n\n### h2 ###\n\n#### h3 ####\n\n### h2 ###\n\n# h1 #\n"))
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "### h1 ###\n\n#### h2 ####\n\n##### h3 #####\n\n#### h2 ####\n\n# h1 #\n"))
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "#### h1 ####\n\n##### h2 #####\n\n###### h3 ######\n\n##### h2 #####\n\n# h1 #\n"))
                        ;; Stop demoting at level six
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "##### h1 #####\n\n###### h2 ######\n\n###### h3 ######\n\n###### h2 ######\n\n# h1 #\n"))
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "###### h1 ######\n\n###### h2 ######\n\n###### h3 ######\n\n###### h2 ######\n\n# h1 #\n"))))

(ert-deftest test-markdown-subtree/move-up ()
  "Test `markdown-move-subtree-up'."
  ;; Note that prior to Emacs 24.5, this does not work for the last subtree in
  ;; the buffer due to Emacs bug #19102:
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19102
  ;; https://github.com/emacs-mirror/emacs/commit/b3910f
  ;; That also corrects the type of the "Cannot move pase superior level" error
  ;; from 'error to 'user-error.
  (markdown-test-string "# 1 #\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n# 2 #\n# Extra\n"
                        (re-search-forward "^# 2")
                        (markdown-move-subtree-up)
                        (should (string-equal (buffer-string) "# 2 #\n# 1 #\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n# Extra\n"))
                        ;; Second attempt should fail, leaving buffer unchanged.
                        ;; (This way of asserting the contents of the error
                        ;; message is a bit convoluted and more fragile than
                        ;; ideal. But prior to Emacs 24.5, the type of this
                        ;; error is just 'error, and a bare "should-error" is
                        ;; really overly broad.)
                        (should (string-equal
                                 "Cannot move past superior level"
                                 (cl-second (should-error (markdown-move-subtree-up)))))))

(ert-deftest test-markdown-subtree/move-down ()
  "Test `markdown-move-subtree-down'."
  (markdown-test-string "# 1 #\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n# 2 #\n"
                        (re-search-forward "^## 1\.1")
                        (markdown-move-subtree-down)
                        (should (string-equal (buffer-string) "# 1 #\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n# 2 #\n"))))

;(ert-deftest test-markdown-subtree/move-down ()

;;; Cycling:

(ert-deftest test-markdown-cycle/atx-header ()
  "Test `markdown-demote' cycling for atx headers."
  (markdown-test-string "##### test"
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "###### test ######"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "# test #"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "## test ##"))))

(ert-deftest test-markdown-cycle/setext-header ()
  "Test `markdown-demote' cycling for setext headers."
  (markdown-test-string "test\n===="
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "test\n----"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "### test ###"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "#### test ####"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "##### test #####"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "###### test ######"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "# test #"))))

(ert-deftest test-markdown-cycle/hr ()
  "Test cycling of horizontal rules."
  ;; Cycle using markdown-demote
  (markdown-test-string (car markdown-hr-strings)
                        (dolist (n (number-sequence 1 5))
                          (call-interactively 'markdown-demote)
                          (should (string-equal (buffer-string) (nth n markdown-hr-strings))))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) (car markdown-hr-strings))))
  ;; Cycle using markdown-promote
  (markdown-test-string (car (reverse markdown-hr-strings))
                        (dolist (n (number-sequence 4 0 -1))
                          (call-interactively 'markdown-promote)
                          (should (string-equal (buffer-string) (nth n markdown-hr-strings))))
                        (call-interactively 'markdown-promote)
                        (should (string-equal (buffer-string) (car (reverse markdown-hr-strings))))))

(ert-deftest test-markdown-cycle/bold ()
  "Test cycling of bold markup."
  (markdown-test-string "**bold**"
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "__bold__"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "**bold**"))))

(ert-deftest test-markdown-cycle/italic ()
  "Test cycling of italic markup."
  (markdown-test-string "*italic*"
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "_italic_"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "*italic*"))))

;;; Indentation tests:

(ert-deftest test-markdown-indentation/calc-indents ()
  "Test `markdown-calc-indents' a nested list context."
  (markdown-test-file "nested-list.text"
                      (goto-char (point-max))
                      (let ((indents (markdown-calc-indents)))
                        (should (= (car indents) 17)) ; indentation of previous line first
                        (should (equal (sort indents '<)
                                       (list
                                        0 ; beginning of line
                                        3 ; first-level list marker
                                        7 ; second-level list marker
                                        11 ; third-level list marker
                                        13 ; previous list item text
                                        16 ; pre-block indentation
                                        17 ; indentation of previous line
                                        21 ; previous line plus tab-width
                                        ))))))

(ert-deftest test-markdown-indentation/indent-region ()
  "Test `markdown-indent-region'."
  ;; Basic test with multiple lines
  (markdown-test-string "abc\ndef\nghi\n"
                        (markdown-indent-region (point-min) (point-max) nil)
                        (should (string-equal (buffer-string) "    abc\n    def\n    ghi\n")))
  ;; Following a list item
  (markdown-test-string "  * abc\ndef\n"
                        (forward-line)
                        (markdown-indent-region (line-beginning-position) (line-end-position) nil)
                        (should (string-equal (buffer-string) "  * abc\n  def\n"))
                        (markdown-indent-region (line-beginning-position) (line-end-position) nil)
                        (should (string-equal (buffer-string) "  * abc\n    def\n"))))

(ert-deftest test-markdown-indentation/indent-list-hanging ()
  "Test `markdown-indent-line' with hanging list item."
  (markdown-test-string
   "- list
  - nested list with long lines which need to be
    hard wrapped"
   (goto-char (point-max))
   (markdown-enter-key)
   (should (eq (point) 78))))

(ert-deftest test-markdown-indentation/indent-list-single ()
  "Test `markdown-indent-line' with single list item."
  (markdown-test-string
   "  * item 1"
   (end-of-line)
   (markdown-enter-key)
   (should (string-equal (buffer-string) "  * item 1\n  "))
   (should (eq (point) 14))))

(ert-deftest test-markdown-indentation/indent-pre ()
  "Test `markdown-indent-line' with a pre block."
  (markdown-test-string
   "I'm gonna write a code block:

    my first line of code"
   (goto-char (point-max))
   (markdown-enter-key)
   (should (eq (point) 62))
   (should (looking-back "^    "))))

;;; Font lock tests:

(ert-deftest test-markdown-font-lock/italics-1 ()
  "A simple italics test."
  (markdown-test-file "inline.text"
                      (goto-char 9)
                      (should (looking-at "\*"))
                      ;; Check face of char before leading asterisk
                      (markdown-test-range-has-face 8 8 nil)
                      ;; Check face of italic range
                      (markdown-test-range-has-face 9 9 markdown-markup-face)
                      (markdown-test-range-has-face 10 16 markdown-italic-face)
                      (markdown-test-range-has-face 17 17 markdown-markup-face)
                      ;; Check face of point past leading asterisk
                      (markdown-test-range-has-face 18 18 nil)))

(ert-deftest test-markdown-font-lock/italics-2 ()
  "Test space after leading asterisk or underscore."
  (markdown-test-string
   "This is * not italic*, nor _ is this_."
   (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/italics-3 ()
  "Test that slash inside asterisks is not italic."
  (markdown-test-string
   "not italic *\\*"
   (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/italics-4 ()
  "Test that escaped asterisk inside italics is not bold."
  (markdown-test-string
   "italic **\\**"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 8 markdown-markup-face)
   (markdown-test-range-has-face 9 11 markdown-italic-face)
   (markdown-test-range-has-face 12 12 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-5 ()
  "Test italic single letter."
  (markdown-test-string
   "*a*"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 2 2 markdown-italic-face)
   (markdown-test-range-has-face 3 3 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-6 ()
  "Test multiline italics across list items."
  (markdown-test-string
   "* something about function foo_bar
* something else about foo_bar"
   (markdown-test-range-has-face 31 34 nil)
   (markdown-test-range-has-face 38 62 nil))
  (markdown-test-string
   "* something about function
  foo_bar
* something else about
  foo_bar"
   (markdown-test-range-has-face 30 36 nil)
   (markdown-test-range-has-face 63 69 nil))
  (markdown-test-string
   "foo_bar
* foo_bar"
   (markdown-test-range-has-face 4 7 nil)
   (markdown-test-range-has-face 11 14 nil)))

(ert-deftest test-markdown-font-lock/italics-7 ()
  "Underscores in URLs should not trigger italics."
  :expected-result :failed
  (markdown-test-string
   "<http://jblevins.org/research/centroid/cd_z_path.m>"
   (markdown-test-range-face-equals 2 50 markdown-link-face))
  (markdown-test-string
   "[1]: http://jblevins.org/research/centroid/cd_z_path.m"
   (markdown-test-range-face-equals 6 54 markdown-url-face))
  (markdown-test-string
   "[cd\\_z\\_path.m](http://jblevins.org/research/centroid/cd_z_path.m)"
   (markdown-test-range-face-equals 17 65 markdown-url-face)))

(ert-deftest test-markdown-font-lock/italics-after-hr ()
  "Test italics after a horizontal rule with asterisks."
  (markdown-test-string "* * *\n\n*italic*\n"
                        (markdown-test-range-has-face 1 5 markdown-header-delimiter-face)
                        (markdown-test-range-has-face 8 8 markdown-markup-face)
                        (markdown-test-range-has-face 9 14 markdown-italic-face)
                        (markdown-test-range-has-face 15 15 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-heading ()
  "Test italic overlay in a heading."
  (markdown-test-string
   "# *Italics* in a Heading"
   (markdown-test-range-has-face 3 3 markdown-markup-face)
   (markdown-test-range-has-face 4 10 markdown-italic-face)
   (markdown-test-range-has-face 11 11 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-link ()
  "Test italic overlay in an inline link."
  (markdown-test-string
   "*[italic link](http://www.link.com/)*"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 2 36 markdown-italic-face)
   (markdown-test-range-has-face 37 37 markdown-markup-face))
  (markdown-test-string
   "[*italic link*](http://www.link.com/)"
   (markdown-test-range-has-face 2 2 markdown-markup-face)
   (markdown-test-range-has-face 3 13 markdown-italic-face)
   (markdown-test-range-has-face 14 14 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-blockquote ()
  "Test italics overlay in a blockquote."
  (markdown-test-string
   "> *italics* inside a blockquote"
   (markdown-test-range-has-face 3 3 markdown-markup-face)
   (markdown-test-range-has-face 4 10 markdown-italic-face)
   (markdown-test-range-has-face 11 11 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-pre ()
  "Test italics overlay in a blockquote."
  (markdown-test-string
   "    *italics* inside a pre block"
   (markdown-test-range-has-face (point-min) (1- (point-max))
                                 markdown-pre-face)))

(ert-deftest test-markdown-font-lock/italics-and-code ()
  "Test seeming italics mixed with code."
  (markdown-test-string
   "define `var_1` and `var_2` inline code"
   (markdown-test-range-has-face 9 13 markdown-inline-code-face)
   (markdown-test-range-has-face 21 25 markdown-inline-code-face))
  (markdown-test-string
   "`var_1` and var_2"
   (markdown-test-range-has-face 2 6 markdown-inline-code-face)
   (markdown-test-range-has-face 8 17 nil))
  (markdown-test-string
   "var_1 and `var_2`"
   (markdown-test-range-has-face 1 10 nil)
   (markdown-test-range-has-face 12 16 markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/italics-and-code ()
  "Test seeming italics mixed with code."
  (markdown-test-string
   "[lg]: twilight_sm.png\n[sm]: twilight_lg.png"
   (markdown-test-range-has-face 7 21 markdown-url-face)
   (markdown-test-range-has-face 22 22 nil)
   (markdown-test-range-has-face 29 43 markdown-url-face)
   (markdown-test-range-has-face 28 28 nil)))

(ert-deftest test-markdown-font-lock/bold-1 ()
  "A simple bold test."
  (markdown-test-file "inline.text"
                      (goto-char 27)
                      (should (looking-at "\*\*"))
                      ;; Check face of char before leading asterisk
                      (markdown-test-range-has-face 26 26 nil)
                      ;; Check face of opening asterisks
                      (markdown-test-range-has-face 27 28 markdown-markup-face)
                      ;; Check face of bold range
                      (markdown-test-range-has-face 29 33 markdown-bold-face)
                      ;; Check face of closing asterisks
                      (markdown-test-range-has-face 34 35 markdown-markup-face)
                      ;; Check face of point past leading asterisk
                      (markdown-test-range-has-face 36 36 nil)))

(ert-deftest test-markdown-font-lock/bold-2 ()
  "Test space after leading asterisks or underscores."
  (markdown-test-string
   "This is ** not bold**, nor __ is this__ (but they match italics)."
   (markdown-test-range-has-face 1 8 nil)
   (markdown-test-range-has-face 9 9 markdown-markup-face)
   (markdown-test-range-has-face 10 19 markdown-italic-face)
   (markdown-test-range-has-face 20 20 markdown-markup-face)
   (markdown-test-range-has-face 21 27 nil)
   (markdown-test-range-has-face 28 28 markdown-markup-face)
   (markdown-test-range-has-face 29 37 markdown-italic-face)
   (markdown-test-range-has-face 38 38 markdown-markup-face)
   (markdown-test-range-has-face 39 (point-max) nil)))

(ert-deftest test-markdown-font-lock/bold-3 ()
  "Test escaped asterisk inside bold."
  (markdown-test-string
   "bold **\\***"
   (markdown-test-range-has-face 1 5 nil)
   (markdown-test-range-has-face 6 7 markdown-markup-face)
   (markdown-test-range-has-face 8 9 markdown-bold-face)
   (markdown-test-range-has-face 10 11 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-4 ()
  "Test bold single letter."
  (markdown-test-string
   "**a**"
   (markdown-test-range-has-face 1 2 markdown-markup-face)
   (markdown-test-range-has-face 3 3 markdown-bold-face)
   (markdown-test-range-has-face 4 5 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-after-hr ()
  "Test bold after a horizontal rule with asterisks."
  (markdown-test-string "* * *\n\n**bold**\n"
   (markdown-test-range-has-face 1 5 markdown-header-delimiter-face)
   (markdown-test-range-has-face 8 9 markdown-markup-face)
   (markdown-test-range-has-face 10 13 markdown-bold-face)
   (markdown-test-range-has-face 14 15 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-link ()
  "Test bold overlay in an inline link."
  (markdown-test-string
   "**[bold link](http://www.link.com/)**"
   (markdown-test-range-has-face 1 2 markdown-markup-face)
   (markdown-test-range-has-face 3 35 markdown-bold-face)
   (markdown-test-range-has-face 36 37 markdown-markup-face))
  (markdown-test-string
   "[**bold link**](http://www.link.com/)"
   (markdown-test-range-has-face 2 3 markdown-markup-face)
   (markdown-test-range-has-face 4 12 markdown-bold-face)
   (markdown-test-range-has-face 13 14 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-in-blockquote ()
  "Test bold overlay in a blockquote."
  (markdown-test-string
   "> **bold** inside a blockquote"
   (markdown-test-range-has-face 3 4 markdown-markup-face)
   (markdown-test-range-has-face 5 8 markdown-bold-face)
   (markdown-test-range-has-face 9 10 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-in-pre ()
  "Test bold overlay in a blockquote."
  (markdown-test-string
   "    **bold** inside a pre block"
   (markdown-test-range-has-face (point-min) (1- (point-max))
                                 markdown-pre-face)))

(ert-deftest test-markdown-font-lock/no-bold-in-code ()
  "Bold markers in inline code should not trigger bold."
  (markdown-test-string
   "`def __init__(self):`"
   (markdown-test-range-has-face 8 11 markdown-inline-code-face))
  (markdown-test-string
   "`**foo` bar `baz**`"
   (markdown-test-range-face-equals 2 6 markdown-inline-code-face)
   (markdown-test-range-face-equals 9 11 nil)
   (markdown-test-range-face-equals 14 18 markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/no-bold-in-math ()
  "Bold markers in math should not trigger bold."
  (markdown-test-file "math.text"
    (markdown-toggle-math t)
    (funcall markdown-test-font-lock-function)
    (markdown-test-range-has-face 279 299 markdown-math-face)
    (markdown-test-range-has-face 301 308 nil)
    (markdown-test-range-has-face 310 312 markdown-math-face)))

(ert-deftest test-markdown-font-lock/code-1 ()
  "A simple inline code test."
  (markdown-test-file "inline.text"
   (goto-char 45)
   (should (looking-at "`"))
   ;; Regular code span
   (markdown-test-range-has-face 45 45 markdown-markup-face)
   (markdown-test-range-has-face 46 49 markdown-inline-code-face)
   (markdown-test-range-has-face 50 50 markdown-markup-face)
   ;; Code containing backticks
   (markdown-test-range-has-face 61 62 markdown-markup-face)
   (markdown-test-range-has-face 63 87 markdown-inline-code-face)
   (markdown-test-range-has-face 88 89 markdown-markup-face)
   ;; Seven backquotes in a row
   (markdown-test-range-has-face 119 125 nil)
   ;; Backquotes at beginning or end
   (markdown-test-range-has-face 228 229 markdown-markup-face)
   (markdown-test-range-has-face 230 237 markdown-inline-code-face)
   (markdown-test-range-has-face 238 239 markdown-markup-face)
   (markdown-test-range-has-face 341 342 markdown-markup-face)
   (markdown-test-range-has-face 343 349 markdown-inline-code-face)
   (markdown-test-range-has-face 350 351 markdown-markup-face)
   ;; Backslash as final character
   (markdown-test-range-has-face 460 460 markdown-markup-face)
   (markdown-test-range-has-face 461 467 markdown-inline-code-face)
   (markdown-test-range-has-face 468 468 markdown-markup-face)
   ;; Escaping of leading backquotes
   (markdown-test-range-has-face 586 592 nil)
   (markdown-test-range-has-face 597 603 nil)
   ;; A code span crossing lines
   (markdown-test-range-has-face 652 656 nil)
   (markdown-test-range-has-face 657 657 markdown-markup-face)
   (markdown-test-range-has-face 658 665 markdown-inline-code-face)
   (markdown-test-range-has-face 666 666 markdown-markup-face)
   ;; Three backquotes: same line, across lines, not across blocks
   (markdown-test-range-has-face 695 748 nil)
   (markdown-test-range-has-face 749 750 markdown-markup-face)
   (markdown-test-range-has-face 751 755 markdown-inline-code-face)
   (markdown-test-range-has-face 756 757 markdown-markup-face)
   (markdown-test-range-has-face 758 805 nil)
   (markdown-test-range-has-face 806 807 markdown-markup-face)
   (markdown-test-range-has-face 808 812 markdown-inline-code-face)
   (markdown-test-range-has-face 813 814 markdown-markup-face)
   (markdown-test-range-has-face 815 891 nil)
   ))

(ert-deftest test-markdown-font-lock/code-2 ()
  "Multiple code spans in a row and on different lines."
  (markdown-test-string "`foo` `bar` `baz`"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 2 4 markdown-inline-code-face)
   (markdown-test-range-has-face 5 5 markdown-markup-face)
   (markdown-test-range-has-face 6 6 nil)
   (markdown-test-range-has-face 7 7 markdown-markup-face)
   (markdown-test-range-has-face 8 10 markdown-inline-code-face)
   (markdown-test-range-has-face 11 11 markdown-markup-face)
   (markdown-test-range-has-face 12 12 nil)
   (markdown-test-range-has-face 13 13 markdown-markup-face)
   (markdown-test-range-has-face 14 16 markdown-inline-code-face)
   (markdown-test-range-has-face 17 17 markdown-markup-face))
  (markdown-test-string "`a`\n`b`\n`c`\n"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 2 2 markdown-inline-code-face)
   (markdown-test-range-has-face 3 3 markdown-markup-face)
   (markdown-test-range-has-face 4 4 nil)
   (markdown-test-range-has-face 5 5 markdown-markup-face)
   (markdown-test-range-has-face 6 6 markdown-inline-code-face)
   (markdown-test-range-has-face 7 7 markdown-markup-face)
   (markdown-test-range-has-face 8 8 nil)
   (markdown-test-range-has-face 9 9 markdown-markup-face)
   (markdown-test-range-has-face 10 10 markdown-inline-code-face)
   (markdown-test-range-has-face 11 11 markdown-markup-face)
   (markdown-test-range-has-face 12 12 nil))
  (markdown-test-string "a`foo`b`bar`c`baz`d"
   (markdown-test-range-has-face 1 1 nil)
   (markdown-test-range-has-face 2 2 markdown-markup-face)
   (markdown-test-range-has-face 3 5 markdown-inline-code-face)
   (markdown-test-range-has-face 6 6 markdown-markup-face)
   (markdown-test-range-has-face 7 7 nil)
   (markdown-test-range-has-face 8 8 markdown-markup-face)
   (markdown-test-range-has-face 9 11 markdown-inline-code-face)
   (markdown-test-range-has-face 12 12 markdown-markup-face)
   (markdown-test-range-has-face 13 13 nil)
   (markdown-test-range-has-face 14 14 markdown-markup-face)
   (markdown-test-range-has-face 15 17 markdown-inline-code-face)
   (markdown-test-range-has-face 18 18 markdown-markup-face)
   (markdown-test-range-has-face 19 19 nil)))

(ert-deftest test-markdown-font-lock/code-3 ()
  "Backslashes don't escape backticks inside of inline code strings."
  (markdown-test-string
   "`foo\\`bar`"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 2 5 markdown-inline-code-face)
   (markdown-test-range-has-face 6 6 markdown-markup-face)
   (markdown-test-range-has-face 7 10 nil)))

(ert-deftest test-markdown-font-lock/code-link-precedence ()
  "Test that inline code takes precedence over inline links.
Test currently fails because this case isn't handled properly."
  :expected-result :failed
  (markdown-test-string
   "[not a `link](/foo`)"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 8 markdown-markup-face)
   (markdown-test-range-has-face 9 18 markdown-inline-code-face)
   (markdown-test-range-has-face 19 19 markdown-markup-face)
   (markdown-test-range-has-face 20 20 nil)))

(ert-deftest test-markdown-font-lock/kbd ()
  "Test font lock for <kbd> tags."
  (markdown-test-string "<kbd>C-c <</kbd>"
   (markdown-test-range-has-face 1 5 markdown-markup-face)
   (markdown-test-range-has-face 6 10 markdown-inline-code-face)
   (markdown-test-range-has-face 11 16 markdown-markup-face))
  (markdown-test-string "To quit Emacs, press <kbd>C-x C-c</kbd>."
   (markdown-test-range-has-face 1 21 nil)
   (markdown-test-range-has-face 22 26 markdown-markup-face)
   (markdown-test-range-has-face 27 33 markdown-inline-code-face)
   (markdown-test-range-has-face 34 39 markdown-markup-face)
   (markdown-test-range-has-face 40 40 nil)))

(ert-deftest test-markdown-font-lock/lists-1 ()
  "A simple list marker font lock test."
  (markdown-test-file "lists.text"
   (dolist (loc (list 1063 1283 1659 1830 1919 2150 2393 2484
                      2762 2853 3097 3188 3700 3903 4009))
     (goto-char loc)
     (should (looking-at "[*+-]"))
     (markdown-test-range-has-face loc loc markdown-list-face))))

(ert-deftest test-markdown-font-lock/pre-1 ()
  "Nested list and pre block font lock test."
  (markdown-test-file "nested-list.text"
    (dolist (loc (list 4 29 194 224 491 525))
      (markdown-test-range-has-face loc loc markdown-list-face))
    (markdown-test-range-has-face 6 25 nil)
    (markdown-test-range-has-face 31 83 nil)
    (markdown-test-range-has-face 85 154 markdown-pre-face)
    (markdown-test-range-has-face 157 189 nil)
    (markdown-test-range-has-face 196 215 nil)
    (markdown-test-range-has-face 226 403 nil)
    (markdown-test-range-has-face 405 481 markdown-pre-face)
    (markdown-test-range-has-face 493 512 nil)
    (markdown-test-range-has-face 527 546 nil)
    (markdown-test-range-has-face 548 580 markdown-pre-face)))

(ert-deftest test-markdown-font-lock/pre-2 ()
  (markdown-test-string "* item\n\nreset baseline\n\n    pre block\n"
   (markdown-test-range-has-face 1 1 markdown-list-face)
   (markdown-test-range-has-face 2 23 nil)
   (markdown-test-range-has-face 29 37 markdown-pre-face)))

(ert-deftest test-markdown-font-lock/pre-3 ()
  (markdown-test-string "It is interesting to see what happens when one queries
`social upheaval` and `protopalatial era`.

* `social upheaval`: the follwing queries have been tried:

    social upheaval subClassOf"
   (markdown-test-range-has-face 160 190 nil)))

(ert-deftest test-markdown-font-lock/pre-4 ()
  "Pre blocks must be preceded by a blank line"
  (markdown-test-string "Paragraph
    for (var i = 0; i < 10; i++) {
        console.log(i);
    }"
    (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/fenced-1 ()
  "Test fenced code blocks containing four-space indents."
  (markdown-test-string "Fenced code block

~~~
if (x)
    foo();

if (y)
    bar();
~~~
"
   (markdown-test-range-has-face 1 19 nil)
   (markdown-test-range-has-face 20 22 markdown-markup-face)
   (markdown-test-range-has-face 24 60 markdown-pre-face)
   (markdown-test-range-has-face 61 63 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/gfm-fenced-1 ()
  "Test GFM-style fenced code blocks (1)."
  (markdown-test-string "```ruby
require 'redcarpet'
markdown = Redcarpet.new('Hello World!')
puts markdown.to_html
```"
   (markdown-test-range-has-face 1 3 markdown-markup-face) ; ```
   (markdown-test-range-has-face 4 7 markdown-language-keyword-face) ; ruby
   (markdown-test-range-has-face 9 90 markdown-pre-face) ; code
   (markdown-test-range-has-face 92 94 markdown-markup-face))) ; ```

(ert-deftest test-markdown-font-lock/gfm-fenced-2 ()
  "Test GFM-style fenced code blocks (2)."
  (markdown-test-string "```{r sum}\n2+2\n```"
   (markdown-test-range-has-face 1 3 markdown-markup-face) ; ```
   (markdown-test-range-has-face 4 10 markdown-language-keyword-face) ; {r sum}
   (markdown-test-range-has-face 12 14 markdown-pre-face) ; 2+2
   (markdown-test-range-has-face 16 18 markdown-markup-face))) ; ```

(ert-deftest test-markdown-font-lock/gfm-fenced-3 ()
  "GFM-style code blocks need not be preceded by a blank line."
  (markdown-test-string "Paragraph
```js
for (var i = 0; i < 10; i++) {
    console.log(i);
}
```"
    (markdown-test-range-has-face 1 10 nil) ; Paragraph
    (markdown-test-range-has-face 11 13 markdown-markup-face) ; ```
    (markdown-test-range-has-face 14 15 markdown-language-keyword-face) ; js
    (markdown-test-range-has-face 17 68 markdown-pre-face)
    (markdown-test-range-has-face 70 72 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/atx-no-spaces ()
  "Test font-lock for atx headers with no spaces."
  (markdown-test-string "##abc##"
   (markdown-test-range-has-face 1 7 nil))
  (markdown-test-string "##"
   (markdown-test-range-has-face 1 2 nil))
  (markdown-test-string "###"
   (markdown-test-range-has-face 1 3 nil)))

(ert-deftest test-markdown-font-lock/setext-1-letter ()
  "An edge case for level-one setext headers."
  (markdown-test-string "a\n=\n"
   (markdown-test-range-has-face 1 1 markdown-header-face-1)
   (markdown-test-range-has-face 3 3 markdown-header-rule-face)))

(ert-deftest test-markdown-font-lock/setext-2-letter ()
  "An edge case for level-two setext headers."
  (markdown-test-string "b\n-\n"
   (markdown-test-range-has-face 1 1 markdown-header-face-2)
   (markdown-test-range-has-face 3 3 markdown-header-rule-face)))

(ert-deftest test-markdown-font-lock/inline-links ()
  "Test font lock for inline links."
  (markdown-test-file "inline.text"
   (markdown-test-range-has-face 925 925 markdown-markup-face)
   (markdown-test-range-has-face 926 929 markdown-link-face)
   (markdown-test-range-has-face 930 931 markdown-markup-face)
   (markdown-test-range-has-face 932 949 markdown-url-face)
   (markdown-test-range-has-face 951 957 markdown-link-title-face)
   (markdown-test-range-has-face 958 958 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/pre-comment ()
  "Test comments inside of a pre block."
  (markdown-test-string "    <!-- pre, not comment -->"
   (markdown-test-range-has-face (point-min) (1- (point-max)) markdown-pre-face)))

(ert-deftest test-markdown-font-lock/inline-code-comment ()
  "Test comments inside of a pre block."
  (markdown-test-string "`<h1> <!-- HTML comment inside inline code -->`"
   (markdown-test-range-has-face (1+ (point-min)) (- (point-max) 2) markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/comment-hanging-indent ()
  "Test comments with hanging indentation."
  (markdown-test-string "<!-- This comment has\n    hanging indentation -->"
   (markdown-test-range-has-face (point-min) (1- (point-max)) markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-multiple ()
  "Test multiple single-line comments in arow."
  (markdown-test-string "<!-- This is a comment -->\n<!-- And so is this -->"
   (markdown-test-range-has-face
    (point-at-bol) (1- (point-at-eol)) markdown-comment-face)
   (forward-line)
   (markdown-test-range-has-face
    (point-at-bol) (1- (point-at-eol)) markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-list-items ()
  "Test comment with list inside."
  (markdown-test-string
   "<!--
  - note 1;
  - note 2.
-->"
   (markdown-test-range-face-equals (point-min) (1- (point-max))
                                    markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-angle-bracket ()
  "Regression test for GH-117."
  (markdown-test-string "<!-- > test -->"
   (markdown-test-range-face-equals (point-min) (1- (point-max))
                                    markdown-comment-face)))

(ert-deftest test-markdown-font-lock/footnote-markers-links ()
  "Test an edge case involving footnote markers and inline reference links."
  (markdown-test-string "Harvard[^1] [tuition][]"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 8 markdown-markup-face)
   (markdown-test-range-has-face 10 10 markdown-footnote-face)
   (markdown-test-range-has-face 11 11 markdown-markup-face)
   (markdown-test-range-has-face 12 12 nil)
   (markdown-test-range-has-face 13 13 markdown-markup-face)
   (markdown-test-range-has-face 14 20 markdown-link-face)
   (markdown-test-range-has-face 21 21 markdown-markup-face)
   (markdown-test-range-has-face 22 23 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/mmd-metadata ()
  "Basic MultMarkdown metadata tests."
  (markdown-test-string "Title: peg-multimarkdown User's Guide
Author: Fletcher T. Penney
Base Header Level: 2"
   (markdown-test-range-has-face 1 5 markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 markdown-markup-face)
   (markdown-test-range-has-face 8 37 markdown-metadata-value-face)
   (markdown-test-range-has-face 39 44 markdown-metadata-key-face)
   (markdown-test-range-has-face 46 46 markdown-markup-face)
   (markdown-test-range-has-face 47 64 markdown-metadata-value-face)
   (markdown-test-range-has-face 66 82 markdown-metadata-key-face)
   (markdown-test-range-has-face 83 83 markdown-markup-face)
   (markdown-test-range-has-face 85 85 markdown-metadata-value-face))
  ;; Avoid triggering when a title contains a colon (e.g., Markdown: Syntax)
  (markdown-test-file "syntax.text"
   (markdown-test-range-has-face 1 16 markdown-header-face-1)))

(ert-deftest test-markdown-font-lock/mmd-metadata-after-header ()
  "Ensure that similar lines are not matched after the header."
  (markdown-test-string "Title: peg-multimarkdown User's Guide

Author: Fletcher T. Penney
Base Header Level: 2"
   (markdown-test-range-has-face 1 5 markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 markdown-markup-face)
   (markdown-test-range-has-face 8 37 markdown-metadata-value-face)
   (markdown-test-range-has-face 40 65 nil)
   (markdown-test-range-has-face 67 86 nil)))

(ert-deftest test-markdown-font-lock/pandoc-metadata ()
  "Basic Pandoc metadata tests."
  (markdown-test-string "% title
  two-line title
% first author;
  second author
% date

body"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 3 24 markdown-metadata-value-face)
   (markdown-test-range-has-face 26 26 markdown-markup-face)
   (markdown-test-range-has-face 28 56 markdown-metadata-value-face)
   (markdown-test-range-has-face 58 58 markdown-markup-face)
   (markdown-test-range-has-face 60 63 markdown-metadata-value-face)
   (markdown-test-range-has-face 64 69 nil)))

(ert-deftest test-markdown-font-lock/yaml-metadata ()
  "Basic YAML metadata tests."
  (markdown-test-string
   "---
layout: post
date: 2015-08-13 11:35:25 EST
---
"
   (markdown-test-range-has-face 1 3 markdown-markup-face)
   (markdown-test-range-has-face 5 10 markdown-metadata-key-face)
   (markdown-test-range-has-face 11 11 markdown-markup-face)
   (markdown-test-range-has-face 13 16 markdown-metadata-value-face)
   (markdown-test-range-has-face 18 21 markdown-metadata-key-face)
   (markdown-test-range-has-face 22 22 markdown-markup-face)
   (markdown-test-range-has-face 24 46 markdown-metadata-value-face)
   (markdown-test-range-has-face 48 50 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/toml-metadata ()
  "Basic TOML metadata tests."
  (markdown-test-string
   "---
layout = post
date = 2015-08-13 11:35:25 EST
---
"
   (markdown-test-range-has-face 1 3 markdown-markup-face)
   (markdown-test-range-has-face 5 10 markdown-metadata-key-face)
   (markdown-test-range-has-face 12 12 markdown-markup-face)
   (markdown-test-range-has-face 14 17 markdown-metadata-value-face)
   (markdown-test-range-has-face 19 22 markdown-metadata-key-face)
   (markdown-test-range-has-face 24 24 markdown-markup-face)
   (markdown-test-range-has-face 26 48 markdown-metadata-value-face)
   (markdown-test-range-has-face 50 52 markdown-markup-face)))

(ert-deftest test-markdown-font-lock/pandoc-yaml-metadata ()
  "Basic yaml metadata tests, with pandoc syntax."
  (let ((markdown-use-pandoc-style-yaml-metadata t))
    (markdown-test-string
     "some text

---
layout: post
date: 2015-08-13 11:35:25 EST
...

more text

---
layout: post
date: 2015-08-13 11:35:25 EST
---

But this is merely a code block

```
---
layout: post
date: 2015-08-13 11:35:25 EST
---
```
"
     ;; first section
     (markdown-test-range-has-face 12 14 markdown-markup-face)
     (markdown-test-range-has-face 16 21 markdown-metadata-key-face)
     (markdown-test-range-has-face 22 22 markdown-markup-face)
     (markdown-test-range-has-face 24 27 markdown-metadata-value-face)
     (markdown-test-range-has-face 29 32 markdown-metadata-key-face)
     (markdown-test-range-has-face 33 33 markdown-markup-face)
     (markdown-test-range-has-face 35 57 markdown-metadata-value-face)
     (markdown-test-range-has-face 59 61 markdown-markup-face)
     ;; second section
     (markdown-test-range-has-face 75 77 markdown-markup-face)
     (markdown-test-range-has-face 79 84 markdown-metadata-key-face)
     (markdown-test-range-has-face 85 85 markdown-markup-face)
     (markdown-test-range-has-face 87 90 markdown-metadata-value-face)
     (markdown-test-range-has-face 92 95 markdown-metadata-key-face)
     (markdown-test-range-has-face 96 96 markdown-markup-face)
     (markdown-test-range-has-face 98 120 markdown-metadata-value-face)
     (markdown-test-range-has-face 122 124 markdown-markup-face)
     ;; third section
     (markdown-test-range-has-face 160 162 markdown-markup-face)
     (markdown-test-range-has-face 164 213 markdown-pre-face)
     (markdown-test-range-has-face 215 217 markdown-markup-face))))

(ert-deftest test-markdown-font-lock/line-break ()
  "Basic line break tests."
  (markdown-test-string "    \nasdf  \n"
   (markdown-test-range-has-face 1 9 nil)
   (markdown-test-range-has-face 10 11 markdown-line-break-face)))

(ert-deftest test-markdown-font-lock/blockquote-bold ()
  "Test font lock for bold inside of a blockquote."
  (markdown-test-string
   "> **bold**"
   (markdown-test-range-has-face 2 10 markdown-blockquote-face)
   (markdown-test-range-has-face 5 8 markdown-bold-face)))

(ert-deftest test-markdown-font-lock/blockquote-italic ()
  "Test font lock for italic inside of a blockquote."
  (markdown-test-string
   "> *italic*"
   (markdown-test-range-has-face 2 10 markdown-blockquote-face)
   (markdown-test-range-has-face 4 9 markdown-italic-face)))

(ert-deftest test-markdown-font-lock/blockquote-link ()
  "Test font lock for links inside of a blockquote.
This test will fail until font lock for inline links inside
blockquotes is implemented (at present, the blockquote face
takes precedence)."
  :expected-result :failed
  (markdown-test-string
   "> [link](url)"
   (markdown-test-range-has-face 1 13 markdown-blockquote-face)
   (markdown-test-range-has-face 3 8 markdown-link-face)
   (markdown-test-range-has-face 9 13 markdown-url-face)))

(ert-deftest test-markdown-font-lock/blockquote-comment ()
  "Test font lock for comments inside of a blockquote."
  (markdown-test-string
   "> <!-- comment -->"
   (markdown-test-range-has-face 1 1 markdown-markup-face)
   (markdown-test-range-has-face 3 18 markdown-comment-face)))

(ert-deftest test-markdown-font-lock/pre-override ()
  "Test that font lock for pre blocks overrides everything else."
  (markdown-test-string
   "    **bold**
    _italic_
    <!-- comment -->
    [link](url)
    * list"
   (markdown-test-range-has-face 1 73 markdown-pre-face)))

(ert-deftest test-markdown-font-lock/gfm-code-block-font-lock ()
  "GFM code block font lock test. Now in base markdown-mode as well!"
  (markdown-test-file "gfm.text"
    (markdown-test-range-has-face 2639 2641 markdown-markup-face) ; ```
    (markdown-test-range-has-face 2642 2645 markdown-language-keyword-face) ; lang
    (markdown-test-range-has-face 2647 2728 markdown-pre-face) ; code
    (markdown-test-range-has-face 2730 2732 markdown-markup-face))) ; ```

(ert-deftest test-markdown-font-lock/reference-definition ()
  "Reference definitions should not include ]."
  (markdown-test-string "[1]: http://daringfireball.net/ \"title\""
    (markdown-test-range-has-face 2 2 markdown-reference-face) ; 1
    (markdown-test-range-has-face 6 31 markdown-url-face) ; URL
    (markdown-test-range-has-face 34 38 markdown-link-title-face)) ; title
  (markdown-test-string "[foo][1] and [bar][2]: not a reference definition"
    (markdown-test-range-has-face 2 4 markdown-link-face) ; foo
    (markdown-test-range-has-face 7 7 markdown-reference-face) ; 1
    (markdown-test-range-has-face 9 13 nil) ; [ ]and[ ]
    (markdown-test-range-has-face 15 17 markdown-link-face) ; bar
    (markdown-test-range-has-face 20 20 markdown-reference-face) ; 2
    (markdown-test-range-has-face 22 49 nil))) ; [ ]and[ ]

;;; Markdown Parsing Functions:

(ert-deftest test-markdown-parsing/extend-region-function ()
  "Test `markdown-syntax-propertize-extend-region'.
Should return a cons (NEW-START . NEW-END) or nil if no
adjustment should be made. Function is called repeatedly until it
returns nil."
  (markdown-test-file
   "inline.text"
   (should (equal (markdown-syntax-propertize-extend-region 1 17)
                  (cons 1 91)))
   (should (equal (markdown-syntax-propertize-extend-region 2 17)
                  (cons 1 91)))
   (should (equal (markdown-syntax-propertize-extend-region 1 91)
                  nil))
   (should (equal (markdown-syntax-propertize-extend-region 93 157)
                  nil))
   (should (equal (markdown-syntax-propertize-extend-region 496 502)
                  (cons 486 510)))
   (should (equal (markdown-syntax-propertize-extend-region 486 510)
                  nil))
   ;; Region that begins and ends with \n\n should not be extended
   (should (equal (markdown-syntax-propertize-extend-region 157 355)
                  nil))))

(defun markdown-test-check-match-limits (prop num begin end &optional pos)
  (let* ((posn (or pos (point)))
         (props (get-text-property posn prop)))
    (save-match-data
      (set-match-data props)
      (and (match-beginning num) (match-end num)
           (= (match-beginning num) begin)
           (= (match-end num) end)))))

(ert-deftest test-markdown-parsing/syntax-with-adjacent-code-blocks ()
  "Test `markdown-syntax-propertize-fenced-code-blocks' with adjacent blocks."
  (markdown-test-string
   "~~~ shell
#!/bin/sh

echo \"Hello, world!\"
~~~

~~~ shell
#!/bin/sh

echo \"Hello, world v2!\"
~~~
"
   (let ((start-top-1 (make-marker)) (end-top-1 (make-marker))
         (start-lang-1 (make-marker)) (end-lang-1 (make-marker))
         (start-mid-1 (make-marker)) (end-mid-1 (make-marker))
         (start-bottom-1 (make-marker)) (end-bottom-1 (make-marker))
         (between (make-marker))
         (start-top-2 (make-marker)) (end-top-2 (make-marker))
         (start-lang-2 (make-marker)) (end-lang-2 (make-marker))
         (start-mid-2 (make-marker)) (end-mid-2 (make-marker))
         (start-bottom-2 (make-marker)) (end-bottom-2 (make-marker)))
     ;; First code block
     (set-marker start-top-1 1)
     (set-marker end-top-1 4)
     (set-marker start-lang-1 5)
     (set-marker end-lang-1 10)
     (set-marker start-mid-1 11)
     (set-marker end-mid-1 43)
     (set-marker start-bottom-1 43)
     (set-marker end-bottom-1 46)
     ;; check top tildes
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 1 (marker-position start-top-1)
              (marker-position end-top-1) (marker-position start-top-1)))
     ;; check top language specifier
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 2 (marker-position start-lang-1)
              (marker-position end-lang-1) (marker-position start-lang-1)))
     ;; check text in between
     (should (markdown-test-check-match-limits
              'markdown-fenced-code 0 (marker-position start-mid-1)
              (marker-position end-mid-1) (marker-position start-mid-1)))
     ;; check bottom tildes
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-end 1 (marker-position start-bottom-1)
              (marker-position end-bottom-1) (marker-position start-bottom-1)))
     ;; Point between code blocks
     (set-marker between 47)
     (should (equal (get-text-property between 'markdown-fenced-code)
                    nil))
     ;; Second code block
     (set-marker start-top-2 48)
     (set-marker end-top-2 51)
     (set-marker start-lang-2 52)
     (set-marker end-lang-2 57)
     (set-marker start-mid-2 58)
     (set-marker end-mid-2 93)
     (set-marker start-bottom-2 93)
     (set-marker end-bottom-2 96)
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 1 (marker-position start-top-2)
              (marker-position end-top-2) (marker-position start-top-2)))
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 2 (marker-position start-lang-2)
              (marker-position end-lang-2) (marker-position start-lang-2)))
     (should (markdown-test-check-match-limits
              'markdown-fenced-code 0 (marker-position start-mid-2)
              (marker-position end-mid-2) (marker-position start-mid-2)))
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-end 1 (marker-position start-bottom-2)
              (marker-position end-bottom-2) (marker-position start-bottom-2)))
     ;; ;; Move point between code blocks and insert a character
     (goto-char between)
     (insert "x")
     ;; Re-propertize region after change
     (let ((range (markdown-syntax-propertize-extend-region (1- between) (point-max))))
       (markdown-syntax-propertize (car range) (cdr range)))
     ;; Re-check first code block
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 1 (marker-position start-top-1)
              (marker-position end-top-1) (marker-position start-top-1)))
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 2 (marker-position start-lang-1)
              (marker-position end-lang-1) (marker-position start-lang-1)))
     (should (markdown-test-check-match-limits
              'markdown-fenced-code 0 (marker-position start-mid-1)
              (marker-position end-mid-1) (marker-position start-mid-1)))
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-end 1 (marker-position start-bottom-1)
              (marker-position end-bottom-1) (marker-position start-bottom-1)))
     ;; Re-check point between code blocks
     (should (equal (get-text-property between 'markdown-fenced-code)
                    nil))
     ;; Re-check second code block
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 1 (marker-position start-top-2)
              (marker-position end-top-2) (marker-position start-top-2)))
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-begin 2 (marker-position start-lang-2)
              (marker-position end-lang-2) (marker-position start-lang-2)))
     (should (markdown-test-check-match-limits
              'markdown-fenced-code 0 (marker-position start-mid-2)
              (marker-position end-mid-2) (marker-position start-mid-2)))
     (should (markdown-test-check-match-limits
              'markdown-tilde-fence-end 1 (marker-position start-bottom-2)
              (marker-position end-bottom-2)
              (marker-position start-bottom-2))))))

(ert-deftest test-markdown-parsing/propertize-fenced-in-between ()
  "Test whether `markdown-syntax-propertize-fenced-block-constructs' handles the
case when it can't propertize both the start and end of a fenced block within a
single pass (the end of the block is past the END argument)."
  (markdown-test-string
      "~~~ shell
#!/bin/sh

echo \"Hello, world!\"
~~~
"
    (set-text-properties (point-min) (point-max) nil)
    ;; syntax-propertize up to right after hashbang
    (markdown-syntax-propertize-fenced-block-constructs (point-min) 21)
    ;; ~~~ shell should be propertized, but nothing else
    ;; check tildes
    (should (markdown-test-check-match-limits
             'markdown-tilde-fence-begin 1 1 4 1))
    ;; check language
    (should (markdown-test-check-match-limits
             'markdown-tilde-fence-begin 2 5 10 5))
    ;; middle should not be propertized
    (should-not (get-text-property 11 'markdown-fenced-code))
    ;; neither should end
    (should-not (get-text-property 43 'markdown-tilde-fence-end))
    (markdown-syntax-propertize-fenced-block-constructs 21 (point-max))
    ;; everything should be propertized now
    ;; re-check top
    (should (markdown-test-check-match-limits
             'markdown-tilde-fence-begin 1 1 4 1))
    (should (markdown-test-check-match-limits
             'markdown-tilde-fence-begin 2 5 10 5))
    ;; check middle
    (should (markdown-test-check-match-limits 'markdown-fenced-code 0 10 43 10))
    ;; check ending tildes
    (should (markdown-test-check-match-limits
             'markdown-tilde-fence-end 1 43 46 43))))

(ert-deftest test-markdown-parsing/get-code-block-at-pos ()
  "Test whether `markdown-code-block-at-pos' works in all situations. All
  situations are:
1. pre block
2. tilde block
3. gfm block
4. yaml metadata block"
  (let ((markdown-use-pandoc-style-yaml-metadata t))
    (markdown-test-string
        "
~~~ ruby
some_ruby_fun()
~~~

---
a: b
---

``` {.bash}
#!/bin/sh
echo hey
```

    pre code
    random stuff
    more preformatted code

---
data: pandoc
...
"
      ;; start/mid/end at tilde block
      (should (equal (markdown-code-block-at-pos 2) (list 2 30)))
      (should (equal (markdown-code-block-at-pos 11) (list 2 30)))
      (should (equal (markdown-code-block-at-pos 27) (list 2 30)))
      ;; yaml metadata block
      (should (equal (markdown-code-block-at-pos 32) (list 32 44)))
      (should (equal (markdown-code-block-at-pos 36) (list 32 44)))
      (should (equal (markdown-code-block-at-pos 41) (list 32 44)))
      ;; gfm block
      (should (equal (markdown-code-block-at-pos 46) (list 46 80)))
      (should (equal (markdown-code-block-at-pos 58) (list 46 80)))
      (should (equal (markdown-code-block-at-pos 77) (list 46 80)))
      ;; pre block
      (should (equal (markdown-code-block-at-pos 82) (list 82 138)))
      (should (equal (markdown-code-block-at-pos 99) (list 82 138)))
      (should (equal (markdown-code-block-at-pos 137) (list 82 138)))
      ;; pandoc yaml metadata block (should work if yaml above works)
      (should (equal (markdown-code-block-at-pos 140) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 142) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 144) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 157) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 159) (list 140 160))))))

(ert-deftest test-markdown-parsing/syntax-get-fenced-blocks ()
  "Test whether *-get-fenced-block-* functions work in the case where a block is
only partially propertized."
  (save-match-data
    (markdown-test-string
     "~~~
"
     (should (equal (markdown-syntax-propertize-extend-region
                     (point-min) (point-max))
                    nil))
     (goto-char 1)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-begin))
     (should (equal (markdown-get-fenced-block-from-start
                     'markdown-tilde-fence-begin)
                    nil)))
    (markdown-test-string
     "~~~
~~~"
     (goto-char 1)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-begin))
     (should (equal (markdown-get-fenced-block-from-start
                     'markdown-tilde-fence-begin)
                    (list 1 8)))
     (should (equal (markdown-code-block-at-point) (list 1 8)))
     (goto-char 5)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-end))
     (should (equal (markdown-get-fenced-block-from-end
                     'markdown-tilde-fence-end)
                    (list 1 8)))
     (should (equal (markdown-code-block-at-point) (list 1 8))))
    (markdown-test-string
     "~~~

~~~"
     (goto-char 1)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-begin))
     (should (equal (markdown-get-fenced-block-from-start
                     'markdown-tilde-fence-begin)
                    (list 1 9)))
     (should (equal (markdown-code-block-at-point) (list 1 9)))
     (goto-char 5)
     (set-match-data (markdown-text-property-at-point 'markdown-fenced-code))
     (should (equal (markdown-get-fenced-block-from-middle
                     'markdown-fenced-code)
                    (list 1 9)))
     (should (equal (markdown-code-block-at-point) (list 1 9)))
     (goto-char 6)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-end))
     (should (equal (markdown-get-fenced-block-from-end
                     'markdown-tilde-fence-end)
                    (list 1 9)))
     (should (equal (markdown-code-block-at-point) (list 1 9))))))

(ert-deftest test-markdown-parsing/reference-definition-basic ()
  "Test reference definition function."
  (markdown-test-file "syntax.text"
   ;; Test accuracy of returned text and bounds
   (should (equal (markdown-reference-definition "1")
                  (list "http://docutils.sourceforge.net/mirror/setext.html" 1942 1992)))
   (should (equal (markdown-reference-definition "2")
                  (list "http://www.aaronsw.com/2002/atx/" 2000 2032)))
   ;; Test that match data remains intact
   (should (string-equal (match-string 5) "http://www.aaronsw.com/2002/atx/"))
   ;; Test anchor-only relative URL
   (should (equal (markdown-reference-definition "bq")
                  (list "#blockquote" 7536 7547)))
   ;; Example references that appear in pre blocks in the text
   (should (not (markdown-reference-definition "")))
   (should (not (markdown-reference-definition "id")))
   (should (not (markdown-reference-definition "foo")))
   (should (not (markdown-reference-definition "A")))
   (should (not (markdown-reference-definition "Google")))
   ;; Test that we don't pick up other text in square brackets
   (should (not (markdown-reference-definition "blockquoting")))
   (should (not (markdown-reference-definition "square brackets")))
   ;; Test case insensitivity
   (should (equal (markdown-reference-definition "SRC")
                  (list "/projects/markdown/syntax.text" 1245 1275)))))

(ert-deftest test-markdown-parsing/get-defined-references ()
  "Test `markdown-get-defined-references'."
  (markdown-test-file "syntax.text"
   (should (equal (markdown-get-defined-references)
                  '("src" "1" "2" "3" "4" "5" "6" "bq" "l"))))
  (markdown-test-file "outline.text"
   (should (equal (markdown-get-defined-references) nil)))
  (markdown-test-file "wiki-links.text"
   (should (equal (markdown-get-defined-references) nil))))

(defun markdown-test-test-region (beg end)
  (goto-char (1- beg))
  (should-not (markdown-code-at-point-p))
  (goto-char (1+ end))
  (should-not (markdown-code-at-point-p))
  (dolist (loc (number-sequence beg end))
    (goto-char loc)
    (should (markdown-code-at-point-p))
    (should (equal (match-beginning 0) beg))
    (should (equal (match-end 0) end))))

(ert-deftest test-markdown-parsing/code-at-point-inline ()
  "Test `markdown-code-at-point-p'."
  (markdown-test-file "inline.text"
    (markdown-test-test-region 45 51) ; Regular code span
    (markdown-test-test-region 61 90) ; Code containing backticks
    (markdown-test-test-region 228 240) ; Backquotes at beginning
    (markdown-test-test-region 341 352) ; Backquotes at end
    (markdown-test-test-region 460 469) ; Backslash as final character
    (markdown-test-test-region 657 667) ; A code span crossing lines
    (markdown-test-test-region 749 758) ; Three backquotes on same line
    (markdown-test-test-region 806 815) ; Three backquotes across lines
    ))

(ert-deftest test-markdown-parsing/code-at-point-one-space ()
  "Test `markdown-code-at-point-p' with multiple code spans in a row."
  (markdown-test-string "`foo` `bar` `baz`"
    (dolist (loc (number-sequence 1 6))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 1 6 1 2 2 5 5 6))))
    (dolist (loc (number-sequence 7 12))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 7 12 7 8 8 11 11 12))))
    (dolist (loc (number-sequence 13 18))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 13 18 13 14 14 17 17 18))))))

(ert-deftest test-markdown-parsing/code-at-point-no-space ()
  "Test `markdown-code-at-point-p' with multiple code spans in a row."
  (markdown-test-string "a`foo`b`bar`c`baz`d"
    (goto-char 1)                       ; "a"
    (should-not (markdown-code-at-point-p))
    (dolist (loc (number-sequence 2 7)) ; "`foo`b"
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 2 7 2 3 3 6 6 7))))
    (dolist (loc (number-sequence 8 13)) ; "`bar`c"
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 8 13 8 9 9 12 12 13))))
    (dolist (loc (number-sequence 14 19)) ; "`baz`d"
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 14 19 14 15 15 18 18 19))))))

(ert-deftest test-markdown-parsing/code-at-point-blank-line ()
  "Test `markdown-code-at-point-p' at beginning of block."
  (markdown-test-string "----------\n\n## foo\n"
   (should-not (markdown-code-at-point-p))
   (forward-line)
   (should-not (markdown-code-at-point-p))
   (forward-line)
   (should-not (markdown-code-at-point-p))))

(ert-deftest test-markdown-parsing/match-comments ()
  "Test `markdown-match-comments'."
  (markdown-test-string
   "HTML <!-- foo --> comment"
   (should (markdown-match-comments (point-max)))
   (should (eq (point) 18))
   (should (equal (match-data) (list 6 18)))
   (should-not (markdown-match-comments (point-max)))))

(ert-deftest test-markdown-parsing/range-property-any ()
  "Test behavior of `markdown-range-property-any'."
  (markdown-test-file
   "inline.text"
   (should (markdown-range-property-any
            (point-min) (point-at-eol)
            'face (list markdown-markup-face
                        markdown-italic-face)))
   (should-not (markdown-range-property-any
            (point-min) (point-at-eol)
            'face (list markdown-bold-face)))))

(ert-deftest test-markdown-parsing/inline-code ()
  "Don't cause infinite loop for inline code just after metadata block
Detail: https://github.com/jrblevin/markdown-mode/issues/115"
  (markdown-test-string "---
x: x
---
`x`
"
    (should (= (markdown-match-code (point-max)) (point-max)))))

;;; Reference Checking:

(ert-deftest test-markdown-references/goto-line-button ()
  "Create and test a goto line button."
  (markdown-test-string "line 1\nline 2\n"
   ;; Store the temporary buffer with the text
   (let ((target (current-buffer)))
     ;; Create a new buffer for inserting
     (with-temp-buffer
       ;; Verify that point is in a different buffer
       (should (not (equal (current-buffer) target)))
       ;; Insert and press the button
       (insert-button "goto line 2"
                      :type 'markdown-goto-line-button
                      'target-buffer target
                      'target-line 2)
       (should (string-equal (buffer-string) "goto line 2"))
       (backward-button 1)
       (call-interactively 'push-button)
       ;; Verify that point is on line 2 of target buffer
       (should (= (line-number-at-pos) 2))
       (should (looking-at "line 2"))
       (should (equal (current-buffer) target))))))

(ert-deftest test-markdown-references/button-map ()
  "Verify that button-buffer-map is used for check references buffer."
  (markdown-test-string "[undefined][ref]\n"
   (let* ((target (buffer-name))
          (check (format "*Undefined references for %s*" target)))
   (markdown-check-refs)
   (with-current-buffer (get-buffer check)
     (should (equal (local-key-binding (kbd "TAB")) 'forward-button))
     (should (equal (local-key-binding (kbd "<backtab>")) 'backward-button))))))

;;; Lists:

(ert-deftest test-markdown-lists/levels-1 ()
  "Test list levels function `markdown-calculate-list-levels'."
  (markdown-test-file "nested-list.text"
   (let ((values '(((1 . 1) . nil) ((2 . 13) . (3)) ((14 . 23) . (7 3))
                   ((24 . 26) . (11 7 3)))))
     (cl-loop for (range . value) in values
           do (goto-char (point-min))
              (forward-line (1- (car range)))
              (dotimes (n (- (cdr range) (car range)))
                (should (equal (markdown-calculate-list-levels) value))
                (forward-line))))))

(ert-deftest test-markdown-lists/levels-2 ()
  "Test list levels function `markdown-calculate-list-levels'."
  (markdown-test-file "syntax.text"
   (let ((values '(((1 . 13) . nil) ((14 . 14) . (0)) ((15 . 17) . (4 0))
                   ((18 . 18) . (0)) ((19 . 24) . (4 0)) ((25 . 25) . (0))
                   ((26 . 29) . (4 0)) ((30 . 30) . (0)) ((31 . 33) . (4 0))
                   ((34 . 588) . nil) ((589 . 595) . (0)) ((596 . 814) . nil)
                   ((815 . 820) . (0)) ((821 . 898) . nil))))
     (cl-loop for (range . value) in values
           do (goto-char (point-min))
              (forward-line (1- (car range)))
              (dotimes (n (- (cdr range) (car range)))
                (should (equal (markdown-calculate-list-levels) value))
                (forward-line))))))

(ert-deftest test-markdown-lists/levels-interior ()
  "Test `markdown-calculate-list-levels' from inside a list item."
  (markdown-test-file "nested-list.text"
    (goto-char 36)
    (should (equal (markdown-calculate-list-levels) (list 3)))
    (goto-char 267)
    (should (equal (markdown-calculate-list-levels) (list 7 3)))
    (goto-char 540)
    (should (equal (markdown-calculate-list-levels) (list 11 7 3)))))

(ert-deftest test-markdown-lists/bounds-1 ()
  "Test list item bounds function `markdown-cur-list-item-bounds'."
  (markdown-test-file "lists.text"
    (markdown-test-goto-heading "Case 9")
    (forward-line)
    (should (eq (point) 3699))
    (markdown-next-list-item 4)
    (should (eq (point) 3700))
    (should (equal (markdown-cur-list-item-bounds)
                   (list 3700 3901 0 4 "-   ")))
    (markdown-next-list-item 4)
    (should (eq (point) 3903))
    (should (equal (markdown-cur-list-item-bounds)
                   (list 3903 3937 0 4 "*   ")))))

(ert-deftest test-markdown-lists/bounds-2 ()
  "Function `markdown-cur-list-item-bounds' should return nil outside of list items."
  (markdown-test-string "line one\n\n* item\n"
    (should (null (markdown-cur-list-item-bounds)))
    (forward-line)
    (should (null (markdown-cur-list-item-bounds)))
    (forward-line)
    (should (markdown-cur-list-item-bounds))))

(ert-deftest test-markdown-lists/promotion-and-demotion ()
  "Test function `markdown-promote-list-item'."
  (markdown-test-file "nested-list.text"
    (forward-line)
    (should (looking-at "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (markdown-demote-list-item)
    (should (looking-at "       - List level 1 item 2

         Second paragraph of item 2

            Nested pre block in item 2
            Four spaces past the marker

         Another paragraph of item 2"))
    (markdown-promote-list-item)
    (should (looking-at "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (goto-char (point-min))
    (forward-line 22)
    (should (looking-at "           - List level 3 item 1

                 Nested pre block"))
    (markdown-demote-list-item)
    (should (looking-at "               - List level 3 item 1

                     Nested pre block"))
    (markdown-promote-list-item)
    (should (looking-at "           - List level 3 item 1

                 Nested pre block"))))

(ert-deftest test-markdown-lists/promotion-and-demotion-custom ()
  "Test custom variable `markdown-list-indent-width'."
  (markdown-test-file "nested-list.text"
    (forward-line)
    (should (looking-at "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (let ((markdown-list-indent-width 2))
      (markdown-demote-list-item))
    (should (looking-at "     - List level 1 item 2

       Second paragraph of item 2

          Nested pre block in item 2
          Four spaces past the marker

       Another paragraph of item 2"))))

;;; Outline minor mode tests:

(ert-deftest test-markdown-outline/navigation ()
  "Test outline navigation functions."
  (markdown-test-file "outline.text"
   ;; Navigate to the first visible heading
   (markdown-next-visible-heading 1)
   (should (eq (point) 19))
   (should (looking-at "^# A top-level header"))
   ;; Navigate forward at the same level
   (markdown-forward-same-level 1)
   (should (eq (point) 377))
   (should (looking-at "^=+$"))
   ;; Navigate backward by four visible headings
   (markdown-previous-visible-heading 4)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))))

(ert-deftest test-markdown-outline/navigation-with-code ()
  "Test outline navigation functions with code blocks."
  (markdown-test-file "outline-code.text"
   ;; Navigate forward at the same level
   (markdown-forward-same-level 1)
   (should (eq (point) 159))
   (should (looking-at "^# Level one again"))))

(ert-deftest test-markdown-outline/visibility-atx ()
  "Test outline visibility cycling for ATX-style headers."
  (markdown-test-file "outline.text"
   (let (last-command this-command)
     ;; Navigate to the second visible heading
     (markdown-next-visible-heading 2)
     (should (eq (point) 69))
     (should (looking-at "^## A second-level header$"))
     ;; Cycle visibility of this subtree
     (setq this-command 'markdown-cycle)
     (markdown-cycle)
     (setq last-command 'markdown-cycle)
     (should (eq (point) 69))
     (should (looking-at "^## A second-level header$"))
     ;; Test that the entire subtree is invisible
     (markdown-test-range-has-property 93 349 'invisible 'outline)
     ;; Cycle visibility of this subtree again
     (markdown-cycle)
     (should (eq (point) 69))
     (should (looking-at "^## A second-level header$"))
     ;; Test that text is visible
     (markdown-test-range-has-property 95 121 'invisible nil)
     ;; Test that subheadings are visible
     (markdown-test-range-has-property 123 141 'invisible nil)
     ;; Cycle visibility of this subtree again
     (markdown-cycle)
     (should (eq (point) 69))
     (should (looking-at "^## A second-level header$"))
     ;; Verify that entire subtree is visible
     (markdown-test-range-has-property 93 349 'invisible nil))))

(ert-deftest test-markdown-outline/visibility-setext ()
  "Test outline visibility cycling for setext-style headers."
  (markdown-test-file "outline.text"
   ;; Navigate to the sixth visible heading
   (markdown-next-visible-heading 7)
   (markdown-previous-visible-heading 1)
   (should (looking-at markdown-regex-header))
   (should (string-equal (match-string-no-properties 1) "An underline-style header"))
   (should (string-equal (match-string-no-properties 2) "========================="))
   ;; Cycle visibility subtree, test that it's invisible
   (markdown-cycle)
   (markdown-test-range-has-property 404 515 'invisible 'outline)
   ;; Cycle visibility subtree, test that text and headers are visible
   (markdown-cycle)
   (markdown-test-range-has-property 404 417 'invisible nil)
   (markdown-test-range-has-property 420 451 'invisible nil)))

(ert-deftest test-markdown-outline/visibility-with-code ()
  "Test outline visibility cycling with code blocks."
  (markdown-test-file "outline-code.text"
   (let (last-command this-command)
     ;; Cycle global visibility to "overview" mode
     (setq this-command 'markdown-cycle)
     (markdown-cycle t)
     (setq last-command 'markdown-cycle)
     (should (eq (point) (point-min)))
     (should (looking-at "^# Level one"))
     ;; Test that the code block is invisible
     (markdown-test-range-has-property 83 157 'invisible 'outline)
     ;; Check subsequent headings
     (outline-next-visible-heading 1)
     (should (eq (point) 69))
     (should (looking-at "^## Level two"))
     (outline-next-visible-heading 1)
     (should (eq (point) 159))
     (should (looking-at "^# Level one again")))))

(ert-deftest test-markdown-outline/visibility-with-metadata ()
  "Test outline visibility cycling with metadata blocks."
  (markdown-test-string
   "---
layout = post
date = 2015-08-13 11:35:25 EST
---
"
   (let (last-command this-command)
     ;; Cycle global visibility to "overview" mode
     (setq this-command 'markdown-cycle)
     (markdown-cycle t)
     ;; Check that text is visible
     (markdown-test-range-has-property (point-min) (point-max) 'invisible nil))))

;;; Movement tests:

(ert-deftest test-markdown-movement/defun ()
  "Test defun navigation."
  (markdown-test-file "outline.text"
   ;; end-of-defun should go to point-max
   (end-of-defun 10)
   (should (= (point) (point-max)))
   ;; end-of-defun should stop just before the next header
   (goto-char (point-min))
   (end-of-defun)
   (should (looking-at "\n# A top-level header"))
   (end-of-defun)
   (should (looking-at "\n## A second-level header"))
   (end-of-defun)
   (should (looking-at "\n### Third level ###"))
   (end-of-defun)
   (should (looking-at "\n### Third level number two ###"))
   ;; beginning-of-defun should move to the start of the previous header
   (beginning-of-defun)
   (should (looking-at "### Third level ###"))
   (beginning-of-defun)
   (should (looking-at "## A second-level header"))
   (beginning-of-defun)
   (should (looking-at "# A top-level header"))
   (beginning-of-defun)
   ;; beginning-of-defun should move up to point-min
   (should (= (point) (point-min)))
   ;; (beginning-of-defun -1)  should move to the start of the next header
   (forward-line 2)
   (beginning-of-defun -1)
   (should (looking-at "## A second-level header"))
   (beginning-of-defun -1)
   (should (looking-at "### Third level ###"))
   (beginning-of-defun -1)
   (should (looking-at "### Third level number two ###"))))

(ert-deftest test-markdown-movement/block ()
  "Test block movement."
  (markdown-test-file "outline.text"
   (markdown-end-of-block)
   (should (looking-at "\n# A top-level header"))
   (markdown-end-of-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-end-of-block)
   (should (looking-at "\n## A second-level header"))
   (markdown-end-of-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-end-of-block)
   (should (looking-at "\n### Third level ###"))
   (markdown-end-of-block)
   (should (looking-at "\n\\* A list item"))
   (markdown-end-of-block)
   (should (looking-at "\n### Third level number two ###"))
   (markdown-end-of-block)
   (should (looking-at "\n### Level two again"))
   (markdown-end-of-block)
   (should (looking-at "\nfollowed by some body text"))

   (markdown-test-goto-heading "Level two")
   (markdown-end-of-block)
   (should (looking-at "\nbar"))
   (markdown-end-of-block)
   (should (= (point) (point-max)))
   (markdown-beginning-of-block)
   (should (looking-at "bar"))
   (markdown-beginning-of-block)
   (should (looking-at "## Level two"))
   (markdown-beginning-of-block)
   (should (looking-at "foo"))
   (markdown-beginning-of-block)
   (should (looking-at "# Level one"))
   (markdown-beginning-of-block)
   (should (looking-at "* With"))
   (markdown-beginning-of-block)
   (should (looking-at "And a level two underline header"))

   (goto-char (point-min))
   (markdown-test-goto-heading "A top-level header")
   (beginning-of-line)
   (markdown-beginning-of-block)
   (should (= (point) (point-min)))))

(ert-deftest test-markdown-movement/reference-definition ()
  "Test jumping to reference definitions."
  ;; Jumping to explicit reference definition
  (markdown-test-string "[a][ref]\n\n[ref]: gopher://localhost/\n"
   (markdown-reference-goto-definition)
   (should (= (point) 18)))
  ;; Jumping to implicit reference definition
  (markdown-test-string "[a][]\n\n[a]: ftp://localhost/\n"
   (markdown-reference-goto-definition)
   (should (= (point) 13)))
  ;; Creating non-existent reference definition
  (markdown-test-string "[a][]\n"
   (markdown-reference-goto-definition)
   (should (= (point) 13))
   (should (string-equal (buffer-string) "[a][]\n\n[a]: \n"))))

(ert-deftest test-markdown-movement/back-to-same-level-over-code-block ()
  "`markdown-backward-same-level' over code block which contains header
like statement. Detail: https://github.com/jrblevin/markdown-mode/issues/75"
  (markdown-test-string "
## Header 2-1

## Header 2-2

```R
# Header Like Statement
```

## Header 2-3
"
    (search-forward "## Header 2-3")
    (let ((last-header-pos (point)))
      (forward-line -1)
      (call-interactively #'markdown-backward-same-level)
      (should (looking-at-p "## Header 2-1"))

      (goto-char last-header-pos)
      (call-interactively #'markdown-backward-same-level)
      (should (looking-at-p "## Header 2-2"))

      (goto-char last-header-pos)
      (markdown-backward-same-level 2)
      (should (looking-at-p "## Header 2-1"))

      (search-forward "# Header Like Statement")
      (call-interactively #'markdown-backward-same-level)
      (should (looking-at-p "## Header 2-1")))))

;;; Link tests:

(ert-deftest test-markdown-link/follow ()
  "Test link following in a browser and in Emacs."
  (markdown-test-string "[text](http://path?query=foo#id)"
    (let* ((opened-url nil)
           (browse-url-browser-function
            (lambda (url &rest args) (setq opened-url url))))
      (markdown-follow-thing-at-point nil)
      (should (equal opened-url "http://path?query=foo#id"))))
  (when (featurep 'url-parse)
    (markdown-test-string "[text](path?query=foo#id)"
      (markdown-follow-thing-at-point nil)
      (should (equal (file-name-nondirectory (buffer-file-name)) "path"))
      (kill-buffer))))

;;; Wiki link tests:

(ert-deftest test-markdown-wiki-link/file-local-variables ()
  "Test enabling wiki links via file-local variables."
  (markdown-test-file "wiki-links.text"
   (should-not markdown-enable-wiki-links)
   (hack-local-variables)
   (should markdown-enable-wiki-links)))

(ert-deftest test-markdown-wiki-link/aliasing ()
  "Test filename extraction for aliased wiki links."
  (let ((markdown-enable-wiki-links t))
    (markdown-test-file "wiki-links.text"
      ;; Confirm location of first wiki link
      (should (eq (markdown-next-link) 8))
      ;; Confirm location of second wiki link
      (should (eq (markdown-next-link) 73))
      ;; Test predicate function
      (should (markdown-wiki-link-p))
      ;; Test alias-first filename extraction
      (setq markdown-wiki-link-alias-first t)
      (should (string-equal (markdown-wiki-link-link) "second"))
      ;; Test alias-second filename extraction
      (setq markdown-wiki-link-alias-first nil)
      (should (string-equal (markdown-wiki-link-link) "first")))))

(ert-deftest test-markdown-wiki-link/navigation ()
  "Test wiki link navigation."
  (let ((markdown-enable-wiki-links t))
    (markdown-test-file "wiki-links.text"
      ;; Advance to first link
      (should (eq (markdown-next-link) 8))
      ;; Advance to second link
      (should (eq (markdown-next-link) 73))
      ;; Avance to final link
      (should (eq (markdown-next-link) 155))
      ;; Return nil and don't advance point
      (should (eq (markdown-next-link) nil))
      (should (eq (point) 155))
      ;; Move back to second link
      (should (eq (markdown-previous-link) 73))
      ;; Move back to first link
      (should (eq (markdown-previous-link) 8))
      ;; Return nil and don't move point
      (should (eq (markdown-previous-link) nil))
      (should (eq (point) 8)))))

(ert-deftest test-markdown-wiki-link/font-lock ()
  "Test font lock faces for wiki links."
  (markdown-test-temp-file "wiki-links.text"
   (let* ((fn (concat (file-name-directory buffer-file-name)
                     "inline.text"))
          (markdown-enable-wiki-links t))
     ;; Create inline.text in the same temp directory, refontify
     (write-region "" nil fn nil 1)
     (markdown-fontify-buffer-wiki-links)
     ;; Confirm location of first wiki link
     (should (eq (markdown-next-link) 8))
     ;; First wiki link doesn't have a corresponding file
     (markdown-test-range-has-property 8 20 'font-lock-face markdown-missing-link-face)
     ;; Second wiki link doesn't have a corresponding file
     (should (eq (markdown-next-link) 73))
     (markdown-test-range-has-property 73 88 'font-lock-face markdown-missing-link-face)
     ;; Move to third wiki link, and create the missing file
     (should (eq (markdown-next-link) 155))
     (should (string-equal (markdown-wiki-link-link) "inline"))
     (markdown-test-range-has-property 155 164 'font-lock-face markdown-link-face)
     ;; Check wiki links in code blocks
     (markdown-test-range-has-face 360 395 markdown-pre-face)
     ;; Remove temporary files
     (delete-file fn)
     )))

(ert-deftest test-markdown-wiki-link/kill ()
  "Simple tests for `markdown-kill-thing-at-point' for wiki links."
  (let ((kill-ring nil)
        (markdown-enable-wiki-links t)
        (tests (list '("[[foo]]" . "foo")
                     '("[[foo|bar]]" . "bar"))))
    (dolist (test tests)
      ;; Load test string (the car), move to end of first line, kill
      ;; thing at point, and then verify that the kill ring contains cdr.
      (markdown-test-string (car test)
                            (end-of-line)
                            (call-interactively 'markdown-kill-thing-at-point)
                            (should (string-equal (current-kill 0) (cdr test)))))))

;;; Filling tests:

(ert-deftest test-markdown-filling/blockquote ()
  "Test filling of blockquotes.
See `adaptive-fill-first-line-regexp'."
  (markdown-test-string "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n> eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/space-after-list-marker ()
  "`fill-paragraph' should preserve more than one space after a list marker,
since users may wish to indent their lists more than one space more than the
width of the marker. The examples on the Markdown Syntax page have three
spaces after the list marker for a total indentation of four."
  (let ((str "\n\n*   List item indented four spaces.\n*   Also four spaces."))
   (markdown-test-string str
    (forward-line 2)
    (fill-paragraph)
    (should (string-equal (buffer-string) str)))))

(ert-deftest test-markdown-filling/multi-line-list-with-more-space ()
  "`fill-paragraph' should preserve more than one space after a list marker
(see `test-preserve-space-after-list-marker')."
  (let ((str "*   This list item is continued on\n    the next line"))
   (markdown-test-string str
    ;; The first line is exactly 35 columns
    (let ((fill-column 35))
      (fill-paragraph)
      (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/list-item-plus ()
  "Test filling of list items with plus sign markers.
See `adaptive-fill-regexp'."
  (markdown-test-string "  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) "  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n    eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/list-item-plus-in-blockquote ()
  "Test filling of list items with plus sign markers inside blockquote.
See `adaptive-fill-regexp'."
  (markdown-test-string ">  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) ">  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n>    eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/line-break ()
  "Test filling of paragraphs with hard line breaks.
See `paragraph-separate'."
  (markdown-test-string "Lorem ipsum dolor sit amet,  \nconsectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (let ((fill-column 70))
     (fill-paragraph)
     (should (string-equal (buffer-string) "Lorem ipsum dolor sit amet,  \nconsectetur adipisicing elit, sed do eiusmod tempor incididunt ut\nlabore et dolore magna aliqua.")))))

(ert-deftest test-markdown-filling/decimal-number-at-beginning ()
  "Test filling when a number with a decimal appears at the beginning of a line."
  (markdown-test-string "The circumference of a circle divided by it's radius is around\n3.14."
   (fill-paragraph)
   (should (string-equal (buffer-string) "The circumference of a circle divided by it's radius is around 3.14."))))

(ert-deftest test-markdown-filling/avoid-unintended-list-item ()
  "Avoid breaking lines where it would result in an unintended list item."
  (markdown-test-string "Lorem ipsum dolor sit 4. amet"
   (let ((fill-column 22))
     (fill-paragraph)
     (should (string-equal (buffer-string) "Lorem ipsum dolor\nsit 4. amet")))))

(ert-deftest test-markdown-filling/no-break-link-reference ()
  "Shouldn't break line between label and url, or combine two link references."
  (let ((str "[label1]: http://long-url.example.com\n[label2]: http://another-long-url.example.com/"))
    (markdown-test-string str
     (let ((fill-column 15)) ; after end of label, before end of URL
       (fill-paragraph)
       (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/no-break-before-list-item ()
  "There's no point in putting the first item of a list on the next line,
indented the same amount."
  :expected-result :failed
  (let ((str "*   [Link](http://way-too-long.example.com)\n"))
    (markdown-test-string str
      (auto-fill-mode 1)
      (let ((fill-column 10))
        (end-of-line)
        (funcall auto-fill-function)
        (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/break-within-list-item ()
  "This doesn't suppress auto-fill within a multi-word list item."
  :expected-result :failed
  (markdown-test-string "*   [Link](http://example.com/) more text"
    (auto-fill-mode 1)
    (let ((fill-column 10))
      (end-of-line)
      (funcall auto-fill-function)
      (should (string-equal
               (buffer-string)
               "*   [Link](http://example.com/)\n    more text")))))

(ert-deftest test-markdown-filling/preserve-next-line-footnote ()
  "Footnote block can be after label"
  (let ((str "[^label1]:\n    Footnote block\n    more footnote")) ; six spaces
    (markdown-test-string str
     (let ((fill-column 20)) ; could fit "footnote" after label, but shouldn't
       (fill-paragraph)
       (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/wrap-same-line-footnote ()
  "Additional lines must be indented one level (four spaces) when wrapped."
  (markdown-test-string "[^label]: Long line should be wrapped"
     (let ((fill-column 25)) ; wrap before end of "should"
       (fill-paragraph)
       (should (string-equal (buffer-string) "[^label]: Long line\n    should be wrapped")))))

(ert-deftest test-markdown-filling/wrap-extra-hanging-indentation ()
  "Additional lines must be indented one level (four spaces) when wrapped."
  (markdown-test-string "[^label]: Long line\n      should be wrapped"
     (let ((fill-column 25)) ; wrap before end of "should"
       (fill-paragraph)
       (should (string-equal (buffer-string) "[^label]: Long line\n      should be wrapped")))))

(ert-deftest test-markdown-filling/full-justification ()
  "Test paragraph detection with lines with lots of whitespace."
  (markdown-test-string "Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Dolor Sit Amet Consectetur http://very-long-url.lorem.ipsum.sic.dolor.sit.amet.com"
     (setq default-justification 'full)
     (fill-paragraph)
     (should (string-equal (buffer-string) "Lorem  Ipsum Lorem  Ipsum Lorem  Ipsum Lorem  Ipsum Lorem  Ipsum Lorem\nDolor                Sit               Amet                Consectetur\nhttp://very-long-url.lorem.ipsum.sic.dolor.sit.amet.com"))
     (backward-paragraph)
     (forward-paragraph)
     (should (= (point) 198))))

(ert-deftest test-markdown-filling/list-line ()
  "Test fill-paragraph for list line. Don't insert bullet automatically.
Detail: https://github.com/jrblevin/markdown-mode/issues/79"
  (markdown-test-string "* foo foo *foo* foo foo foo foo foo foo"
    (let ((fill-column 10))
      (fill-paragraph)
      (fill-paragraph)
      (forward-line 2)
      (back-to-indentation)
      (should-not (looking-at-p "\\*foo"))
      (forward-line 1)
      (back-to-indentation)
      (should-not (looking-at-p "\\*foo")))))

(ert-deftest test-markdown-filling/ignore-header ()
  "# Test fill-paragraph for containing header line paragraph.
https://github.com/jrblevin/markdown-mode/issues/159"
  (markdown-test-string "# this is header line
this is not header line
"
    (let ((fill-column 10))
      (fill-paragraph)
      (should (string= (buffer-substring (point) (line-end-position)) "# this is header line")))))

(ert-deftest test-markdown-filling/unclosed-square-bracket ()
  "Test fill-paragraph following an unclosed square bracket."
  (markdown-test-string "```\n[3\n```\n\naaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbb"
    (let ((fill-column 20))
      (forward-line 4)
      (fill-paragraph)
      (should (looking-at "aaaaaaaaaaaaaaaa\nbbbbbbbbbbbbbbbb")))))

;;; Export tests:

(ert-deftest test-markdown-hook/xhtml-standalone ()
  "Test `markdown-xhtml-standalone-regexp' and `markdown-output-standalone-p'."
  (should (string-match markdown-xhtml-standalone-regexp
                        "<?xml version='1.0' encoding='UTF-8'?>"))
  (should (string-match markdown-xhtml-standalone-regexp
                        "<!DOCTYPE html>"))
  (should (string-match markdown-xhtml-standalone-regexp
                        "<html>"))
  (should-not (string-match markdown-xhtml-standalone-regexp
                            "<h1>title</h1>"))
  (should-not (string-match markdown-xhtml-standalone-regexp
                            "<div id=\"name\">")))

;;; Hook tests:

(ert-deftest test-markdown-hook/before-export ()
  "Test hook run before export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((before-hook-run nil)
          (orig-point (point))
          (func (lambda ()
                  ;; Change value of a variable
                  (setq before-hook-run t)
                  ;; Insert some text
                  (goto-char (point-min))
                  (insert "#")
                  ;; Deliberately move the point
                  (end-of-line)
                  ;; Verify changes
                  (should (looking-back "^## List Cases" nil))
                  (should-not (= (point) orig-point))))
          (ofile (progn
                   ;; Register hook
                   (add-hook 'markdown-before-export-hook func)
                   ;; Export XHTML and return filename
                   (markdown-export)))
          (obuffer (get-file-buffer ofile)))
     ;; Test side effects of hook
     (should (eq before-hook-run t))
     ;; Test position of point
     (should (= (point) orig-point))
     ;; Test that buffer was restored to original state
     (goto-char (point-min))
     (should (looking-at "^# List Cases"))
     ;; Clean
     (remove-hook 'markdown-before-export-hook func)
     (kill-buffer obuffer)
     (delete-file ofile))))

(ert-deftest test-markdown-hook/after-export ()
  "Test hook run after export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((after-hook-run nil)
          (func (lambda ()
                  ;; Change variable value
                  (setq after-hook-run t)
                  ;; Add comment to output buffer
                  (goto-char (point-min))
                  (insert "<!-- after-export-hook -->\n")))
          (ofile (progn
                   ;; Register hook
                   (add-hook 'markdown-after-export-hook func)
                   ;; Export XHTML and return filename
                   (markdown-export)))
          (obuffer (get-file-buffer ofile)))
     (message "obuffer = %S" obuffer)
     ;; Test that variable was changed
     (should (eq after-hook-run t))
     ;; Test that output buffer remains open
     (should (get-buffer obuffer))
     ;; Test that output buffer modification remains
     (with-current-buffer obuffer
       (goto-char (point-min))
       (should (looking-at "<!-- after-export-hook -->\n")))
     ;; Test that buffer modification was saved
     (should-not (buffer-modified-p obuffer))
     ;; Clean up
     (remove-hook 'markdown-after-export-hook func)
     (kill-buffer obuffer)
     (delete-file ofile))))

;;; Extension: math support

(ert-deftest test-markdown-math/file-local-variable ()
  "Test enabling math mode via `hack-local-variables-hook'."
  (markdown-test-file "math.text"
   (should-not markdown-enable-math)
   (hack-local-variables)
   (should markdown-enable-math)))

(ert-deftest test-markdown-math/reload ()
  "Test enabling math mode via function `markdown-enable-math'."
  (markdown-test-file "math.text"
    (markdown-toggle-math t)
    ;; Flag should be set to t
    (should markdown-enable-math)
    ;; Font-lock keywords should be updated
    (should (member (cons markdown-regex-math-display '((1 markdown-markup-face prepend)
                                                        (2 markdown-math-face append)
                                                        (3 markdown-markup-face prepend)))
                    markdown-mode-font-lock-keywords))))

(ert-deftest test-markdown-math/font-lock ()
  "Test markdown math mode."
  (markdown-test-file "math.text"
   (markdown-toggle-math t)
   (funcall markdown-test-font-lock-function)
   (markdown-test-range-has-face 1 32 nil)
   (markdown-test-range-has-face 33 33 markdown-markup-face)
   (markdown-test-range-has-face 34 45 markdown-math-face)
   (markdown-test-range-has-face 46 46 markdown-markup-face)
   (markdown-test-range-has-face 47 49 nil)
   (markdown-test-range-has-face 50 51 markdown-markup-face)
   (markdown-test-range-has-face 52 63 markdown-math-face)
   (markdown-test-range-has-face 64 65 markdown-markup-face)
   (markdown-test-range-has-face 66 98 nil)
   (markdown-test-range-has-face 99 100 markdown-markup-face)
   (markdown-test-range-has-face 101 112 markdown-math-face)
   (markdown-test-range-has-face 113 114 markdown-markup-face)
   (markdown-test-range-has-face 113 114 markdown-markup-face)
   (markdown-test-range-has-face 117 117 markdown-header-delimiter-face)
   (markdown-test-range-has-face 119 152 markdown-header-face-1)
   (markdown-test-range-has-face 129 129 markdown-markup-face)
   (markdown-test-range-has-face 136 136 markdown-markup-face)
   (markdown-test-range-has-face 174 177 markdown-markup-face)
   (markdown-test-range-has-face 179 188 markdown-language-keyword-face)
   (markdown-test-range-has-face 190 211 markdown-pre-face)
   (markdown-test-range-has-face 212 215 markdown-markup-face)
   (markdown-test-range-has-face 218 218 markdown-markup-face)
   (markdown-test-range-has-face 219 223 markdown-math-face)
   (markdown-test-range-has-face 224 224 markdown-markup-face)))

(ert-deftest test-markdown-math/font-lock-italics ()
  "Test markdown math mode with underscores."
  (markdown-test-file "math.text"
   (markdown-toggle-math t)
   (funcall markdown-test-font-lock-function)
   (markdown-test-range-has-face 227 227 markdown-markup-face)
   (markdown-test-range-has-face 228 233 markdown-math-face)
   (markdown-test-range-has-face 234 234 markdown-markup-face)
   (markdown-test-range-has-face 235 270 nil)
   (markdown-test-range-has-face 271 271 markdown-markup-face)
   (markdown-test-range-has-face 272 274 markdown-math-face)
   (markdown-test-range-has-face 275 275 markdown-markup-face)))

;;; gfm-mode tests:

(ert-deftest test-markdown-gfm/pre-1 ()
  "GFM pre block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2626 2637 nil)
    (markdown-test-range-has-face 2639 2641 markdown-markup-face)
    (markdown-test-range-has-face 2642 2645 markdown-language-keyword-face)
    (markdown-test-range-has-face 2647 2728 markdown-pre-face)
    (markdown-test-range-has-face 2730 2732 markdown-markup-face)))

(ert-deftest test-markdown-gfm/italic-1 ()
  "GFM italic font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 1483 1483 markdown-markup-face)
    (markdown-test-range-has-face 1484 1487 markdown-italic-face)
    (markdown-test-range-has-face 1488 1488 markdown-markup-face)
    (markdown-test-range-has-face 1729 1790 nil)))

(ert-deftest test-markdown-gfm/strike-through-1 ()
  "GFM strike through font lock test."
  (markdown-test-string-gfm "one ~~two~~ three"
    (markdown-test-range-has-face 1 4 nil)
    (markdown-test-range-has-face 5 6 markdown-markup-face)
    (markdown-test-range-has-face 7 9 markdown-strike-through-face)
    (markdown-test-range-has-face 10 11 markdown-markup-face)
    (markdown-test-range-has-face 12 17 nil)))

(ert-deftest test-markdown-gfm/toggle-strike-through ()
  "Test toggling functionality of `markdown-insert-strike-through'."
  (markdown-test-string-gfm "one ~~two~~ three"
   (forward-word 2)
   (markdown-insert-strike-through)
   (should (string-equal (buffer-string) "one two three"))
   (should (= (point) 8))
   (forward-word)
   (markdown-insert-strike-through)
   (should (= (point) 16))
   (should (string-equal (buffer-string) "one two ~~three~~"))))

(ert-deftest test-markdown-gfm/insert-code-block ()
  "GFM code block insertion test."
  ;; Test empty markup insertion
  (markdown-test-string-gfm "line 1\nline 2\n"
   (end-of-line)
   (markdown-insert-gfm-code-block "elisp")
   (should (equal (car markdown-gfm-used-languages) "elisp"))
   (should (equal (car (markdown-gfm-get-corpus)) "elisp"))
   (should (string-equal (buffer-string)
                         "line 1\n\n``` elisp\n\n```\n\nline 2\n")))
  ;; Test with active region
  (markdown-test-string-gfm "line 1\nline 2\nline 3\n"
   (forward-line)
   (transient-mark-mode)
   (push-mark (point) t t)
   (end-of-line)
   (should (markdown-use-region-p))
   (markdown-insert-gfm-code-block "elisp")
   (should (string-equal (buffer-string)
                         "line 1\n\n``` elisp\nline 2\n```\n\nline 3\n"))))

(ert-deftest test-markdown-gfm/gfm-parse-buffer-for-languages ()
  "Parse buffer for existing languages for `markdown-gfm-used-languages' test."
  (markdown-test-string-gfm "``` MADEUP\n\n```\n``` LANGUAGES\n\n```\n```MaDeUp\n\n```\n```\n\n```\n``` \n\n```\n"
    (markdown-gfm-parse-buffer-for-languages)
    (should (equal markdown-gfm-used-languages
                   (list "MaDeUp" "LANGUAGES" "MADEUP")))
    (should (equal (car markdown-gfm-used-languages) "MaDeUp"))
    (should (equal (car (markdown-gfm-get-corpus)) "MaDeUp"))
    (goto-char (point-max))
    (markdown-insert-gfm-code-block "newlang")
    (should (equal markdown-gfm-used-languages
                   (list "newlang" "MaDeUp" "LANGUAGES" "MADEUP")))
    (should (equal (car markdown-gfm-used-languages) "newlang"))
    (should (equal (car (markdown-gfm-get-corpus)) "newlang"))
    (let ((markdown-gfm-downcase-languages nil))
      (should
       (equal (markdown-gfm-get-corpus)
              (append markdown-gfm-used-languages
                      markdown-gfm-additional-languages
                      markdown-gfm-recognized-languages))))
    (let ((markdown-gfm-downcase-languages t))
      (should
       (equal
        (markdown-gfm-get-corpus)
        (append markdown-gfm-used-languages
                (cl-mapcar #'downcase
                           (append markdown-gfm-additional-languages
                                   markdown-gfm-recognized-languages))))))))

(ert-deftest test-markdown-gfm/code-block-font-lock ()
  "GFM code block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2639 2641 markdown-markup-face) ; ```
    (markdown-test-range-has-face 2642 2645 markdown-language-keyword-face) ; lang
    (markdown-test-range-has-face 2647 2728 markdown-pre-face) ; code
    (markdown-test-range-has-face 2730 2732 markdown-markup-face))) ; ```

(ert-deftest test-markdown-gfm/code-block-font-lock-2 ()
  "GFM code block font lock test without language identifier."
  (markdown-test-string-gfm "Plain code block:\n\n```\nfoo\n```\n"
    (markdown-test-range-has-face 20 22 markdown-markup-face)
    (markdown-test-range-has-face 24 26 markdown-pre-face)
    (markdown-test-range-has-face 28 30 markdown-markup-face)))

;;; Tests for other extensions:

(ert-deftest test-markdown-ext/pandoc-fancy-lists ()
  "Test basic support for font lock and filling of Pandoc 'fancy lists'."
  (markdown-test-string " #. abc\ndef\n"
    ;; font lock
    (markdown-test-range-has-face 1 1 nil)
    (markdown-test-range-has-face 2 3 markdown-list-face)
    (markdown-test-range-has-face 4 11 nil)
    ;; filling
    (forward-line)
    (markdown-indent-region (line-beginning-position) (line-end-position) nil)
    (should (string-equal (buffer-string) " #. abc\n def\n"))
    (markdown-indent-region (line-beginning-position) (line-end-position) nil)
    (should (string-equal (buffer-string) " #. abc\n    def\n"))))

(ert-deftest test-markdown-ext/ikiwiki ()
  (let ((markdown-enable-wiki-links t)
        (markdown-wiki-link-fontify-missing t)
        (markdown-wiki-link-search-parent-directories t))
    (progn
      (find-file "ikiwiki/root")
      (unwind-protect
          (progn
            (markdown-mode)
            ;; font lock
            (markdown-test-range-has-property 1 11 'font-lock-face markdown-link-face)
            (markdown-test-range-has-property 14 33 'font-lock-face markdown-missing-link-face))
        (kill-buffer)))
    (progn
      (find-file "ikiwiki/sub/foo")
      (unwind-protect
          (progn
            (markdown-mode)
            ;; font lock
            (markdown-test-range-has-property 1 16 'font-lock-face markdown-missing-link-face)
            (markdown-test-range-has-property 19 26 'font-lock-face markdown-link-face))
        (kill-buffer)))))

(defadvice markdown-live-preview-window-eww
    (around markdown-test-create-fake-eww disable)
  (setq ad-return-value (get-buffer-create "*eww*")))

(defmacro markdown-test-fake-eww (&rest body)
  `(progn
     ,@(if (and (fboundp 'libxml-parse-html-region) (require 'eww nil t)) body
         `((ad-enable-advice #'markdown-live-preview-window-eww
                             'around 'markdown-test-create-fake-eww)
           (ad-activate #'markdown-live-preview-window-eww)
           ,@body
           (ad-disable-advice #'markdown-live-preview-window-eww
                              'around 'markdown-test-create-fake-eww)
           (ad-activate #'markdown-live-preview-window-eww)))))

(defmacro markdown-test-eww-or-nothing (test &rest body)
  (if (and (fboundp 'libxml-parse-html-region) (require 'eww nil t)
           (executable-find markdown-command))
      `(progn ,@body)
    (message "no eww, no libxml2, or no %s found: skipping %s" markdown-command test)
    nil))

(ert-deftest test-markdown-ext/live-preview-exports ()
  (markdown-test-temp-file "inline.text"
    (unless (and (fboundp 'libxml-parse-html-region) (require 'eww nil t))
      (should-error (markdown-live-preview-mode)))
    (markdown-test-fake-eww
     (markdown-live-preview-mode)
     (should (buffer-live-p markdown-live-preview-buffer))
     (should (eq (current-buffer)
                 (with-current-buffer markdown-live-preview-buffer
                   markdown-live-preview-source-buffer)))
     (kill-buffer markdown-live-preview-buffer)
     (should (null markdown-live-preview-buffer))
     (set-buffer-modified-p t)
     (save-buffer)                      ; should create new export
     (should (buffer-live-p markdown-live-preview-buffer)))))

(ert-deftest test-markdown-ext/live-preview-delete-exports ()
  (markdown-test-fake-eww
   (let ((markdown-live-preview-delete-export 'delete-on-destroy)
         file-output)
     (markdown-test-temp-file "inline.text"
       (markdown-live-preview-mode)
       (setq file-output (markdown-export-file-name)))
     (should-not (file-exists-p file-output)))
   (let ((markdown-live-preview-delete-export 'delete-on-export)
         file-output)
     (markdown-test-temp-file "inline.text"
       (markdown-live-preview-mode)
       (setq file-output (markdown-export-file-name))
       (should-not (file-exists-p file-output))))
   (let ((markdown-live-preview-delete-export nil)
         file-output)
     (unwind-protect
         (markdown-test-temp-file "inline.text"
           (markdown-live-preview-mode)
           (setq file-output (markdown-export-file-name))
           (should (file-exists-p file-output)))
       (delete-file file-output)))))

(ert-deftest test-markdown-ext/live-preview-follow-min-max ()
  (markdown-test-eww-or-nothing "live-preview-follow-min-max"
   (markdown-test-temp-file "inline.text"
     (markdown-live-preview-mode)
     (should (buffer-live-p markdown-live-preview-buffer))
     (should (window-live-p (get-buffer-window markdown-live-preview-buffer)))
     (with-selected-window (get-buffer-window markdown-live-preview-buffer)
       (goto-char (point-min)))
     (goto-char (point-min))
     (insert "a test ")
     (markdown-live-preview-export)
     (let (final-pt final-win-st-diff)
       ;; test that still starts at point-min
       (with-selected-window (get-buffer-window markdown-live-preview-buffer)
         (should (= (window-point) 1))
         (should (= (markdown-visual-lines-between-points
                     (window-start) (window-point))
                    0))
         (set-window-point (selected-window) (point-max))
         (setq final-pt (window-point)
               final-win-st-diff (markdown-visual-lines-between-points
                                  (window-start) (window-point))))
       (goto-char (point-min))
       (insert "this is ")
       (markdown-live-preview-export)
       (with-selected-window (get-buffer-window markdown-live-preview-buffer)
         (should (= (window-point) (+ final-pt (length "this is "))))
         (should (= (markdown-visual-lines-between-points
                     (window-start) (window-point))
                    final-win-st-diff))
         ;; test that still starts at point-max, with correct line difference
         (goto-char (floor (/ (float (- (point-max) (point-min))) 2)))
         (setq final-pt (window-point)
               final-win-st-diff (markdown-visual-lines-between-points
                                  (window-start) final-pt)))
       (markdown-live-preview-export)
       ;; test that still starts at same point, with correct line difference
       (with-selected-window (get-buffer-window markdown-live-preview-buffer)
         (should (= (window-point) final-pt))
         (should (= (markdown-visual-lines-between-points
                     (window-start) (window-point))
                    final-win-st-diff)))))))

;; Tests for imenu

(ert-deftest test-markdown-imenu/metadata ()
  "Don't correct header like statement in metadata.
https://github.com/jrblevin/markdown-mode/issues/145"
  (markdown-test-string "---
title = \"Blah\"
comments = false
---

# Header1

## Header2
"
    (let ((headers (mapcar #'car (markdown-imenu-create-flat-index))))
      (should (member "Header1" headers))
      (should (member "Header2" headers))
      (should-not (member "comments = false" headers)))))

(provide 'markdown-test)

;;; markdown-test.el ends here
