;;; ein-multilang.el --- Notebook mode with multiple language fontification

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-multilang.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-multilang.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-multilang.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (defvar markdown-mode-map))

(require 'ein-worksheet)
(require 'ein-multilang-fontify)
(require 'python)
(require 'ess-r-mode nil t)
(require 'ess-custom nil t)

(declare-function ess-indent-line "ess")
(declare-function ess-r-eldoc-function "ess-r-completion")
(declare-function ess-setq-vars-local "ess-utils")

(defun ein:ml-fontify (limit)
  "Fontify next input area comes after the current point then
return `t' or `nil' if not found.
See info node `(elisp) Search-based Fontification'."
  (ein:log-ignore-errors
    (ein:ml-fontify-1 limit)))

(defun ein:ml-current-or-next-input-cell (ewoc-node)
  "Almost identical to `ein:worksheet-next-input-cell' but return
the current cell if EWOC-NODE is the input area node."
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ein:$node-data ewoc-data))
         (path (ein:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element '(prompt input))
        cell
      (ein:cell-next cell))))

(defun ein:ml-fontify-1 (limit)
  "Actual implementation of `ein:ml-fontify'.
This function may raise an error."
  (ein:and-let* ((pos (point))
                 (node (ein:worksheet-get-nearest-cell-ewoc-node pos limit))
                 (cell (ein:ml-current-or-next-input-cell node))
                 (start (ein:cell-input-pos-min cell))
                 (end   (ein:cell-input-pos-max cell))
                 ((<= end limit))
                 ((< start end))
                 (lang (ein:cell-language cell)))
    (let ((inhibit-read-only t))
      (ein:mlf-font-lock-fontify-block lang start end)
      ;; Emacs fontification mechanism requires the function to move
      ;; the point.  Do *not* use `(goto-char end)'.  As END is in the
      ;; input area, fontification falls into an infinite loop.
      (ewoc-goto-node (slot-value cell 'ewoc) (ein:cell-element-get cell :footer)))
    t))

(defun ein:ml-back-to-prev-node ()
  (ein:aand (ein:worksheet-get-ewoc) (ewoc-goto-prev it 1)))

(defvar ein:ml-font-lock-keywords
  '((ein:ml-fontify))
  "Default `font-lock-keywords' for `ein:notebook-multilang-mode'.")

(defun ein:ml-set-font-lock-defaults ()
  (setq-local font-lock-defaults
       '(ein:ml-font-lock-keywords
         ;; The following are adapted from org-mode but I am not sure
         ;; if I need them:
         t nil nil
         ein:ml-back-to-prev-node)))

;;;###autoload
(define-derived-mode ein:notebook-multilang-mode prog-mode "EIN"
  "A mode for fontifying multiple languages.

\\{ein:notebook-multilang-mode-map}
"
  (setq-local beginning-of-defun-function
       'ein:worksheet-beginning-of-cell-input)
  (setq-local end-of-defun-function
       'ein:worksheet-end-of-cell-input)
  (ein:ml-set-font-lock-defaults))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'ein:notebook-multilang-mode))


;;; Language setup functions

(defun ein:ml-narrow-to-cell ()
  "Narrow to the current cell."
  (ein:and-let* ((pos (point))
                 (node (ein:worksheet-get-nearest-cell-ewoc-node pos))
                 (cell (ein:ml-current-or-next-input-cell node))
                 (start (ein:cell-input-pos-min cell))
                 (end   (ein:cell-input-pos-max cell))
                 ((< start end)))
    (narrow-to-region start end)))

(defun ein:ml-indent-line-function (lang-func)
  (save-restriction
    (ein:ml-narrow-to-cell)
    (funcall lang-func)))

(defun ein:ml-indent-region (lang-func start end)
  (save-restriction
    (ein:ml-narrow-to-cell)
    (funcall lang-func start end)))

(defun ein:ml-lang-setup-python ()
  "Presumably tkf had good reasons to choose only these forms from `python-mode'."
  (setq-local mode-name "EIN[Py]")
  (setq-local comment-start "# ")
  (setq-local comment-start-skip  "#+\\s-*")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local indent-line-function
              (apply-partially #'ein:ml-indent-line-function #'python-indent-line-function))
  (setq-local indent-region-function
              (apply-partially #'ein:ml-indent-region #'python-indent-region))
  (set-syntax-table python-mode-syntax-table)
  (set-keymap-parent ein:notebook-multilang-mode-map python-mode-map))

(defun ein:ml-lang-setup-R ()
  (when (and (featurep 'ess-r-mode) (featurep 'ess-custom))
    (setq-local mode-name "EIN[R]")
    (when (boundp 'ess-r-customize-alist)
      (ess-setq-vars-local ess-r-customize-alist))
    (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
    (setq-local paragraph-separate (concat "\\s-*$\\|" page-delimiter))
    (setq-local paragraph-ignore-fill-prefix t)
    (setq-local indent-line-function
                (apply-partially #'ein:ml-indent-line-function #'ess-indent-line))
    (when (and (boundp 'ess-style) (boundp 'ess-default-style))
      (setq-local ess-style ess-default-style))
    (when (and (boundp 'prettify-symbols-alist) (boundp 'ess-r-prettify-symbols))
      (setq-local prettify-symbols-alist ess-r-prettify-symbols))
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'ess-r-eldoc-function)
    (when (boundp 'ess-use-eldoc)
      (when ess-use-eldoc (eldoc-mode)))
    (when (boundp 'ess-r-mode-syntax-table)
      (set-syntax-table ess-r-mode-syntax-table))
    (when (boundp 'ess-r-mode-map)
      (set-keymap-parent ein:notebook-multilang-mode-map ess-r-mode-map))))

(defun ein:ml-lang-setup (kernelspec)
  (ein:case-equal (ein:$kernelspec-language kernelspec)
                  (("python") (ein:ml-lang-setup-python))
                  (("R") (ein:ml-lang-setup-R))))

;; (defun ein:ml-lang-setup-markdown ()
;;   "Use `markdown-mode-map'.  NOTE: This function is not used now."
;;   (when (featurep 'markdown-mode)
;;     (set-keymap-parent ein:notebook-multilang-mode-map markdown-mode-map)))

;; FIXME: dynamically call ein:ml-lang-setup-LANG using
;;        `post-command-hook'.
;; FIMXE: add more ein:ml-lang-setup-LANG to switch kaymap.


;;; yasnippet

(defvar ein:ml-yasnippet-parents '(python-mode markdown-mode)
  "Parent modes for `ein:notebook-multilang-mode' to register in yasnippet.")

(defun ein:ml-setup-yasnippet ()
  (loop for define-parents in '(yas/define-parents
                                yas--define-parents)
        when (fboundp define-parents)
        do (ignore-errors
             ;; `let' is for workaround the bug in yasnippet
             (let ((mode-sym 'ein:notebook-multilang-mode))
               (funcall define-parents
                        mode-sym
                        ein:ml-yasnippet-parents)))))

(eval-after-load "yasnippet" '(ein:ml-setup-yasnippet))


;;; Imenu Support

;; Most of this is borrowed from python.el
;; Just replace python with ein in most cases.

(defvar ein:imenu-format-item-label-function
  'ein:imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar ein:imenu-format-parent-item-label-function
  'ein:imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar ein:imenu-format-parent-item-jump-label-function
  'ein:imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun ein:imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun ein:imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (ein:imenu-format-item-label type name)))

(defun python-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))

(defun ein:imenu--put-parent (type name pos tree)
  "Add the parent with TYPE, NAME and POS to TREE."
  (let ((label
         (funcall ein:imenu-format-item-label-function type name))
        (jump-label
         (funcall ein:imenu-format-parent-item-jump-label-function type name)))
    (if (not tree)
        (cons label pos)
      (cons label (cons (cons jump-label pos) tree)))))

(defun ein:imenu--build-tree (&optional min-indent prev-indent tree)
  "Recursively build the tree of nested definitions of a node.
Arguments MIN-INDENT, PREV-INDENT and TREE are internal and should
not be passed explicitly unless you know what you are doing."
  (setq min-indent (or min-indent 0)
        prev-indent (or prev-indent python-indent-offset))
  (let* ((pos (python-nav-backward-defun))
         (type)
         (name (when (and pos (looking-at python-nav-beginning-of-defun-regexp))
                 (let ((split (split-string (match-string-no-properties 0))))
                   (setq type (car split))
                   (cadr split))))
         (label (when name
                  (funcall ein:imenu-format-item-label-function type name)))
         (indent (current-indentation))
         (children-indent-limit (+ python-indent-offset min-indent)))
    (cond ((not pos)
           ;; Nothing found, probably near to bobp.
           nil)
          ((<= indent min-indent)
           ;; The current indentation points that this is a parent
           ;; node, add it to the tree and stop recursing.
           (ein:imenu--put-parent type name pos tree))
          (t
           (ein:imenu--build-tree
            min-indent
            indent
            (if (<= indent children-indent-limit)
                ;; This lies within the children indent offset range,
                ;; so it's a normal child of its parent (i.e., not
                ;; a child of a child).
                (cons (cons label pos) tree)
              ;; Oh no, a child of a child?!  Fear not, we
              ;; know how to roll.  We recursively parse these by
              ;; swapping prev-indent and min-indent plus adding this
              ;; newly found item to a fresh subtree.  This works, I
              ;; promise.
              (cons
               (ein:imenu--build-tree
                prev-indent indent (list (cons label pos)))
               tree)))))))

(defun ein:imenu-create-index ()
  "Return tree Imenu alist for the current Python buffer.
Change `ein:imenu-format-item-label-function',
`ein:imenu-format-parent-item-label-function',
`ein:imenu-format-parent-item-jump-label-function' to
customize how labels are formatted."
  (goto-char (point-max))
  (let ((index)
        (tree))
    (while (setq tree (ein:imenu--build-tree))
      (setq index (cons tree index)))
    index))

(defun ein:imenu-create-flat-index (&optional alist prefix)
  "Return flat outline of the current Python buffer for Imenu.
Optional argument ALIST is the tree to be flattened; when nil
`ein:imenu-build-index' is used with
`ein:imenu-format-parent-item-jump-label-function'
`ein:imenu-format-parent-item-label-function'
`ein:imenu-format-item-label-function' set to
  (lambda (type name) name)
Optional argument PREFIX is used in recursive calls and should
not be passed explicitly.

Converts this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\"
      (\"decorator\" . 173)
      (\"wrap\"
       (\"wrap\" . 353)
       (\"wrapped_f\" . 393))))

To this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\" . 173)
     (\"decorator.wrap\" . 353)
     (\"decorator.wrapped_f\" . 393))"
  ;; Inspired by imenu--flatten-index-alist removed in revno 21853.
  (apply
   'nconc
   (mapcar
    (lambda (item)
      (let ((name (if prefix
                      (concat prefix "." (car item))
                    (car item)))
            (pos (cdr item)))
        (cond ((or (numberp pos) (markerp pos))
               (list (cons name pos)))
              ((listp pos)
               (cons
                (cons name (cdar pos))
                (python-imenu-create-flat-index (cddr item) name))))))
    (or alist
        (let* ((fn (lambda (_type name) name))
               (ein:imenu-format-item-label-function fn)
              (ein:imenu-format-parent-item-label-function fn)
              (ein:imenu-format-parent-item-jump-label-function fn))
          (python-imenu-create-index))))))


(provide 'ein-multilang)

;;; ein-multilang.el ends here
