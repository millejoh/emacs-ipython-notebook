;;; ein-mumamo.el --- MuMaMo for notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-mumamo.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-mumamo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-mumamo.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mumamo)

(require 'ein-worksheet)



;;; Customization

(defcustom ein:mumamo-codecell-mode 'python-mode
  "Major Mode for Code Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:mumamo-textcell-mode 'text-mode
  "Major Mode for Text Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:mumamo-htmlcell-mode 'html-mode
  "Major Mode for HTML Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:mumamo-markdowncell-mode 'markdown-mode
  "Major Mode for Markdown Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:mumamo-rawcell-mode 'rst-mode
  "Major Mode for Raw Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:mumamo-headingcell-mode 'text-mode
  "Major Mode for Heading Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:mumamo-fallback-mode 'text-mode
  "Fallback Major Mode."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein:use-mumamo-indent-line-function-workaround t
  "Turn on workaround for `mumamo-indent-line-function'.

In code cell, hitting TAB or C-j at the end of input area causes
error from MuMaMo.  When this variable is non-`nil', EIN patches
`mumamo-indent-line-function' to workaround this problem.  This
workaround is on by default.

Note that python-mode's indentation function has other problems
with MuMaMo.  For example, hitting TAB twice, which decreases the
indentation level by one in normal Python buffer, causes similar
error in code cell.  The current workaround does not fix this
problem."
  :type 'boolean
  :group 'ein)

(defcustom ein:mumamo-indent-line-function-dummy-code "
def ein_dummy():
    return"
  "Dummy code block for `mumamo-indent-line-function' workaround.
This code block will be inserted at the end of cell input before
indentation and then removed afterward (so user will not see this
code).

This is ugly but... \"practicality beats purity\"...
I guess somebody should fix python.el and/or MuMaMo, in order to
remove this ugliness.

To make the workaround less aggressive, you can set a newline
\"\\n\" for this variable.  In that case, you will be affected by
`issue 24`_.

.. _issue 24: https://github.com/tkf/emacs-ipython-notebook/issues/24"
  :type 'boolean
  :group 'ein)



;;; Workaround

(defadvice mumamo-indent-line-function
  (around ein:mumamo-indent-line-function-workaround)
  "Workaround the indentation problem when the cursor is in the
code cell."
  (let ((cell (ein:worksheet-get-current-cell)))
    ;; Check if the current buffer is notebook AND the current cell is
    ;; code cell.
    (if (ein:codecell-p cell)
        (let ((cur (copy-marker (point)))
              (end (copy-marker (1+ (ein:cell-input-pos-max cell)))))
          ;;             v-- execute `delete-char' here
          ;; ... [] ......DUMMY
          ;;      ^- cur       ^- end (non-inclusive end of cell)
          ;;      ^- `ad-do-it' here
          (unwind-protect
              (progn
                (goto-char (1- end))
                (insert ein:mumamo-indent-line-function-dummy-code)
                (goto-char cur)
                ad-do-it)
            (save-excursion
              (let ((len (length ein:mumamo-indent-line-function-dummy-code)))
                (goto-char (- end 1 len))
                (delete-char len)))))
      ad-do-it)))

(defun ein:mumamo-indent-line-function-workaround-turn-on ()
  "Activate advice for `mumamo-indent-line-function'.
Called via `ein:notebook-mumamo-mode-hook'."
  (when ein:use-mumamo-indent-line-function-workaround
    (ad-enable-advice 'mumamo-indent-line-function 'around
                      'ein:mumamo-indent-line-function-workaround)
    (ad-activate 'mumamo-indent-line-function)))

(defun ein:mumamo-imenu-setup-maybe ()
  "Set `imenu-create-index-function' if the current buffer is the
notebook buffer.
This function is called via `after-change-major-mode-hook', to set
the variable every time visiting the different chunks.

.. note:: Making `imenu-create-index-function' permanent-local
   also solves the problem.  However, this will make the variable
   permanent-local in *any* buffer, including the buffers
   irrelevant to EIN.  Therefore, the current approach is taken.

This is the same workaround as `ein:ac-setup-maybe'."
  (when (ein:worksheet-buffer-p)
    (ein:worksheet-imenu-setup)))

(add-hook 'after-change-major-mode-hook 'ein:mumamo-imenu-setup-maybe)



;;; `ein:notebook-mumamo-mode'

(define-derived-mode ein:notebook-bg-mode fundamental-mode "ein:bg"
  "Background mode for `ein:notebook-mumamo-mode'."
  (setq font-lock-defaults '(nil t))
  (font-lock-mode))

(define-mumamo-multi-major-mode ein:notebook-mumamo-mode
  "IPython notebook mode."
  ("IPython notebook familiy" ein:notebook-bg-mode
   (ein:mumamo-chunk-codecell
    ein:mumamo-chunk-textcell
    ein:mumamo-chunk-htmlcell
    ein:mumamo-chunk-markdowncell
    ein:mumamo-chunk-rawcell
    ein:mumamo-chunk-headingcell
    )))

(add-hook 'ein:notebook-mumamo-mode-hook
          'ein:mumamo-indent-line-function-workaround-turn-on)



;;; Chunk functions

(defmacro ein:mumamo-define-chunk (name)
  (let ((funcname (intern (format "ein:mumamo-chunk-%s" name)))
        (mode (intern (format "ein:mumamo-%s-mode" name)))
        (cell-p (intern (format "ein:%s-p" name))))
    `(defun ,funcname (pos max)
       (mumamo-possible-chunk-forward
        pos max
        (lambda (pos max) "CHUNK-START-FUN"
          (ein:log 'blather "CHUNK-START-FUN(pos=%s max=%s)" pos max)
          (ein:aif (ein:mumamo-find-edge pos max nil #',cell-p)
              (list it (if (functionp ,mode)
                           ,mode
                         ein:mumamo-fallback-mode)
                    nil)))
        (lambda (pos max) "CHUNK-END-FUN"
          (ein:log 'blather "CHUNK-END-FUN(pos=%s max=%s)" pos max)
          (ein:mumamo-find-edge pos max t #',cell-p))))))

(ein:mumamo-define-chunk codecell)
(ein:mumamo-define-chunk textcell)
(ein:mumamo-define-chunk htmlcell)
(ein:mumamo-define-chunk markdowncell)
(ein:mumamo-define-chunk rawcell)
(ein:mumamo-define-chunk headingcell)

(defun ein:mumamo-find-edge (pos max end cell-p)
  "Helper function for `ein:mumamo-chunk-codecell'.

Return the point of beginning of the input element of cell after
the point POS.  Return `nil' if it cannot be found before the point
MAX.  If END is non-`nil', end of the input element is returned."
  (ein:log 'blather "EIN:MUMAMO-FIND-EDGE(pos=%s max=%s end=%s cell-p=%s)"
           pos max end cell-p)
  (let* ((ewoc-node
          (ein:worksheet-get-nearest-cell-ewoc-node pos max cell-p))
         (_ (ein:log 'blather "(null ewoc-node) = %s" (null ewoc-node)))
         (cell (ein:aand ewoc-node
                         (ein:$node-data (ewoc-data it))))
         (_ (ein:log 'blather "(null cell) = %s" (null cell)))
         (find
          (lambda (c)
            (ein:aand c
                      (ein:cell-element-get it (if end :after-input :input))
                      (progn
                        (ein:log 'blather "(null it) = %s" (null it))
                        (ewoc-location it))
                      (if end it (1+ it)))))
         (input-pos (funcall find cell)))
    (ein:log 'blather "input-pos (1) = %s" input-pos)
    (when (and input-pos (< input-pos pos))
      (setq input-pos (ein:aand (ein:cell-next cell)
                                (when (funcall cell-p it) (funcall find it)))))
    (ein:log 'blather "input-pos (2) = %s" input-pos)
    (when (and input-pos (> input-pos max))
      (setq input-pos nil))
    (ein:log 'blather "input-pos (3) = %s" input-pos)
    input-pos))

(provide 'ein-mumamo)

;;; ein-mumamo.el ends here
