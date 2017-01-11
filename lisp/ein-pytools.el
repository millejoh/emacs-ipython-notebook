;;; ein-pytools.el --- Python tools build on top of kernel

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-pytools.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pytools.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pytools.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

;; for `ein:pytools-pandas-to-ses'
(declare-function ses-yank-tsf "ses")
(declare-function ses-command-hook "ses")

(require 'ein-kernel)

(defun ein:goto-file (filename lineno &optional other-window)
  "Jump to file FILEAME at line LINENO.
If OTHER-WINDOW is non-`nil', open the file in the other window."
  (funcall (if other-window #'find-file-other-window #'find-file) filename)
  (goto-char (point-min))
  (forward-line (1- lineno)))

(defun ein:goto-marker (marker &optional other-window)
  (funcall (if other-window #'pop-to-buffer #'switch-to-buffer)
           (marker-buffer marker))
  (goto-char marker))

(defcustom ein:propagate-connect t
  "Set to `t' to connect to the notebook after jumping to a buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

(defun ein:pytools-setup-hooks (kernel notebook)
  (push (cons #'ein:pytools-add-sys-path kernel)
        (ein:$kernel-after-start-hook kernel))
  ;; (push (cons #'ein:pytools-get-notebook-dir (list kernel notebook))
  ;;       (ein:$kernel-after-start-hook kernel))
  )

(defun ein:pytools-add-sys-path (kernel)
  (ein:kernel-execute
   kernel
   (format "__import__('sys').path.append('%s')" ein:source-dir)))

(defun ein:set-buffer-file-name (nb msg-type content -not-used-)
  (let ((buf (ein:notebook-buffer nb)))
    (ein:case-equal msg-type
      (("stream" "output")
       (with-current-buffer buf
         (setq buffer-file-name
               (expand-file-name
                (format "%s" (ein:$notebook-notebook-name nb))
                (plist-get content :text))))))))

(defun ein:pytools-get-notebook-dir (packed)
  (multiple-value-bind (kernel notebook) packed
    (ein:kernel-execute
     kernel
     (format "print(__import__('os').getcwd(),end='')")
     (list
      :output (cons
               #'ein:set-buffer-file-name
               notebook)))))


;;; Tooltip and help

(defun ein:pytools-request-tooltip (kernel func)
  (interactive (list (ein:get-kernel-or-error)
                     (ein:object-at-point-or-error)))
  (if (>= (ein:$kernel-api-version kernel) 3)
      (ein:kernel-execute
       kernel
       (format "__import__('ein').print_object_info_for(%s)" func)
       (list
        :output (cons
                 (lambda (name msg-type content -metadata-not-used-)
                   (ein:case-equal msg-type
                     (("stream" "display_data")
                      (ein:pytools-finish-tooltip name (ein:json-read-from-string (plist-get content :text)) nil))))
                 func)))
    (ein:kernel-object-info-request
     kernel func (list :object_info_reply
                       (cons #'ein:pytools-finish-tooltip nil)))))

(declare-function pos-tip-show "pos-tip")
(declare-function popup-tip "popup")

(defun ein:pytools-finish-tooltip (-ignore- content -metadata-not-used-)
  ;; See: Tooltip.prototype._show (tooltip.js)
  (let ((tooltip (ein:kernel-construct-help-string content))
        (defstring (ein:kernel-construct-defstring content))
        (name (plist-get content :name)))
    (if tooltip
        (cond
         ((and window-system (featurep 'pos-tip))
          (pos-tip-show tooltip 'ein:pos-tip-face nil nil 0))
         ((featurep 'popup)
          (popup-tip tooltip))
         (t (when (stringp defstring)
              (message (ein:trim (ansi-color-apply defstring))))))
      (ein:log 'info "no info for %s" name))))

(defun ein:pytools-request-help (kernel func)
  (interactive (list (ein:get-kernel-or-error)
                     (ein:object-at-point-or-error)))
  (ein:kernel-execute kernel
                      (format "%s?" func) ; = code
                      nil                 ; = callbacks
                      ;; It looks like that magic command does
                      ;; not work in silent mode.
                      :silent nil))

(defun ein:pytools-request-tooltip-or-help (&optional pager)
  "Show the help for the object at point using tooltip.
When the prefix argument ``C-u`` is given, open the help in the
pager buffer.  You can explicitly specify the object by selecting it."
  (interactive "P")
  (call-interactively (if pager
                          #'ein:pytools-request-help
                        #'ein:pytools-request-tooltip)))


;;; Source jump

(defvar ein:pytools-jump-stack nil)

(defvar ein:pytools-jump-to-source-not-found-regexp
  (ein:join-str "\\|"
                (list "^WARNING: .*"
                      "^Traceback (most recent call last):\n"
                      "^.*<ipython-input-[^>\n]+>\n"
                      "^\n")))

(defun ein:pytools-jump-to-source (kernel object &optional
                                          other-window notebook)
  (ein:log 'info "Jumping to the source of %s..." object)
  (let ((last (car ein:pytools-jump-stack)))
    (if (ein:aand last (eql (current-buffer) (marker-buffer it)))
        (unless (equal (point) (marker-position last))
          (push (point-marker) ein:pytools-jump-stack))
      (setq ein:pytools-jump-stack (list (point-marker)))))
  (ein:kernel-execute
   kernel
   (format "__import__('ein').find_source('%s')" object)
   (list
    :output
    (cons
     (lambda (packed msg-type content -metadata-not-used-)
       (destructuring-bind (kernel object other-window notebook)
           packed
         (ein:case-equal msg-type
           (("stream" "display_data")
            (ein:aif (or (plist-get content :text) (plist-get content :data))
                (if (string-match ein:pytools-jump-to-source-not-found-regexp
                                  it)
                    (ein:log 'info
                      "Jumping to the source of %s...Not found" object)
                  (destructuring-bind (filename &optional lineno &rest ignore)
                      (split-string it "\n")
                    (setq lineno (string-to-number lineno))
                    (setq filename
                          (ein:kernel-filename-from-python kernel filename))
                    (let ((ein:connect-default-notebook nil))
                      ;; Avoid auto connection to connect to the
                      ;; NOTEBOOK instead of the default one.
                      (ein:goto-file filename lineno other-window))
                    ;; Connect current buffer to NOTEBOOK. No reconnection.
                    (ein:connect-buffer-to-notebook notebook nil t)
                    (push (point-marker) ein:pytools-jump-stack)
                    (ein:log 'info
                      "Jumping to the source of %s...Done" object)))))
           (("pyerr" "error")
            (ein:log 'info
              "Jumping to the source of %s...Not found" object)))))
     (list kernel object other-window notebook)))))

(defun ein:pytools-jump-to-source-command (&optional other-window)
  "Jump to the source code of the object at point.
When the prefix argument ``C-u`` is given, open the source code
in the other window.  You can explicitly specify the object by
selecting it."
  (interactive "P")
  (let ((kernel (ein:get-kernel))
        (object (ein:object-at-point)))
    (assert (ein:kernel-live-p kernel) nil "Kernel is not ready.")
    (assert object nil "Object at point not found.")
    (ein:pytools-jump-to-source kernel object other-window
                                (when ein:propagate-connect
                                  (ein:get-notebook)))))

(defun ein:pytools-jump-back-command (&optional other-window)
  "Go back to the point where `ein:pytools-jump-to-source-command'
is executed last time.  When the prefix argument ``C-u`` is
given, open the last point in the other window."
  (interactive "P")
  (when (ein:aand (car ein:pytools-jump-stack)
                  (equal (point) (marker-position it)))
    (setq ein:pytools-jump-stack (cdr ein:pytools-jump-stack)))
  (ein:aif (car ein:pytools-jump-stack)
      (ein:goto-marker it other-window)
    (ein:log 'info "Nothing on stack.")))

(define-obsolete-function-alias
  'ein:pytools-eval-string-internal
  'ein:shared-output-eval-string "0.1.2")

(defun ein:pytools-doctest ()
  "Do the doctest of the object at point."
  (interactive)
  (let ((object (ein:object-at-point)))
    (ein:shared-output-eval-string
     (format "__import__('ein').run_docstring_examples(%s)" object)
     t)))

(defun ein:pytools-whos ()
  "Execute ``%whos`` magic command and popup the result."
  (interactive)
  (ein:shared-output-eval-string "%whos" t))

(defun ein:pytools-hierarchy (&optional ask)
  "Draw inheritance graph of the class at point.
hierarchymagic_ extension is needed to be installed.
You can explicitly specify the object by selecting it.

.. _hierarchymagic: https://github.com/tkf/ipython-hierarchymagic"
  (interactive "P")
  (let ((object (ein:object-at-point)))
    (when ask
      (setq object (read-from-minibuffer "class or object: " object)))
    (assert (and object (not (equal object "")))
            nil "Object at point not found.")
    (ein:shared-output-eval-string (format "%%hierarchy %s" object) t)))

(defun ein:pytools-pandas-to-ses (dataframe)
  "View pandas_ DataFrame in SES_ (Simple Emacs Spreadsheet).
Open a `ses-mode' buffer and import DataFrame object into it.

SES_ is distributed with Emacs since Emacs 22, so you don't need
to install it if you are using newer Emacs.

.. _pandas: http://pandas.pydata.org
.. _SES: http://www.gnu.org/software/emacs/manual/html_node/ses/index.html"
  (interactive (list (read-from-minibuffer "pandas DataFrame "
                                           (ein:object-at-point))))
  (let ((buffer (get-buffer-create
                 (generate-new-buffer-name "*ein:ses pandas*"))))
    ;; fetch TSV (tab separated values) via stdout
    (ein:kernel-request-stream
     (ein:get-kernel)
     (concat dataframe ".to_csv(__import__('sys').stdout, sep='\\t')")
     (lambda (tsv buffer)
       (with-current-buffer buffer
         (cl-flet ((y-or-n-p
                    (prompt)
                    (if (string-prefix-p "Yank will insert " prompt)
                        t
                      (error "Unexpected prompt: %s" prompt))))
           ;; Import DataFrame as TSV
           (ses-yank-tsf tsv nil))
         ;; Force SES to update (equivalent to run `post-command-hook').
         (ses-command-hook)))
     (list buffer))
    ;; Open `ses-mode' buffer
    (with-current-buffer buffer
      (ses-mode))
    (pop-to-buffer buffer)))

(defun ein:pytools-export-buffer (buffer format)
  "Export contents of notebook using nbconvert_ to user-specified format
\(options will depend on the version of nbconvert available\) to a new buffer.

Currently EIN/IPython supports exporting to the following formats:

 - HTML
 - JSON (this is basically the same as opening the ipynb file in a buffer).
 - Latex
 - Markdown
 - Python
 - RST
 - Slides

.. _nbconvert: http://ipython.org/ipython-doc/stable/notebook/nbconvert.html"
  (interactive (list (read-buffer "Buffer: " (current-buffer) t)
                     (completing-read "Export format: "
                                      (list "html"
                                            "json"
                                            "latex"
                                            "markdown"
                                            "python"
                                            "rst"
                                            "slides"))))
  (let* ((nb (first (ein:notebook-opened-notebooks
                     #'(lambda (nb)
                         (equal (buffer-name (ein:notebook-buffer nb))
                                buffer)))))
         (json (json-encode (ein:notebook-to-json nb)))
         (name (format "*ein %s export: %s*" format (ein:$notebook-notebook-name nb)))
         (buffer (get-buffer-create name)))
    (if (equal format "json")
        (with-current-buffer buffer
          (erase-buffer)
          (insert json)
          (json-pretty-print (point-min) (point-max)))
      (ein:kernel-request-stream
       (ein:get-kernel)
       (format "__import__('ein').export_nb('%s', '%s')"
               json
               format)
       (lambda (export buffer)
         (with-current-buffer buffer
           (erase-buffer)
           (insert export)))
       (list buffer)))
    (switch-to-buffer buffer)))



;;;; Helper functions for working with matplotlib

(defun ein:pytools-set-figure-size (width height)
  "Set the default figure size for matplotlib figures. Works by setting `rcParams['figure.figsize']`."
  (interactive "nWidth: \nnHeight: ")
  (ein:shared-output-eval-string (format "__import__('ein').set_figure_size(%s,%s)" width height)))

(provide 'ein-pytools)

;;; ein-pytools.el ends here
