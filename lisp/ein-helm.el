;;; ein-helm.el --- Helm/anything commands

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-helm.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-helm.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-helm.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

(declare-function anything-other-buffer "anything")
(declare-function helm-other-buffer "helm")

(require 'ein-kernel)


;;; Macros

(defmacro ein:helm-export-source (name)
  (let* ((orig-source (intern (format "ein:helm-source-%s"        name)))
         (any-source  (intern (format "anything-c-source-ein-%s" name)))
         (helm-source (intern (format "helm-c-source-ein-%s"     name)))
         (docstring (format "Alias to `%s'" orig-source)))
    `(progn
       (defvaralias ',helm-source ',orig-source ,docstring)
       (defvaralias ',any-source  ',orig-source ,docstring))))


;;; Dynamic Variables

(defvar ein:helm-pattern 'helm-pattern
  "Dynamically bound to one of `helm-pattern' or `anything-pattern'.")

(defvar ein:helm-kernel nil
  "Dynamically bound to a kernel object.")



;;; History search

(defcustom ein:helm-kernel-history-search-auto-pattern t
  "Automatically construct search pattern when non-`nil'.

1. Single space is converted to \"*\".
2. A backslash followed by a space is converted to a single space.
3. A \"*\" is added at the beginning and end of the pattern.

This variable applies to both `helm-ein-kernel-history' and
`anything-ein-kernel-history'."
  :type 'boolean
  :group 'ein)

(defun ein:helm-kernel-history-search-construct-pattern (pattern)
  (when ein:helm-kernel-history-search-auto-pattern
    (setq pattern
          (replace-regexp-in-string "[^\\\\ ]\\( \\)[^\\\\ ]"
                                    "*" pattern nil nil 1))
    (setq pattern
          (replace-regexp-in-string "\\\\ " " " pattern))
    (setq pattern (concat "*" pattern "*")))
  pattern)

(defun ein:helm-kernel-history-search-get-candidates ()
  "Retrieve search result from kernel.
It requires the following dynamical variables:
* `ein:helm-pattern'
* `ein:helm-kernel'"
  (let* ((pattern (ein:helm-kernel-history-search-construct-pattern
                   (eval ein:helm-pattern)))
         (candidates (ein:kernel-history-search-synchronously
                      ein:helm-kernel pattern :unique t)))
    ;; Most recent history first:
    (nreverse candidates)))

(defvar ein:helm-source-kernel-history
  '((name . "IPython history")
    (candidates . ein:helm-kernel-history-search-get-candidates)
    (requires-pattern . 3)
    ;; There is no need to filter out candidates:
    (match . (identity))
    (volatile)
    (action . insert)
    (delayed)
    (multiline))
  "Helm/anything source for searching kernel history.")

;;;###autoload
(defun anything-ein-kernel-history ()
  "Search kernel execution history then insert the selected one."
  (interactive)
  (let ((ein:helm-pattern 'anything-pattern)
        (ein:helm-kernel (ein:get-kernel-or-error)))
    (anything-other-buffer ein:helm-source-kernel-history "*anything ein*")))

;;;###autoload
(defun helm-ein-kernel-history ()
  "Search kernel execution history then insert the selected one."
  (interactive)
  (let ((ein:helm-pattern 'helm-pattern)
        (ein:helm-kernel (ein:get-kernel-or-error)))
    (helm-other-buffer ein:helm-source-kernel-history "*helm ein*")))



;;; Notebook buffers

(defvar ein:helm-source-notebook-buffers
  '((name . "All IPython notebook buffers")
    (candidates . ein:notebook-opened-buffer-names)
    (type . buffer))
  "Helm/anything source for all opened notebook buffers.")

(defvar ein:helm-source-modified-notebook-buffers
  '((name . "Modified IPython notebook buffers")
    (candidates
     . (lambda ()
         (ein:notebook-opened-buffer-names #'ein:notebook-modified-p)))
    (type . buffer))
  "Helm/anything source for modified notebook buffers.")

(defvar ein:helm-source-saved-notebook-buffers
  '((name . "Saved IPython notebook buffers")
    (candidates
     . (lambda ()
         (ein:notebook-opened-buffer-names
          (lambda (nb) (not (ein:notebook-modified-p nb))))))
    (type . buffer))
  "Helm/anything source for saved notebook buffers.")


;;; "Export" sources to `helm/anything-c-source-*'
(ein:helm-export-source notebook-buffers)
(ein:helm-export-source modified-notebook-buffers)
(ein:helm-export-source saved-notebook-buffers)


;;; Helm/anything commands

(defvar ein:helm-notebook-buffer-sources
  '(ein:helm-source-modified-notebook-buffers
    ein:helm-source-saved-notebook-buffers))

;;;###autoload
(defun anything-ein-notebook-buffers ()
  "Choose opened notebook using anything.el interface."
  (interactive)
  (anything-other-buffer ein:helm-notebook-buffer-sources "*anything ein*"))

;;;###autoload
(defun helm-ein-notebook-buffers ()
  "Choose opened notebook using helm interface."
  (interactive)
  (helm-other-buffer ein:helm-notebook-buffer-sources "*helm ein*"))

(provide 'ein-helm)
;;; ein-helm.el ends here
