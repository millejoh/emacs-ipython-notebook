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



;;; Dynamic Variables

(defvar ein:helm-pattern 'helm-pattern
  "Dynamically bound to one of `helm-pattern' or `anything-pattern'.")

(defvar ein:helm-kernel nil
  "Dynamically bound to a kernel object.")



;;; History search

(defvar ein:helm-source-kernel-history
  '((name . "IPython history")
    (candidates . (lambda ()
                    (ein:kernel-history-search-synchronously
                     ein:helm-kernel (eval ein:helm-pattern))))
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
  '((name . "IPython notebook buffers")
    (candidates . ein:notebook-opened-buffer-names)
    (type . buffer))
  "Helm/anything source for notebook buffers.")


;;; "Export" sources to `helm/anything-c-source-*'

(defvaralias 'anything-c-source-ein-notebook-buffers
  'ein:helm-source-notebook-buffers
  "Alias to `anything-c-source-ein-notebook-buffers'")

(defvaralias 'helm-c-source-ein-notebook-buffers
  'ein:helm-source-notebook-buffers
  "Alias to `ein:helm-source-notebook-buffers'")


;;; Helm/anything commands

;;;###autoload
(defun anything-ein-notebook-buffers ()
  "Choose opened notebook using anything.el interface."
  (interactive)
  (anything-other-buffer ein:helm-source-notebook-buffers "*anything ein*"))

;;;###autoload
(defun helm-ein-notebook-buffers ()
  "Choose opened notebook using helm interface."
  (interactive)
  (helm-other-buffer ein:helm-source-notebook-buffers "*helm ein*"))

(provide 'ein-helm)
;;; ein-helm.el ends here
