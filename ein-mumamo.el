;;; ein-mumamo.el --- MuMaMo for notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

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
(require 'mumamo-cmirr)

(require 'ein-notebook)

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


(define-derived-mode ein:notebook-bg-mode fundamental-mode "ein:bg"
  "Background mode for `ein:notebook-mumamo-mode'."
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

;; "Sync" `ein:notebook-mumamo-mode-map' with `ein:notebook-mode-map'.
(setcdr ein:notebook-mumamo-mode-map (cdr ein:notebook-mode-map))

(add-hook 'ein:notebook-mumamo-mode-hook 'ein:notebook-setup-kill-buffer-hook)

(defmacro ein:mumamo-define-chunk (name)
  (let ((funcname (intern (format "ein:mumamo-chunk-%s" name)))
        (mode (intern (format "ein:mumamo-%s-mode" name)))
        (cell-p (intern (format "ein:%s-p" name))))
    `(defun ,funcname (pos max)
       (mumamo-possible-chunk-forward
        pos max
        (lambda (pos max) "CHUNK-START-FUN"
          (ein:log 'debug "CHUNK-START-FUN(pos=%s max=%s)" pos max)
          (ein:aif (ein:mumamo-find-edge pos max nil #',cell-p)
              (list it (if (functionp ,mode)
                           ,mode
                         ein:mumamo-fallback-mode)
                    nil)))
        (lambda (pos max) "CHUNK-END-FUN"
          (ein:log 'debug "CHUNK-END-FUN(pos=%s max=%s)" pos max)
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
  (ein:log 'debug "EIN:MUMAMO-FIND-EDGE(pos=%s max=%s end=%s cell-p=%s)"
           pos max end cell-p)
  (let* ((ewoc-node
          (ein:notebook-get-nearest-cell-ewoc-node pos max cell-p))
         (_ (ein:log 'debug "(null ewoc-node) = %s" (null ewoc-node)))
         (cell (ein:aand ewoc-node
                         (ein:$node-data (ewoc-data it))))
         (_ (ein:log 'debug "(null cell) = %s" (null cell)))
         (find
          (lambda (c)
            (ein:aand c
                      (ein:cell-element-get it (if end :after-input :input))
                      (progn
                        (ein:log 'debug "(null it) = %s" (null it))
                        (ewoc-location it))
                      (if end it (1+ it)))))
         (input-pos (funcall find cell)))
    (ein:log 'debug "input-pos (1) = %s" input-pos)
    (when (and input-pos (< input-pos pos))
      (setq input-pos (ein:aand (ein:cell-next cell)
                                (when (funcall cell-p it) (funcall find it)))))
    (ein:log 'debug "input-pos (2) = %s" input-pos)
    (when (and input-pos (> input-pos max))
      (setq input-pos nil))
    (ein:log 'debug "input-pos (3) = %s" input-pos)
    input-pos))

(provide 'ein-mumamo)

;;; ein-mumamo.el ends here
