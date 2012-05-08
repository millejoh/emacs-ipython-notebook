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

(define-mumamo-multi-major-mode ein:notebook-mumamo-mode
  "IPython notebook mode."
  ("IPython notebook familiy" fundamental-mode
   (ein:notebook-mumamo-python
    )))

(setq ein:notebook-mumamo-mode-map ein:notebook-mode-map)

(defun ein:notebook-mumamo-python (pos max)
  (mumamo-possible-chunk-forward
   pos max
   (lambda (pos max) "CHUNK-START-FUN"
     (ein:log 'debug "CHUNK-START-FUN(pos=%s max=%s)" pos max)
     (ein:aif (ein:notebook-mumamo-python-chunk pos max)
         (list it 'python-mode nil)))
   (lambda (pos max) "CHUNK-END-FUN"
     (ein:log 'debug "CHUNK-END-FUN(pos=%s max=%s)" pos max)
     (ein:notebook-mumamo-python-chunk pos max t))))

(defun ein:notebook-mumamo-python-chunk (pos max &optional end)
  "Helper function for `ein:notebook-mumamo-python'.

Return the point of beginning of the input element of cell after
the point POS.  Return `nil' if it cannot be found before the point
MAX.  If END is non-`nil', end of the input element is returned."
  (let* ((ewoc-node
          (ein:aif (ein:notebook-get-current-ewoc-node pos)
              (let ((ewoc-node it))
                ;; can be optimized using the argument `max'
                (while (and ewoc-node
                            (not (ein:cell-ewoc-node-p ewoc-node)))
                  (setq ewoc-node (ein:@notebook ewoc) ewoc-node))
                ewoc-node)))
         (_ (ein:log 'debug "(null ewoc-node) = %s" (null ewoc-node)))
         (cell (ein:aif ewoc-node (ein:$node-data (ewoc-data it))))
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
    (when (< input-pos pos)
      (setq input-pos (funcall find (ein:cell-next cell))))
    (ein:log 'debug "input-pos (2) = %s" input-pos)
    (when (and (not end) input-pos (> input-pos max))
      ;; FIXME: do I need "(not end)"?
      (setq input-pos nil))
    (ein:log 'debug "input-pos (3) = %s" input-pos)
    input-pos))

(provide 'ein-mumamo)

;;; ein-mumamo.el ends here
