;;; ein-python-send.el --- Ad hoc sending of code fragments to kernel   -*- lexical-binding: t -*-

;; Copyright (C) 2012- The Authors

;; This file is NOT part of GNU Emacs.

;; ein-python-send.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-python-send.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-python-send.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; python parsing code by authors of elpy (Schaefer et al)

;;; Code:

(autoload 'ein:get-notebook "ein-notebook")

(defvar ein:python-send-map)

(defun ein:python-send--prepare (&optional reset)
  (cl-assert (boundp 'ein:python-send-map) nil
	     "ein:python-send--prepare: %s not called"
	     "ein:python-send--init")
  (unless (and (buffer-live-p (current-buffer))
	       (eq major-mode 'python-mode))
    (error "ein:python-send--prepare: %s is not a python buffer" (buffer-name)))
  (when (or (not (ein:get-notebook)) reset)
    (aif (ein:notebook-opened-notebooks)
	(let ((choice
	       (ein:completing-read
		"Notebook: "
		(mapcar (lambda (nb) (ein:$notebook-notebook-name nb)) it))))
	  (setq ein:%notebook% (seq-find
				(lambda (nb)
				  (string= choice (ein:$notebook-notebook-name nb)))
				it)))
      (error "ein:python-send--prepare: No open notebooks"))))

(defun ein:python-send-region-or-buffer (&optional reset)
  "Based on `elpy-shell--send-region-or-buffer-internal' by Schaefer et al."
  (interactive "P")
  (ein:python-send--prepare reset)
  (if (use-region-p)
      (let ((region (python-shell-buffer-substring
		     (region-beginning) (region-end))))
	(when (string-match "\t" region)
	  (message "Region contained tabs, this might cause weird errors"))
	;; python-shell-buffer-substring (intentionally?) does not accurately
	;; respect (region-beginning); it always start on the first character
	;; of the respective line even if that's before the region beginning
	;; Here we post-process the output to remove the characters before
	;; (region-beginning) and the start of the line. The end of the region
	;; is handled correctly and needs no special treatment.
	(let* ((bounds (save-excursion
			 (goto-char (region-beginning))
			 (bounds-of-thing-at-point 'line)))
	       (used-part (string-trim
			   (buffer-substring-no-properties
			    (car bounds)
			    (min (cdr bounds) (region-end)))))
	       (relevant-part (string-trim
			       (buffer-substring-no-properties
				(max (car bounds) (region-beginning))
				(min (cdr bounds) (region-end))))))
	  (setq region
		;; replace just first match
		(replace-regexp-in-string
		 (concat "\\(" (regexp-quote used-part) "\\)\\(?:.*\n?\\)*\\'")
		 relevant-part
		 region t t 1))
	  (ein:shared-output-eval-string (ein:get-kernel) region)))
    (ein:shared-output-eval-string (ein:get-kernel) (buffer-string)))
  (if (use-region-p)
      (progn
	(goto-char (region-end))
	(deactivate-mark))
    (goto-char (point-max))))

(defun ein:python-send-statement (&optional reset)
  "Based on `elpy-shell-send-statement' by Schaefer et al."
  (interactive "P")
  (ein:python-send--prepare reset)
  (python-nav-beginning-of-statement)
  (unless (looking-at "[[:space:]]*$")
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (progn (ein:python-send--nav-end-of-statement) (point))))
      (unless (eq beg end)
	(ein:shared-output-eval-string (ein:get-kernel)
				       (buffer-substring beg end))))))

(defun ein:python-send--nav-end-of-statement ()
  "Based on `elpy-shell--nav-end-of-statement' by Schaefer et al."
  (let ((continue t)
        p)
    (while (and (not (eq p (point))) continue)
      ;; is there another block at same indentation level?
      (setq p (point))
      (ein:python-send--nav-forward-block)
      (if (eq p (point))
	  (progn
	   ;; nope, go to the end of the block and done
	    (python-nav-end-of-block)
	    (setq continue nil))
	(unless (eq 0 (string-match-p "\\s-*el\\(?:se:\\|if[^\w]\\)"
				      (thing-at-point 'line)))
	  (forward-line -1)
	  (while (and (or (eq (string-match-p "\\s-*$" (thing-at-point 'line)) 0)
			  (python-info-current-line-comment-p))
		      (not (eq (point) (point-min))))
	    (forward-line -1))
	  (setq continue nil)))))
  (end-of-line))

(defun ein:python-send--nav-forward-block ()
  "Based on `elpy-shell--nav-forward-block' by Schaefer et al.

Move to the next line indented like point.  This will skip over lines and
statements with different indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-forward-statement)
    (while (and (< indent (current-indentation))
                (not (eobp)))
      (when (equal (point) cur)
        (error "Statement does not finish"))
      (setq cur (point))
      (python-nav-forward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun ein:python-send--init ()
  (unless (boundp 'ein:python-send-map)
    (require 'python)
    (setq ein:python-send-map
	  (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "e") 'ein:python-send-statement)
	    (define-key map (kbd "r") 'ein:python-send-region-or-buffer)
	    map))
    (define-key python-mode-map (kbd "C-c C-/") ein:python-send-map)))

(provide 'ein-python-send)

;;; ein-python-send.el ends here
