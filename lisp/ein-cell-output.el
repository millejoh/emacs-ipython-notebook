;;; ein-cell-output.el --- Cell module

;; (C) 2015- John M. Miller

;; Author: John M. Miller (millejoh at mac dot com)

;; This file is NOT part of GNU Emacs.

;; ein-cell-output.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-cell-output.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-cell-output.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Because wiriting cell outputs to nbformat v4.0 json is complicated
;;  enought that it warrants a file all to its own.

;;; Code:

(require 'ein-cell)
(require 'cl-generic)

(defmethod ein:cell-to-json ((cell ein:codecell) &optional discard-output)
  "Return json-ready alist."
  `((input . ,(ein:cell-get-text cell))
    (cell_type . "code")
    ,@(ein:aif (ein:oref-safe cell :input-prompt-number)
          `((prompt_number . ,it)))
    (outputs . ,(if discard-output [] (apply #'vector (oref cell :outputs))))
    (language . "python")
    (collapsed . ,(if (oref cell :collapsed) t json-false))))

(defvar ein:output-type-map
  '((:svg . :image/svg) (:png . :image/png) (:jpeg . :image/jpeg)
    (:text . :text/plain)
    (:html . :text/html) (:latex . :text/latex) (:javascript . :text/javascript)))

(defun ein:output-property (maybe-property)
  (cdr (assoc maybe-property ein:output-type-map)))

(defun ein:get-slideshow (cell)
  (setq slide_type (oref cell :slidetype))
  (setq SS_table (make-hash-table))
  (setf (gethash 'slide_type SS_table) slide_type)
  SS_table)

(defmethod ein:cell-to-nb4-json ((cell ein:codecell) wsidx &optional discard-output)

  (setq SS_table (ein:get-slide-show cell))
  
  (let ((metadata `((collapsed . ,(if (oref cell :collapsed) t json-false))
		    (autoscroll . json-false)
                    (ein.tags . (,(format "worksheet-%s" wsidx)))
		    (slideshow . ,SS_table)
		    ))
        (outputs (if discard-output []
                   (oref cell :outputs)))
        (renamed-outputs '())
        (execute-count (ein:aif (ein:oref-safe cell :input-prompt-number)
                           (and (numberp it) it))))
    (unless discard-output
      (dolist (o outputs)
	(let ((type  (plist-get o :output_type)))
	  (push (cond ((equal type "display_data") (ein:cell-display-data-output-to-json o)) 
		      ((equal type "execute_result") (ein:cell-execute-result-output-to-json o))
		      ((equal type "stream") (ein:cell-stream-output-to-json o))
		      ((equal type "error") (ein:cell-error-output-to-json o))
		      (t (warn "Unhandled output_type %s." type)))
		renamed-outputs))))
    `((source . ,(ein:cell-get-text cell))
      (cell_type . "code")
      ,@(if execute-count
            `((execution_count . ,execute-count))
          `((execution_count . 0)))
      (outputs . ,(apply #'vector (or renamed-outputs outputs)))
      (metadata . ,metadata))))


(defmethod ein:cell-to-json ((cell ein:textcell) &optional discard-output)
  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ein:cell-get-text cell))))

(defmethod ein:cell-to-nb4-json ((cell ein:textcell) wsidx &optional discard-output)
  (setq SS_table (ein:get-slide-show cell))
  

  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ein:cell-get-text cell))
    (metadata . ((ein.tags . (,(format "worksheet-%s" wsidx)))
		 (slideshow . ,SS_table)))))

(defmethod ein:cell-to-nb4-json ((cell ein:headingcell) wsidx &optional discard-output)
  (setq SS_table (ein:get-slide-show cell))
  
  (let ((header (make-string (oref cell :level) ?#)))
    `((cell_type . "markdown")
      (source .  ,(format "%s %s" header (ein:cell-get-text cell)))
      (metadata . ((ein.tags . (,(format "worksheet-%s" wsidx)))
		   (slideshow . ,SS_table)
		   )))))

(defmethod ein:cell-to-json ((cell ein:headingcell) &optional discard-output)
  (let ((json (call-next-method)))
    (append json `((level . ,(oref cell :level))))))

;;; Dealing with Code cell outputs
(defun ein:cell-stream-output-to-json (output)
  `((output_type . "stream")
    (name . ,(plist-get output :stream))
    (text . ,(plist-get output :text))))

(defun ein:cell-error-output-to-json (output)
  `((output_type . "error")
    (ename . ,(plist-get output :ename))
    (evalue . ,(plist-get output :evalue))
    (traceback . ,(plist-get output :traceback))))

(defun ein:cell-execute-result-output-to-json (output)
  (let ((data (ein:aif (plist-get output :text)
		  `("text/plain" . ,it)
		(plist-get output :data))))
    `((output_type . "execute_result")
      (metadata . ,(make-hash-table))
      (execution_count . ,(or (plist-get output :prompt_number)
			      (plist-get output :execution_count)))
      (data . (,data)))))

(defun ein:maybe-get-output-mime-data (output)
  (loop for type in '(:svg :png :jpeg :html :latex  :javascript :text)
	if (plist-get output type)
	collecting (cons (ein:output-property type) (plist-get output type))))

(defun ein:cell-display-data-output-to-json (output)
  (let ((data (or (ein:maybe-get-output-mime-data output)
		  (plist-get output :data))))
    `((output_type . "display_data")
      (data . ,data)
      (metadata . ,(make-hash-table)))))

(defun ein:find-and-make-outputs (output-plist)
  (loop for prop in ein:output-type-map
	when (plist-get output-plist (cdr prop))
	collect (list (cdr prop) (plist-get output-plist (cdr prop)))))

(provide 'ein-cell-output)

