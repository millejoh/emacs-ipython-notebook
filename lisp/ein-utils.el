;;; ein-utils.el --- Utility module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-utils.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-utils.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-utils.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)

(defgroup ein nil
  "IPython notebook client in Emacs"
  :group 'applications
  :prefix "ein:")


;;; Configuration

(defcustom ein:url-or-port '(8888)
  "List of default url-or-port values.
This will be used for completion. So put your IPython servers.
You can connect to servers not in this list \(but you will need
to type every time)."
  :type '(repeat (choice (integer :tag "Port number" 8888)
                         (string :tag "URL" "http://127.0.0.1:8888")))
  :group 'ein)

(defcustom ein:default-url-or-port nil
  "Default URL or port.  This should be your main IPython
Notebook server."
  :type '(choice (integer :tag "Port number" 8888)
                 (string :tag "URL" "http://127.0.0.1:8888")
                 (const :tag "First value of `ein:url-or-port'" nil))
  :group 'ein)

(defcustom ein:scratch-notebook-name-template "_scratch_%Y-%m-%d-%H%M%S_"
  "Template of scratch notebook name.
This value is used from `ein:notebooklist-new-scratch-notebook'
and `ein:notebook-rename-to-scratch-command'.  This must be a
format string which can be passed to `format-time-string'."
  :type '(string :tag "Format string")
  :group 'ein)

(defcustom ein:filename-translations nil
  "Convert file paths between Emacs and Python process.

This value can take these form:

alist
    Its key specifies URL-OR-PORT and value must be a list of two
    functions: (TO-PYTHON FROM-PYTHON).  Key (URL-OR-PORT) can be
    string (URL), integer (port), or `default' (symbol).  The
    value of `default' is used when other key does not much.
function
    Called with an argument URL-OR-PORT (integer or string).
    This function must return a list of two functions:
    (TO-PYTHON FROM-PYTHON).

Here, the functions TO-PYTHON and FROM-PYTHON are defined as:

TO-PYTHON
    A function which converts a file name (returned by
    `buffer-file-name') to the one Python understands.
FROM-PYTHON
    A function which converts a file path returned by
    Python process to the one Emacs understands."
  ;; I've got the idea from `slime-filename-translations'.
  :type '(choice
          (alist :tag "Translations mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (list (function :tag "TO-PYTHON")
                                   (function :tag "FROM-PYTHON")))
          (function :tag "Translations getter"))
  :group 'ein)


;;; Macros and core functions/variables

(defun ein:default-url-or-port ()
  (or ein:default-url-or-port (car ein:url-or-port) 8888))

(defun ein:scratch-notebook-name ()
  "Generate new scratch notebook name based on `current-time' and
`ein:scratch-notebook-name-template'."
  (format-time-string ein:scratch-notebook-name-template (current-time)))

(defvar ein:source-dir (file-name-directory load-file-name))


(defmacro ein:aif (test-form then-form &rest else-forms)
  "Anaphoric IF.  Adapted from `e2wm:aif'."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'ein:aif 'lisp-indent-function 2)

(defmacro ein:aand (test &rest rest)
  "Anaphoric AND.  Adapted from `e2wm:aand'."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(ein:aand ,@rest)) 'it))))


(defmacro ein:deflocal (name &optional initvalue docstring)
  "Define permanent buffer local variable named NAME.
INITVALUE and DOCSTRING are passed to `defvar'."
  (declare (indent defun)
           (doc-string 3))
  `(progn
     (defvar ,name ,initvalue ,docstring)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defmacro ein:with-read-only-buffer (buffer &rest body)
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (setq buffer-read-only t)
     (save-excursion
       (let ((inhibit-read-only t))
         ,@body))))

(defvar ein:dotty-syntax-table
  (let ((table (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Adapted from `python-dotty-syntax-table'.")

(defun ein:object-at-point ()
  "Return dotty.words.at.point.
When region is active, text in region is returned after trimmed
white spaces, newlines and dots.
When object is not found at the point, return the object just
before previous opening parenthesis."
  ;; For auto popup tooltip (or something like eldoc), probably it is
  ;; better to return function (any word before "(").  I should write
  ;; another function or add option to this function when the auto
  ;; popup tooltip is implemented.
  (if (region-active-p)
      (ein:trim (buffer-substring (region-beginning) (region-end))
                "\\s-\\|\n\\|\\.")
    (save-excursion
      (with-syntax-table ein:dotty-syntax-table
        (ein:aif (thing-at-point 'word)
            it
          (unless (looking-at "(")
            (search-backward "(" (point-at-bol) t))
          (thing-at-point 'word))))))


;;; URL utils

(defvar ein:url-localhost-template "http://127.0.0.1:%s")

(defun ein:url (url-or-port &rest paths)
  (loop with url = (if (integerp url-or-port)
                       (format ein:url-localhost-template url-or-port)
                     url-or-port)
        for p in paths
        do (setq url (concat (ein:trim-right url "/")
                             "/"
                             (ein:trim-left p "/")))
        finally return url))

(defun ein:url-no-cache (url)
  "Imitate `cache=false' of `jQuery.ajax'.
See: http://api.jquery.com/jQuery.ajax/"
  (concat url (format-time-string "?_=%s")))


;;; JSON utils

(defmacro ein:with-json-setting (&rest body)
  `(let ((json-object-type 'plist)
         (json-array-type 'list))
     ,@body))

(defun ein:json-read ()
  "Read json from `url-retrieve'-ed buffer.

* `json-object-type' is `plist'. This is mainly for readability.
* `json-array-type' is `list'.  Notebook data is edited locally thus
  data type must be edit-friendly.  `vector' type is not."
  (goto-char (point-max))
  (backward-sexp)
  (ein:with-json-setting
   (json-read)))

(defun ein:json-read-from-string (string)
  (ein:with-json-setting
   (json-read-from-string string)))


;;; Text property

(defun ein:propertize-read-only (string &rest properties)
  (apply #'propertize string 'read-only t 'front-sticky t properties))

(defun ein:insert-read-only (string &rest properties)
  (insert (apply #'ein:propertize-read-only string properties)))


;;; String manipulation

(defun ein:trim (string &optional regexp)
  (ein:trim-left (ein:trim-right string regexp) regexp))

(defun ein:trim-left (string &optional regexp)
  (unless regexp (setq regexp "\\s-\\|\n"))
  (ein:trim-regexp string (format "^\\(%s\\)+" regexp)))

(defun ein:trim-right (string &optional regexp)
  (unless regexp (setq regexp "\\s-\\|\n"))
  (ein:trim-regexp string (format "\\(%s\\)+$" regexp)))

(defun ein:trim-regexp (string regexp)
  (if (string-match regexp string)
      (replace-match "" t t string)
    string))

(defun ein:trim-indent (string)
  "Strip uniform amount of indentation from lines in STRING."
  (let* ((lines (split-string string "\n"))
         (indent
          (let ((lens
                 (loop for line in lines
                       for stripped = (ein:trim-left line)
                       unless (equal stripped "")
                       collect (- (length line) (length stripped)))))
            (if lens (apply #'ein:min lens) 0)))
         (trimmed
          (loop for line in lines
                if (> (length line) indent)
                collect (ein:trim-right (substring line indent))
                else
                collect line)))
    (ein:join-str "\n" trimmed)))

(defun ein:join-str (sep strings)
  (mapconcat 'identity strings sep))

(defun ein:join-path (paths)
  (mapconcat 'file-name-as-directory paths ""))

(defun ein:string-fill-paragraph (string &optional justify)
  (with-temp-buffer
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (fill-paragraph justify)
    (buffer-string)))

(defmacro ein:case-equal (str &rest clauses)
  "Similar to `case' but comparison is done by `equal'.
Adapted from twittering-mode.el's `case-string'."
  (declare (indent 1))
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))


;;; Misc

(defun ein:plist-iter (plist)
  "Return list of (key . value) in PLIST."
  (loop for p in plist
        for i from 0
        for key-p = (= (% i 2) 0)
        with key = nil
        if key-p do (setq key p)
        else collect `(,key . ,p)))

(defun ein:hash-keys (table)
  (let (keys)
    (maphash (lambda (k v) (push k keys)) table)
    keys))

(defun ein:hash-vals (table)
  (let (vals)
    (maphash (lambda (k v) (push v vals)) table)
    vals))

(defun ein:filter (predicate sequence)
  (loop for item in sequence
        when (funcall predicate item)
        collect item))

(defun ein:clip-list (list first last)
  "Return elements in region of the LIST specified by FIRST and LAST element.

Example::

    (ein:clip-list '(1 2 3 4 5 6) 2 4)  ;=> (2 3 4)"
  (loop for elem in list
        with clipped
        with in-region-p = nil
        when (eq elem first)
        do (setq in-region-p t)
        when in-region-p
        do (push elem clipped)
        when (eq elem last)
        return (reverse clipped)))

(defun ein:get-value (obj)
  "Get value from obj if it is a variable or function."
  (cond
   ((not (symbolp obj)) obj)
   ((boundp obj) (eval obj))
   ((fboundp obj) (funcall obj))))

(defun ein:choose-setting (symbol value)
  "Choose setting in stored in SYMBOL based on VALUE.
The value of SYMBOL can be string, alist or function."
  (let ((setting (eval symbol)))
    (cond
     ((stringp setting) setting)
     ((functionp setting) (funcall setting value))
     ((listp setting)
      (ein:get-value (or (assoc-default value setting)
                         (assoc-default 'default setting))))
     (t (error "Unsupported type of `%s': %s" symbol (type-of setting))))))

(defmacro ein:setf-default (place val)
  "Set VAL to PLACE using `setf' if the value of PLACE is `nil'."
  `(unless ,place
     (setf ,place ,val)))

(defun ein:funcall-packed (func-arg &rest args)
  "Call \"packed\" function.
FUNC-ARG is a `cons' of the form: (FUNC ARG).
FUNC is called as (apply FUNC ARG ARGS)."
  (apply (car func-arg) (cdr func-arg) args))

(defun ein:eval-if-bound (symbol)
  (if (boundp symbol) (eval symbol)))

(defun ein:remove-by-index (list indices)
  "Remove elements from LIST if its index is in INDICES.
NOTE: This function creates new list."
  (loop for l in list
        for i from 0
        when (not (memq i indices))
        collect l))

(defun ein:min (x &rest xs)
  (loop for y in xs if (< y x) do (setq x y))
  x)

(defun ein:do-nothing (&rest -ignore-)
  "A function which can take any number of variables and do nothing.")

(defun ein:ask-choice-char (prompt choices)
  "Show PROMPT and read one of acceptable key specified as CHOICES."
  (let ((char-list (loop for i from 0 below (length choices)
                         collect (elt choices i)))
        (answer 'recenter))
    (while
        (let ((key
               (let ((cursor-in-echo-area t))
                 (read-key (propertize (if (eq answer 'recenter)
                                           prompt
                                         (concat "Please choose answer from"
                                                 (format " %s.  " choices)
                                                 prompt))
                                       'face 'minibuffer-prompt)))))
          (setq answer (lookup-key query-replace-map (vector key) t))
          (cond
           ((memq key char-list) (setq answer key) nil)
           ((eq answer 'recenter) (recenter) t)
           ((memq answer '(exit-prefix quit)) (signal 'quit nil) t)
           (t t)))
      (ding)
      (discard-input))
    answer))


(defun ein:truncate-lines-on ()
  "Set `truncate-lines' on (set it to `t')."
  (setq truncate-lines t))

(defun ein:byte-compile-ein ()
  "Byte compile EIN files."
  (interactive)
  (let* ((files (directory-files ein:source-dir 'full "^ein-.*\\.el$"))
         (errors (ein:filter
                  'identity
                  (mapcar (lambda (f) (unless (byte-compile-file f) f))
                          files))))
    (ein:aif errors
        (error "Got %s errors while compiling these files: %s"
               (length errors)
               (ein:join-str " " (mapcar #'file-name-nondirectory it))))
    (message "Compiled %s files" (length files))))


;;; File name translation

(defun ein:filename-translations-get (url-or-port)
  (ein:choose-setting 'ein:filename-translations url-or-port))

(defun ein:filename-to-python (filename url-or-port)
  (ein:aif (car (ein:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein:filename-from-python (filename url-or-port)
  (ein:aif (cadr (ein:filename-translations-get url-or-port))
      (funcall it filename)
    filename))


;;; utils.js compatible

(defun ein:utils-uuid ()
  "Return string with random (version 4) UUID.
Adapted from org-mode's `org-id-uuid'."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random t)
			  (current-time)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))


(provide 'ein-utils)

;;; ein-utils.el ends here
