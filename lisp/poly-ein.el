;;; poly-ein.el --- polymode for EIN    -*- lexical-binding:t -*-

;; Copyright (C) 2019- The Authors

;; This file is NOT part of GNU Emacs.

;; poly-ein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; poly-ein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with poly-ein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'polymode)
(require 'ein-cell)
(require 'jit-lock)
(require 'quail)
(require 'display-line-numbers nil t)
(require 'undo-tree nil t)

(declare-function ein:get-notebook "ein-notebook")
(declare-function ein:notebook-mode "ein-notebook")

(declare-function polymode-inhibit-during-initialization "polymode-core")

(defmacro poly-ein--remove-hook (label functions)
  "Remove any hooks saying LABEL from FUNCTIONS"
  `(mapc (lambda (x) (when (and (symbolp x) (cl-search ,label (symbol-name x)))
                       (remove-hook (quote ,functions) x t)))
         ,functions))

(defun poly-ein--narrow-to-inner (modifier f &rest args)
  (if (or pm-initialization-in-progress (not poly-ein-mode))
      (apply f args)
    (save-restriction
      (widen)
      (let ((range (pm-innermost-range
                    (or (if (numberp (car args))
                            (max (funcall modifier (car args)) (point-min)))
                        (point)))))
        (narrow-to-region (car range) (cdr range))
        (apply f args)))))

(defun poly-ein--decorate-functions ()
  "Affect global definitions of ppss and jit-lock rather intrusively."
  (mapc (lambda (fun)
          (dolist (adv (list 'poly-lock-no-jit-lock-in-polymode-buffers
                             'polymode-inhibit-during-initialization))
            (when (advice-member-p adv fun)
              ;; must set log level at toplevel to show following
              (ein:log 'debug "poly-ein--decorate-functions: removing %s from %s"
                       adv fun)
              (advice-remove fun adv))))
        (list 'jit-lock-mode
              'font-lock-fontify-region
              'font-lock-fontify-buffer
              'font-lock-ensure))

  ;; https://github.com/millejoh/emacs-ipython-notebook/issues/537
  ;; alternatively, filter-args on ad-should-compile but then we'd have to
  ;; match on function name
  (custom-set-default 'ad-default-compilation-action 'never)

  (add-function
   :before-until (symbol-function 'pm-select-buffer)
   (lambda (span &optional visibly)
     (prog1 poly-ein-mode
       (when poly-ein-mode
         (let ((src-buf (current-buffer))
               (dest-buf (pm-span-buffer span)))
           (unless (eq src-buf dest-buf)
             (poly-ein-set-buffer src-buf dest-buf visibly)))))))

  (fmakunbound 'poly-lock-mode)
  (defalias 'poly-lock-mode (symbol-function (default-value 'font-lock-function)))

  (defun poly-ein--syntax-propertize (pos)
    (prog1 poly-ein-mode
      (when (and poly-ein-mode (< syntax-propertize--done pos))
        (save-excursion
          ;; pared down from default `syntax-propertize'
          (with-silent-modifications
            (let ((parse-sexp-lookup-properties t)
                  (start (point-min)) ;; i've narrowed in the :around
                  (end (point-max))
                  (span (pm-innermost-span pos)))
              (setq syntax-propertize--done end)
              (when (eq 'body (nth 0 span))
                (remove-text-properties start end
                                        '(syntax-table nil syntax-multiline nil))
                ;; avoid recursion if syntax-propertize-function calls me (syntax-propertize)
                (when syntax-propertize-function
                  (let ((syntax-propertize--done most-positive-fixnum))
                    (funcall syntax-propertize-function start end))))))))))
  (add-function
   :before-until (symbol-function 'syntax-propertize)
   #'poly-ein--syntax-propertize)

  (add-function
   :around (symbol-function 'syntax-propertize)
   (apply-partially #'poly-ein--narrow-to-inner #'identity))

  (add-function
   :around (symbol-function 'syntax-ppss)
   (apply-partially #'poly-ein--narrow-to-inner #'identity))

  (add-function
   :around (symbol-function 'pm--mode-setup)
   (lambda (f &rest args)
     ;; global-font-lock-mode will call an after-change-mode-hook
     ;; that calls font-lock-initial-fontify, which fontifies the entire buffer!
     (cl-letf (((symbol-function 'global-font-lock-mode-enable-in-buffers) #'ignore))
       (when-let (b (or (cl-second args) (current-buffer)))
         (with-current-buffer b
           (unless (eq font-lock-support-mode 'jit-lock-mode)
             (ein:log 'info "pm--mode-setup: deactivating %s in %s"
                      font-lock-support-mode (buffer-name))
             (setq-local font-lock-support-mode 'jit-lock-mode))))
       (apply f args))))

  (add-function
   :around (symbol-function 'pm--common-setup)
   (lambda (f &rest args)
     "somewhere between pm--mode-setup and pm--common-setup is a
      kill-all-local-variables that douses any early attempt at
      overriding font-lock-support-mode."
     (when-let (b (or (cl-second args) (current-buffer)))
       (with-current-buffer b
         (unless (eq font-lock-support-mode 'jit-lock-mode)
           (ein:log 'info "pm--common-setup: deactivating %s in %s"
                    font-lock-support-mode (buffer-name))
           (setq-local font-lock-support-mode 'jit-lock-mode))))
     (apply f args)))

  (add-function
   :around (symbol-function 'jit-lock-mode)
   (lambda (f &rest args)
     ;; Override jit-lock.el.gz deliberately skipping indirect buffers
     (cl-letf (((symbol-function 'buffer-base-buffer) #'ignore)) (apply f args))))

  ;; :before-until before :filter-args (reversed order when executed)

  (add-function :before-until (symbol-function 'jit-lock-refontify)
                #'poly-ein--unrelated-span)

  (add-function :before-until (symbol-function 'jit-lock-fontify-now)
                #'poly-ein--unrelated-span)

  (add-function :filter-args (symbol-function 'jit-lock-refontify)
                #'poly-ein--span-start-end)

  (add-function :filter-args (symbol-function 'jit-lock-fontify-now)
                #'poly-ein--span-start-end)

  (add-function :filter-args (symbol-function 'font-lock-flush)
                #'poly-ein--span-start-end)

  (add-function :filter-args (symbol-function 'jit-lock-after-change)
                #'poly-ein--span-start-end)

  (add-function :before-until
                (symbol-function 'pm--synchronize-points)
                (lambda (&rest _args) poly-ein-mode))

  (let ((dont-lookup-props
         (lambda (f &rest args)
           (let ((parse-sexp-lookup-properties (if poly-ein-mode
                                                   nil
                                                 parse-sexp-lookup-properties)))
             (apply f args)))))
    (add-function :around (symbol-function 'scan-lists) dont-lookup-props)
    (add-function :around (symbol-function 'scan-sexps) dont-lookup-props))

  (advice-add 'other-buffer
	      :filter-args
	      (lambda (args)
		"Avoid switching to indirect buffers."
		(if poly-ein-mode
		    (cons (or (buffer-base-buffer (car args)) (car args))
			  (cdr args))
		  args)))

  (let* ((unadorned (symbol-function 'isearch-done))
         (after-isearch-done
          (lambda (&rest _args)
            "Clear `isearch-mode' for all base and indirect buffers."
            (-when-let* ((poly-ein-mode-p poly-ein-mode)
                         (notebook (ein:get-notebook))
                         (buffers (cl-remove-if (apply-partially #'string= (buffer-name))
                                                (ein:notebook-buffer-list notebook))))
              ;; could just call unadorned, but what if `isearch-done' calls itself?
              (cl-letf (((symbol-function 'isearch-done) unadorned))
                (mapc (lambda (b) (with-current-buffer b (isearch-done))) buffers))))))
    (add-function :after (symbol-function 'isearch-done) after-isearch-done)))

(defmacro poly-ein-base (&rest body)
  "Copy the undo accounting to the base buffer and run BODY in it.
This is a bottleneck as we do this on every `pm-get-span'."
  `(let ((base-buffer (pm-base-buffer))
         (derived-buffer (current-buffer))
         (pm-allow-post-command-hook nil)
         (quail (aand (overlayp quail-overlay)
                      (overlay-start quail-overlay)
                      (list it (overlay-end quail-overlay))))
         (quail-conv (aand (overlayp quail-conv-overlay)
                           (overlay-start quail-conv-overlay)
                           (list it (overlay-end quail-conv-overlay)))))
     (poly-ein-set-buffer derived-buffer base-buffer)
     (unwind-protect
	 (cl-letf (((symbol-function 'poly-ein--copy-state) #'ignore))
           ,@body)
       (save-current-buffer
	 (with-current-buffer derived-buffer
	   (poly-ein-set-buffer base-buffer derived-buffer)
	   (when quail
             (apply #'move-overlay quail-overlay quail))
	   (when quail-conv
             (apply #'move-overlay quail-conv-overlay quail-conv)))))))

(defclass pm-inner-overlay-chunkmode (pm-inner-auto-chunkmode)
  ()
  "Inner chunkmode delimited by cell overlays.")

(cl-defmethod pm-get-span ((cm pm-inner-overlay-chunkmode) &optional pos)
  "Return a list of the form (TYPE POS-START POS-END RESULT-CM).

TYPE can be \\='body, nil."
  (poly-ein-base
   (setq pos (or pos (point)))
   (when-let ((result-cm cm)
              (span `(nil ,(point-min) ,(point-min)))
              (cell (ein:worksheet-get-current-cell :pos pos :noerror t)))
     ;; Change :mode if necessary
     (-when-let* ((nb (ein:get-notebook))
                  (lang
                   (condition-case err
                       (ein:$kernelspec-language
                        (ein:$notebook-kernelspec nb))
                     (error (message "%s: defaulting language to python"
                                     (error-message-string err))
                            "python")))
                  (what (cond ((ein:codecell-p cell) lang)
                              ((ein:markdowncell-p cell) "ein:markdown")
                              (t "fundamental")))
                  (mode (pm-get-mode-symbol-from-name what))
                  (f (not (equal mode (ein:oref-safe cm 'mode)))))
       (when (eq mode 'poly-fallback-mode)
         (let ((warning (format (concat "pm-get-span: Add (%s . [mode-prefix]) to "
                                        "polymode-mode-name-aliases")
                                what)))
           (when (or (not (get-buffer "*Warnings*"))
                     (not (with-current-buffer "*Warnings*"
                            (save-excursion
                              (goto-char (point-min))
                              (re-search-forward (regexp-quote warning) nil t)))))
             (ein:display-warning warning))))
       (setq result-cm
             (cl-loop for ocm in (eieio-oref pm/polymode '-auto-innermodes)
                      when (equal mode (ein:oref-safe ocm 'mode))
                      return ocm
                      finally return (let ((new-mode (clone cm :mode mode)))
                                       (object-add-to-list pm/polymode '-auto-innermodes
                                                           new-mode)
                                       new-mode))))
     ;; Span is a zebra pattern of "body" (within input cell) and "nil"
     ;; (outside input cell).  Decide boundaries of span and return it.
     (let ((rel (poly-ein--relative-to-input pos cell)))
       (cond ((zerop rel)
              (setq span `(body
                           ,(ein:cell-input-pos-min cell)
                           ,(1+ (ein:cell-input-pos-max cell)))))
             ((< rel 0)
              (setq span `(nil
                           ,(or (ein:aand (ein:cell-prev cell)
                                          (1+ (ein:cell-input-pos-max it)))
                                (point-min))
                           ,(ein:cell-input-pos-min cell))))
             (t
              (setq span `(nil
                           ,(1+ (ein:cell-input-pos-max cell))
                           ,(or (ein:aand (ein:cell-next cell)
                                          (ein:cell-input-pos-min it))
                                (point-max)))))))
     (append span (list result-cm)))))

(defun poly-ein-fontify-buffer (buffer)
  "Called from `ein:notebook--worksheet-render'"
  (with-current-buffer buffer
    (save-excursion
      (pm-map-over-spans
       (lambda (span)
         (with-current-buffer (pm-span-buffer span)
           (cl-assert (eq font-lock-function 'poly-lock-mode))
           (ignore-errors (jit-lock-function (nth 1 span)))))))))

(defun poly-ein--relative-to-input (pos cell)
  "Return -1 if POS before input, 1 if after input, 0 if within"
  (let* ((input-pos-min (ein:cell-input-pos-min cell))
         (input-pos-max (ein:cell-input-pos-max cell)))
    (cond ((< pos input-pos-min) -1)
          ((> pos input-pos-max) 1)
          (t 0))))

(defvar jit-lock-start)
(defvar jit-lock-end)
(defun poly-ein--hem-jit-lock (start _end _old-len)
  (when (and poly-ein-mode (not pm-initialization-in-progress))
    (let ((range (pm-innermost-range (or start (point)))))
      (setq jit-lock-start (max jit-lock-start (car range)))
      (setq jit-lock-end (min jit-lock-end (cdr range))))))

(defun poly-ein-initialize (type)
  (poly-ein--remove-hook "polymode" after-change-functions)
  (poly-ein--remove-hook "polymode" syntax-propertize-extend-region-functions)
  (add-hook 'jit-lock-after-change-extend-region-functions #'poly-ein--hem-jit-lock t t)
  (setq jit-lock-contextually nil) ; else recenter font-lock-fontify-keywords-region
  (setq jit-lock-context-unfontify-pos nil)
  (when (ein:eval-if-bound 'display-line-numbers-mode)
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode -1)))
  (when (ein:eval-if-bound 'linum-mode)
    (when (fboundp 'linum-mode)
      (linum-mode -1)))
  (when (ein:eval-if-bound 'undo-tree-mode)
    (when (fboundp 'undo-tree-mode)
      (undo-tree-mode -1)))
  (when visual-line-mode
    (visual-line-mode -1))
  (if (eq type 'host)
      (setq syntax-propertize-function nil)
    (aif pm--syntax-propertize-function-original
        (progn
          (setq syntax-propertize-function it)
          (add-function :before-until (local 'syntax-propertize-function)
                        #'poly-ein--unrelated-span)
          (add-function :filter-args (local 'syntax-propertize-function)
                        #'poly-ein--span-start-end)))
    (add-function :around (local 'font-lock-syntactic-face-function)
                  (apply-partially #'poly-ein--narrow-to-inner #'identity))))

(defun poly-ein--record-window-buffer ()
  "(pm--visible-buffer-name) needs to get onto window's prev-buffers.
But `C-x b` seems to consult `buffer-list' and not the C (window)->prev_buffers."
  (when (buffer-base-buffer)
    (let* ((buffer-list (frame-parameter nil 'buffer-list))
           (pos-visible (seq-position
                         buffer-list
                         (pm--visible-buffer-name)
                         (lambda (x visible*)
                           (string-prefix-p (buffer-name x) visible*)))))
      ;; no way to know if i've switched in or out of indirect buf.
      ;; (if in, I *don't* want to add visible to buffer-list)
      (cond ((and (numberp pos-visible) (> pos-visible 0))
             (let ((visible-buffer (nth pos-visible buffer-list)))
               (setcdr (nthcdr (1- pos-visible) buffer-list)
                       (nthcdr (1+ pos-visible) buffer-list))
               (set-frame-parameter nil 'buffer-list (cons visible-buffer buffer-list))))
            ((null pos-visible)
             (set-frame-parameter nil 'buffer-list
                                  (cons (buffer-base-buffer) buffer-list)))))))

(defun poly-ein-init-input-cell (_type)
  "Contrary to intuition, this inits the entire buffer of input cells
(collectively denoted by the chunkmode pm-inner/ein-input-cell), not each individual one."
  (mapc (lambda (f) (add-hook 'after-change-functions f t t))
        (buffer-local-value 'after-change-functions (pm-base-buffer)))
  (setq-local font-lock-dont-widen t)
  (setq-local syntax-propertize-chunks 0) ;; internal--syntax-propertize too far
  (add-hook 'buffer-list-update-hook #'poly-ein--record-window-buffer nil t)
  (add-hook 'ido-make-buffer-list-hook
	    (lambda ()
	      (defvar ido-temp-list)
	      (when-let ((visible (pm--visible-buffer-name)))
		(ido-to-end (delq nil
				  (mapcar (lambda (x)
					    (when (string-prefix-p x visible) x))
					  ido-temp-list)))))
	    nil t)
  (ein:notebook-mode)
  (unless (eq 'ein:notebook-mode (caar minor-mode-map-alist))
    ;; move `ein:notebook-mode' to the head of `minor-mode-map-alist'
    (when-let ((entry (assq 'ein:notebook-mode minor-mode-map-alist)))
      (setf minor-mode-map-alist
	    (cons entry
		  (assq-delete-all 'ein:notebook-mode minor-mode-map-alist))))))

(defcustom pm-host/ein
  (pm-host-chunkmode :name "ein"
                     :init-functions '(poly-ein-initialize))
  "EIN host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-inner/ein-input-cell
  (pm-inner-overlay-chunkmode :name "ein-input-cell"
                              :init-functions '(poly-ein-initialize poly-ein-init-input-cell))
  "EIN input cell."
  :group 'poly-innermodes
  :type 'object)

(defcustom poly-ein-mode-hook nil
  "Hook for poly-ein-mode"
  :type 'hook :group 'poly-ein)

(unless (fboundp 'with-suppressed-warnings)
  (defmacro with-suppressed-warnings (warnings &rest body)
    `(progn (ignore ',warnings) ,@body)))

;;;###autoload (autoload 'poly-ein-mode "poly-ein")
(with-suppressed-warnings ((obsolete easy-mmode-define-keymap))
  (define-polymode poly-ein-mode
		   :lighter " PM-ipynb"
		   :hostmode 'pm-host/ein
		   :innermodes '(pm-inner/ein-input-cell)))

(defun poly-ein--copy-state (src-buf dest-buf)
  "Dangerous to call this outside `poly-ein-set-buffer' (loses overlays)."
  (unless (eq src-buf dest-buf)
    (dolist (b (eieio-oref pm/polymode '-buffers))
      (unless (eq b dest-buf)
	(with-current-buffer b
	  (save-excursion
            (save-restriction
	      (widen)
	      (dolist (ol (overlays-in (point-min) (point-max)))
		(move-overlay ol (overlay-start ol) (overlay-end ol) dest-buf)))))))
    (pm--move-vars (append ein:local-variables
                           '(header-line-format buffer-undo-list isearch-mode))
                   src-buf dest-buf)))

(defun poly-ein-set-buffer (src-buf dest-buf &optional switch)
  (let ((pm-initialization-in-progress t))
    (when (and (not (eq src-buf dest-buf))
               (buffer-live-p src-buf)
               (buffer-live-p dest-buf))
      (cl-destructuring-bind (point window-start region-begin pos-visible _)
          (with-current-buffer src-buf (list (point)
                                             (window-start)
                                             (and switch (region-active-p) (mark))
                                             (pos-visible-in-window-p)
                                             (when switch (deactivate-mark))))
        (poly-ein--copy-state src-buf dest-buf)
        (if switch
            (switch-to-buffer dest-buf)
          (set-buffer dest-buf))
        (when region-begin
          (setq deactivate-mark nil) ;; someone is setting this, I don't know who
          (push-mark region-begin t t))
        (goto-char point)
        (setq syntax-propertize--done (point-min))
        (when switch
          (when pos-visible
            (set-window-start (get-buffer-window) window-start))
          (bury-buffer-internal src-buf)
          (set-window-prev-buffers
           nil
           (assq-delete-all src-buf (window-prev-buffers nil)))
          (run-hook-with-args 'polymode-switch-buffer-hook src-buf dest-buf)
          (pm--run-hooks pm/polymode :switch-buffer-functions src-buf dest-buf)
          (pm--run-hooks pm/chunkmode :switch-buffer-functions src-buf dest-buf))))))

(defsubst poly-ein--span-start-end (args)
  (if (or pm-initialization-in-progress (not poly-ein-mode))
      args
    (let* ((span-start (cl-first args))
           (span-end (cl-second args))
           (range (pm-innermost-range (or span-start (point)))))
      (setq span-start (max (or span-start (car range)) (car range)))
      (setq span-end (min (or span-end (cdr range)) (cdr range)))
      (append (list span-start span-end) (cddr args)))))

(defsubst poly-ein--unrelated-span (&optional beg _end)
  (or pm-initialization-in-progress
      (and poly-ein-mode
           (let* ((span (pm-innermost-span (or beg (point))))
                  (span-mode (eieio-oref (nth 3 span) 'mode)))
             ;; only fontify type 'body (the other type is nil)
             (or (null (nth 0 span)) (not (eq major-mode span-mode)))))))

(make-variable-buffer-local 'parse-sexp-lookup-properties)

(poly-ein--decorate-functions)

(provide 'poly-ein)
