(add-to-list 'features 'poly-lock) ;; skirt poly-lock
(require 'polymode)
(require 'ein-cell)
(require 'jit-lock)

(defmacro poly-ein-base (&rest body)
  "Copy the undo accounting to the base buffer and run BODY in it."
  `(let ((base-buffer (pm-base-buffer))
         (derived-buffer (current-buffer))
         (pm-allow-post-command-hook nil)
         (pm-initialization-in-progress t))
     (poly-ein--set-buffer derived-buffer base-buffer)
     (condition-case err
         (prog1 (progn ,@body)
           (poly-ein--set-buffer base-buffer derived-buffer))
       (error (message "%s" (error-message-string err))
              (poly-ein--set-buffer base-buffer derived-buffer)))))

(defcustom ein:polymode nil
  "Turn off hacky major mode emulations, turn on polymode."
  :type 'boolean
  :group 'ein)

(defclass pm-inner-overlay-chunkmode (pm-inner-auto-chunkmode)
  ()
  "Inner chunkmode delimited by cell overlays.")

(cl-defmethod pm-get-span ((cm pm-inner-overlay-chunkmode) &optional pos)
  "Return a list of the form (TYPE POS-START POS-END RESULT-CM).

TYPE can be 'body, nil."
  (poly-ein-base
   (setq pos (or pos (point)))
   ;; Assume: ein:worksheet-get-current-cell always returns non-nil
   (let ((result-cm cm)
         (span `(nil ,(point-min) ,(point-min)))
         (cell (ein:worksheet-get-current-cell :pos pos :noerror nil)))
     ;; Change :mode if necessary
     (ein:and-let* ((lang
                     (condition-case err
                         (ein:$kernelspec-language
                          (ein:$notebook-kernelspec
                           (ein:get-notebook)))
                       (error (message "%s: defaulting language to python"
                                       (error-message-string err))
                              "python")))
                    (mode
                     (pm-get-mode-symbol-from-name
                      (cond ((ein:codecell-p cell) lang)
                            ((ein:markdowncell-p cell) "markdown")
                            (t "fundamental"))))
                    ((not (equal mode (ein:oref-safe cm :mode)))))
       (setq result-cm
             (loop for ocm in (eieio-oref pm/polymode '-auto-innermodes)
                   when (equal mode (ein:oref-safe ocm :mode))
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

(defun poly-ein-fontify-buffer (notebook)
  "Called from `ein:notebook--worksheet-render'"
  (with-current-buffer (ein:notebook-buffer notebook)
    (save-excursion
      (pm-map-over-spans
       (lambda (span)
         (condition-case err
             (let ((syntax-propertize--done (point-min)))
               (jit-lock-function (nth 1 span)))
           (error (ein:log 'warn "ein:notebook--worksheet-render: %s"
                           (error-message-string err)))))))))

(defun poly-ein--relative-to-input (pos cell)
  "Return -1 if POS before input, 1 if after input, 0 if within"
  (let* ((input-pos-min (ein:cell-input-pos-min cell))
         (input-pos-max (ein:cell-input-pos-max cell)))
    (cond ((< pos input-pos-min) -1)
          ((> pos input-pos-max) 1)
          (t 0))))

(defvar jit-lock-start)
(defvar jit-lock-end)
(defun poly-ein--hem-jit-lock (start end _old-len)
  (when (and poly-ein-mode (not pm-initialization-in-progress))
    (let ((range (pm-innermost-range (or start (point)))))
      (setq jit-lock-start (max jit-lock-start (car range)))
      (setq jit-lock-end (min jit-lock-end (cdr range))))))

(defun poly-ein-undo-damage (type)
  (remove-hook 'after-change-functions 'polymode-flush-syntax-ppss-cache t)
  (add-hook 'jit-lock-after-change-extend-region-functions #'poly-ein--hem-jit-lock t t)
  (setq jit-lock-contextually nil) ; else recenter font-lock-fontify-keywords-region
  (setq jit-lock-context-unfontify-pos nil)
  (if (eq type 'host)
      (setq syntax-propertize-function nil)
    (setq syntax-propertize-function pm--syntax-propertize-function-original)
    (add-function :before-until (local 'syntax-propertize-function)
                  #'poly-ein--unrelated-span)
    (add-function :filter-args (local 'syntax-propertize-function)
                  #'poly-ein--span-start-end)))

(defun poly-ein-init-input-cell (_type)
  (mapc (lambda (f) (add-to-list 'after-change-functions f))
        (buffer-local-value 'after-change-functions (pm-base-buffer)))
  (poly-ein-copy-state (pm-base-buffer) (current-buffer))
  (ein:notebook-mode))

(defcustom pm-host/ein
  (pm-host-chunkmode :name "ein"
                     :init-functions '(poly-ein-undo-damage))
  "EIN host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-inner/ein-input-cell
  (pm-inner-overlay-chunkmode :name "ein-input-cell"
                              :init-functions '(poly-ein-undo-damage poly-ein-init-input-cell))
  "EIN input cell."
  :group 'poly-innermodes
  :type 'object)

(defcustom poly-ein-mode-hook nil
  "Hook for poly-ein-mode"
  :type 'hook :group 'poly-ein)

;;;###autoload (autoload 'poly-ein-mode "poly-ein")
(define-polymode poly-ein-mode
  :lighter " PM-ipynb"
  :hostmode 'pm-host/ein
  :innermodes '(pm-inner/ein-input-cell))

(defun poly-ein-copy-state (src-buf dest-buf)
  "Consolidate fragility here."
  (unless (eq src-buf dest-buf)
    (with-current-buffer dest-buf (remove-overlays nil nil 'face 'ein:cell-input-area))
    (mapc (lambda (ol)
            (if (eq 'ein:cell-input-area (overlay-get ol 'face))
                (move-overlay (copy-overlay ol)
                              (overlay-start ol) (overlay-end ol)
                              dest-buf)))
          (with-current-buffer src-buf (overlays-in (point-min) (point-max))))
    (pm--move-vars (append ein:local-variables '(header-line-format buffer-undo-list))
                   src-buf dest-buf)))

(defsubst poly-ein--set-buffer (src-buf dest-buf &optional switch)
  (when (and (not (eq src-buf dest-buf))
             (buffer-live-p src-buf)
             (buffer-live-p dest-buf))
    (cl-destructuring-bind (point window-start region-begin pos-visible _)
        (with-current-buffer src-buf (list (point) (window-start)
                                           (and switch (region-active-p) (mark))
                                           (pos-visible-in-window-p)
                                           (when switch (deactivate-mark))))
      (poly-ein-copy-state src-buf dest-buf)
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
        (pm--run-hooks pm/chunkmode :switch-buffer-functions src-buf dest-buf)))))

(add-function
 :before-until (symbol-function 'pm-select-buffer)
 (lambda (span &optional visibly)
   (prog1 poly-ein-mode
     (when poly-ein-mode
       (let ((src-buf (current-buffer))
             (dest-buf (pm-span-buffer span)))
         ;; (font-lock-flush)
         (poly-ein--set-buffer src-buf dest-buf visibly))))))

(add-function
 :override (symbol-function 'poly-lock-mode)
 (symbol-function (default-value 'font-lock-function)))

(defun poly-ein--narrow-to-inner (modifier f &rest args)
  (if (or pm-initialization-in-progress (not poly-ein-mode))
      (apply f args)
    (save-restriction
      (widen)
      (let ((range (pm-innermost-range
                    (or (when (car args) (max (point-min) (funcall modifier (car args))))
                        (point)))))
        (narrow-to-region (car range) (cdr range))
        (apply f args)))))

(add-function
 :before-until (symbol-function 'syntax-propertize)
 (lambda (pos)
   (prog1 poly-ein-mode
     (when (and poly-ein-mode (< syntax-propertize--done pos))
       (save-excursion
         (with-silent-modifications
           (let ((parse-sexp-lookup-properties t)
                 (start (point-min))
                 (end (point-max)))
             ;; (dolist (fun syntax-propertize-extend-region-functions)
             ;;   (ein:and-let* ((new (funcall fun start end)))
             ;;     (setq start (min start (car new)))
             ;;     (setq end (max end (cdr new)))))
             (setq syntax-propertize--done end)
             (remove-text-properties start end
                                     '(syntax-table nil syntax-multiline nil))
             ;; avoid recursion if syntax-propertize-function calls me (syntax-propertize)
             (when syntax-propertize-function
               (let ((syntax-propertize--done most-positive-fixnum))
                 (funcall syntax-propertize-function start end))))))))))

(add-function
 :around (symbol-function 'syntax-propertize)
 (apply-partially #'poly-ein--narrow-to-inner #'1-))

(add-function
 :around (symbol-function 'syntax-ppss)
 (apply-partially #'poly-ein--narrow-to-inner #'1-))

(add-function
 :around (symbol-function 'jit-lock-mode)
 (lambda (f &rest args)
   (cl-letf (((symbol-function 'buffer-base-buffer) #'ignore)) (apply f args))))

(defsubst poly-ein--span-start-end (args)
  (if (or pm-initialization-in-progress (not poly-ein-mode))
      args
    (let* ((span-start (first args))
           (span-end (second args))
           (range (pm-innermost-range (or span-start (point)))))
      (setq span-start (max (or span-start (car range)) (car range)))
      (setq span-end (min (or span-end (cdr range)) (cdr range)))
      (append (list span-start span-end) (cddr args)))))

(defsubst poly-ein--unrelated-span (&optional beg end)
  (or pm-initialization-in-progress
      (and poly-ein-mode
           (not (eq major-mode
                    (eieio-oref (nth 3 (pm-innermost-span (or beg (point)))) :mode))))))

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

(make-variable-buffer-local 'parse-sexp-lookup-properties)
(with-eval-after-load "markdown-mode"
  (fset 'markdown-unfontify-region-wiki-links #'ignore))

(add-function :before-until
              (symbol-function 'pm--synchronize-points)
              (lambda (&rest args) poly-ein-mode))

(provide 'poly-ein)
