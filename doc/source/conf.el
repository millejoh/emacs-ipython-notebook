(require 'cask "~/.cask/cask.el")

(let* (join  ; to suppress compiler warning...
       (join (lambda (p &rest ps)
               (if ps (apply join (expand-file-name (car ps) p) (cdr ps)) p)))
       (current-directory (file-name-directory load-file-name))
       (project-directory (funcall join current-directory ".." ".."))
       (bundle (cask-setup project-directory))
       (elpa-path (cask-elpa-path bundle))
       (path (lambda (&rest ps) (apply join project-directory ps))))
  (setq load-path (cask-load-path bundle))
  (add-to-list 'load-path (funcall path "lisp"))
  (cask-build bundle))
  ;; (add-to-list 'load-path (funcall path ".cask" version-str "websocket"))
  ;; (add-to-list 'load-path (funcall path ".cask" version-str "request"))
  ;; (add-to-list 'load-path (funcall path ".cask" version-str "nxhtml" "util")) ; mumamo
  ;; (add-to-list 'load-path (funcall path ".cask" version-str "auto-complete"))
  ;; (add-to-list 'load-path (funcall path ".cask" version-str "popup"))
   ; for auto-complete

(package-initialize)
(require 'ein-dev)
(ein:dev-require-all :ignore-p #'(lambda (f) (equal f "ein-smartrep.el")))
(require 'ob-ein)

;; Load `wid-edit'.  Otherwise the following error will be raised:
;;    Symbol's function definition is void: widget-button-press
(require 'wid-edit)
