(let* (join  ; to suppress compiler warning...
       (join (lambda (p &rest ps)
               (if ps (apply join (expand-file-name (car ps) p) (cdr ps)) p)))
       (current-directory (file-name-directory load-file-name))
       (project-directory (funcall join current-directory ".." ".."))
       (path (lambda (&rest ps) (apply join project-directory ps))))
  (add-to-list 'load-path (funcall path "lisp"))
  (add-to-list 'load-path (funcall path "lib" "websocket"))
  (add-to-list 'load-path (funcall path "lib" "request"))
  (add-to-list 'load-path (funcall path "lib" "nxhtml" "util")) ; mumamo
  (add-to-list 'load-path (funcall path "lib" "auto-complete"))
  (add-to-list 'load-path (funcall path "lib" "popup"))) ; for auto-complete

(require 'ein-dev)
(ein:dev-require-all)

;; Load `wid-edit'.  Otherwise the following error will be raised:
;;    Symbol's function definition is void: widget-button-press
(require 'wid-edit)
