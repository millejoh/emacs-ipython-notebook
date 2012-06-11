(add-to-list 'load-path "~/.emacs.d/el-get/ein/")
(add-to-list 'load-path "~/.emacs.d/el-get/websocket/")

(require 'ein-notebooklist)
(require 'ein-connect)

;; Load `wid-edit'.  Otherwise the following error will be raised:
;;    Symbol's function definition is void: widget-button-press
(require 'wid-edit)
