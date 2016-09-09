(let* ((doc-source-path (file-name-directory load-file-name))
       (project-path (concat doc-source-path "../..")))
  (add-to-list 'load-path project-path))

(require 'request)

(provide 'deferred)     ; Pretend like deferred.el is already imported
(require 'request-deferred)
