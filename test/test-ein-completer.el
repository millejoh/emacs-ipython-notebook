(eval-when-compile (require 'cl))
(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (concat (file-name-directory load-file-name) "mocker")))
(require 'mocker)

(require 'ein-completer)


(ert-deftest ein:completer-finish-completing ()
  (let ((matched-text "dummy-matched-text-value")
        (matches "dummy-matches-value"))
    (with-temp-buffer
      (insert matched-text)
      (let ((content (list :matches matches
                           :cursor_end (point-at-eol)
                           :cursor_start (point-at-bol)))
            (args '((:extend t))))     ; should this be :expand
        (mocker-let
         ((ein:completer-choose () ((:output 'completer)))
          (completer
           (matched-text matches &rest args)
           ((:input (list matched-text matches (car args))))))
         (ein:completer-finish-completing args content '-not-used-))))))
