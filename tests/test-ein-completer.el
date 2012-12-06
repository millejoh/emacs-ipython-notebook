(eval-when-compile (require 'cl))
(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (concat (file-name-directory load-file-name) "mocker")))
(require 'mocker)

(require 'ein-completer)


(ert-deftest ein:completer-finish-completing ()
  (let* ((matched-text 'dummy-matched-text-value) ; value can be anything
         (matches 'dummy-matches-value)
         (content (list :matched_text matched-text
                        :matches matches))
         (args '(:extend t)))
    (mocker-let
        ((ein:completer-choose () ((:output 'completer)))
         (completer
          (matched-text matches &rest args)
          ((:input (list matched-text matches args)))))
      (ein:completer-finish-completing args content '-not-used-))))
