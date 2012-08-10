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
                        :matches matches)))
    (mocker-let
        ((ein:completer-choose () ((:output 'completer)))
         (completer
          (matched-text matches)
          ((:input (list matched-text matches)))))
      (ein:completer-finish-completing '-not-used- content '-not-used-))))
