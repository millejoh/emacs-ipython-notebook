(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notebook)

(ert-deftest ein:notebook-test-notebook-name-simple ()
  (should-not (ein:notebook-test-notebook-name nil))
  (should-not (ein:notebook-test-notebook-name ""))
  (should-not (ein:notebook-test-notebook-name "/"))
  (should-not (ein:notebook-test-notebook-name "\\"))
  (should-not (ein:notebook-test-notebook-name "a/b"))
  (should-not (ein:notebook-test-notebook-name "a\\b"))
  (should (ein:notebook-test-notebook-name "This is a OK notebook name")))
