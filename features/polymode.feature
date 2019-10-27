@poly
Scenario: selection spans cells
  Given new python notebook
  And I press "C-c C-b"
  And I press "C-<up>"
  And I press "C-p"
  And I press "C-SPC"
  And I press "C-n"
  And I press "C-n"
  And I press "C-n"
  And I press "C-n"
  Then newlined region should be "In [ ]:\n\n\nIn [ ]:\n"
  And I press "C-g"
  Then the region should not be active

@poly
Scenario: markdown often erroneously fontifies the whole buffer
  Given new python notebook
  And I press "C-c C-t"
  And I type "# Header"
  And I press "RET"
  And I press "C-p"
  And I press "C-p"
  And I press "C-e"
  Then text property at point includes "rear-nonsticky"

@poly
Scenario: moving cells requires refontification
  Given new python notebook
  And I press "C-c C-t"
  And I type "# Header"
  And I press "RET"
  And I press "C-c C-b"
  And I type "import math"
  And I press "C-c C-c"
  And I press "M-<up>"
  And I press "C-<down>"
  And I go to word "Header"

@poly-complete
Scenario: completion in polymode notebook
  Given I set "ein:completion-backend" to eval "(quote ein:use-none-backend)"
  Given new python notebook
  And I type "import itertools"
  And I press "RET"
  And I type "itertools.chai"
  And I call "jedi:complete"
  And I wait for the smoke to clear
  Then jedi completions should contain "chain"
