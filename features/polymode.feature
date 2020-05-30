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

@poly
Scenario: Indirect buffers confuse buffer switching without hacks
  Given new python notebook
  And I type "print("hello")"
  And I switch to buffer "*Messages*"
  And I switch to buffer like "Untitled"
  And I press "C-x b"
  Then I should be in buffer "*Messages*"
  And I press "C-x b"
  Then eval "(string-prefix-p " *ein:" (buffer-name))"

@poly
Scenario: ido-mode spits back the indirect buffer we're already in
  Given eval "(ido-mode t)"
  Given new python notebook
  And I type "print("hello")"
  And I press "C-x b"
  And I press "C-x b"
  Then eval "(string-prefix-p " *ein:" (buffer-name))"
  And eval "(ido-mode -1)"