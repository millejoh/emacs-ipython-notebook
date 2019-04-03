@poly
Scenario: selection spans cells
  Given new default notebook
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
