@execute-all-cells
Scenario: Execute all cells
  Given new python notebook
  And I type "2 ** 3"
  And I press "C-c C-b"
  And I type "2 ** 4"
  And I press "C-c C-b"
  And I type "2 ** 5"
  And I press "C-c C-b"
  And I type "2 ** 6"
  When I call "ein:worksheet-execute-all-cells"
  And I wait 1 second
  Then I should see "8"
  And I should see "16"
  And I should see "32"
  And I should see "64"

@execute-all-cells
Scenario: Execute all cells above
  Given new python notebook
  And I type "2 ** 3"
  And I press "C-c C-b"
  And I type "2 ** 4"
  And I press "C-c C-b"
  And I type "2 ** 5"
  And I press "C-c C-b"
  And I type "2 ** 6"
  And I press "C-c C-p"
  When I call "ein:worksheet-execute-all-cells-above"
  And I wait 1 second
  Then I should see "8"
  And I should see "16"
  And I should not see "32"
  And I should not see "64"

@execute-all-cells
Scenario: Execute all cells below
  Given new python notebook
  And I type "2 ** 3"
  And I press "C-c C-b"
  And I type "2 ** 4"
  And I press "C-c C-b"
  And I type "2 ** 5"
  And I press "C-c C-b"
  And I type "2 ** 6"
  And I press "C-c C-p"
  When I call "ein:worksheet-execute-all-cells-below"
  And I wait 1 second
  Then I should not see "8"
  And I should not see "16"
  And I should see "32"
  And I should see "64"
