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
  And I wait for buffer to say "64"
  Then I should see "8"
  And I should see "16"
  And I should see "32"
  And I should see "64"

@execute-all-cells
Scenario: Execute all cells above
  Given new python notebook
  And I type "2 ** 3"
  And I press "C-c C-b"
  And I press "C-c C-t"
  And I type "markdown"
  And I press "C-c C-b"
  And I type "2 ** 4"
  And I press "C-c C-b"
  And I type "2 ** 5"
  And I press "C-c C-b"
  And I type "2 ** 6"
  And I press "C-c C-p"
  When I call "ein:worksheet-execute-all-cells-above"
  And I wait for buffer to say "16"
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
  And I wait for buffer to say "32"
  Then I should not see "8"
  And I should not see "16"
  And I should see "32"
  And I should see "64"

@execute-all-cells
Scenario: Boundary above
  Given new python notebook
  And I type "2 ** 3"
  And I press "C-c C-b"
  And I type "2 ** 4"
  And I press "C-c C-b"
  And I type "2 ** 5"
  And I press "C-c C-p"
  And I press "C-c C-p"
  When I call "ein:worksheet-execute-all-cells-above"
  Then I should see message "ein: [info] ein:worksheet-execute-all-cells: no cells above current"
