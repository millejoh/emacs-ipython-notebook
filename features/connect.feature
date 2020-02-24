@connect
Scenario: Connect buffer to an open notebook
  Given new python notebook
  And eval "(require 'ein-connect)"
  And I type "a"
  And I create buffer named "test.py"
  And I switch to buffer like "test.py"
  And I type "a = 10"
  And I press "RET"
  And I press "M-x ein:connect-to-notebook-command"
  And I press "M-x ein:connect-eval-buffer"
  And I wait for the smoke to clear
  And I switch to buffer like "Untitled"
  And I press "RET"
  And I wait for cell to execute
  Then I should see "10"
