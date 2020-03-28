@connect
Scenario: Connect buffer to an open notebook
  Given new python notebook
  And eval "(get-buffer-create "connect.feature.py")"
  And I switch to buffer like "connect.feature.py"
  And eval "(python-mode)"
  And I type "a = 10"
  And I press "C-c C-/ e"
  And I wait for the smoke to clear
  And I switch to buffer like "Untitled"
  And I type "a"
  And I wait for cell to execute
  Then I should see "10"
  And I switch to buffer like "connect.feature.py"
  And I press "RET"
  And I press "RET"
  And I type "for i in range(10):"
  And I press "RET"
  And I type "    j = i"
  And I press "C-p"
  And I press "C-a"
  And I press "C-SPC"
  And I press "C-x ]"
  And I press "C-c C-/ r"
  And I switch to buffer like "Untitled"
  And I press "C-c C-b"
  And I type "j"
  And I wait for cell to execute
  Then I should see "9"
