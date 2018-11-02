@autosave
Scenario: try autosaving
  Given new default notebook
  And I call "ein:notebook-enable-autosaves"
  Then I should see message "ein:notebook-autosave-frequency is 0"

@reconnect
Scenario: kernel restart succeeds
  Given new default notebook
  When I type "import math"
  And I wait for cell to execute
  And I kill processes like "websocket"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "WS closed unexpectedly"
  And I switch to buffer like "Untitled"
  And header says "Kernel requires reconnect C-c C-r"
  And I press "C-c C-r"
  And I wait for the smoke to clear
  And header does not say "Kernel requires reconnect C-c C-r"
  And I clear log expr "ein:log-all-buffer-name"
  And I reconnect kernel
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"
  And I should see "ein:kernel-start--complete"

  