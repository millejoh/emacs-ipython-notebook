@autosave
Scenario: opened notebook
  Given old notebook "undo.ipynb"
  And I call "ein:notebook-enable-autosaves"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see message "ein:notebook-autosave-frequency is 0"