@dev
Scenario: dev-bug-report-template needs to work
  Given I call "ein:dev-bug-report-template"
  And I switch to buffer "*ein:bug-report*"
  Then I should see ":lib"
  And I should see "#### *ein:log-all*"
