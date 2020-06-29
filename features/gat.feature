@gat
Scenario: gat create
  Given new git repo "test-repo"
  Given new python notebook in "test-repo"
  When I gat create "foo"
  And I switch to buffer like "magit-process: "
  Then I should see "gat create"
  And I stop the server
  And remove git repo "test-repo"
