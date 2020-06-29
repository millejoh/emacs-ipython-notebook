@gat
Scenario: gat create
  Given new git repo "test-repo"
  Given new python in "test-repo" notebook
  When I gat create "foo"
  And I switch to buffer like "magit-process"
  Then I should see "gat create"
  And remove git repo "test-repo"
