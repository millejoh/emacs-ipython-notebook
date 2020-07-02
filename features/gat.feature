@gat
Scenario: gat create from repo, then create from worktree, then run-local
  Given new git repo "test-repo"
  Given new python notebook in "test-repo"
  And I type "99"
  And I wait for cell to execute
  And I clear log expr "ein:log-all-buffer-name"
  And I press "C-x C-s"
  And I switch to log expr "ein:log-all-buffer-name"
  And I wait for buffer to say "Notebook is saved"
  And I switch to buffer like "Untitled"
  When I gat create "foo"
  Then I switch to buffer like ".gat/foo/Untitled"
  When I gat create "baz"
  Then I switch to buffer like ".gat/baz/Untitled"
  When I call "ein:gat-run-local"
  And I switch to buffer like "Dockerfile.Untitled"
  And I should see "FROM jupyter"
  And I press "C-x #"
  And I switch to buffer like "magit-process: baz"
  Then I wait for buffer to say "Building image"
  And I dump buffer
  And I go to word "run"
  And eval "(ignore-errors (kill-process (magit-section-value-if 'process)))"
  And I wait for buffer to not say "run . gat"
  And I am in notebooklist buffer
  And I click on "Home"
  Then I wait for buffer to say "support"
