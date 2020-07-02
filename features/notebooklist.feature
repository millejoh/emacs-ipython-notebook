@bread
Scenario: Breadcrumbs
  Given I am in notebooklist buffer
  When I click on dir "step-definitions" until "ein-steps"
  And I click on "Home"
  Then I wait for buffer to say "support"

@kernel
Scenario: Default kernel
  And I am in notebooklist buffer
  Then I should see "(*) Python"

Scenario: New Notebook
  Given I am in notebooklist buffer
  When I clear log expr "ein:log-all-buffer-name"
  And I click on "New Notebook"
  And no notebooks pending
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "Opened notebook Untitled"

Scenario: Resync
  Given I am in notebooklist buffer
  When I clear log expr "ein:log-all-buffer-name"
  And I click on "Resync"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "kernelspecs--complete"

Scenario: File open
  Given I am in notebooklist buffer
  And I go to word "notebooklist.feature"
  And I go to beginning of line
  And I click without going top on file "Open"
  And I switch to buffer like "notebooklist.feature"
  Then I should see "File open"

@stop
Scenario: Stop after closing notebook
  Given I am in notebooklist buffer
  Given I clear log expr "ein:log-all-buffer-name"
  And I click on "New Notebook"
  And no notebooks pending
  And I switch to buffer like "Untitled"
  And I press "C-x k"
  And I am in notebooklist buffer
  And I clear log expr "ein:log-all-buffer-name"
  And I keep clicking "Resync" until "Stop"
  And I click on "Stop"
  And I switch to log expr "ein:log-all-buffer-name"
  And I dump buffer
  Then I should see "kernel-delete-session--success"
  And I am in notebooklist buffer
  And I go to word "Untitled"
  And I go to beginning of line
  And I click without going top on "Open"
  And no notebooks pending
  And I switch to buffer like "Untitled"

@delete
Scenario: Delete closes buffers and sessions
  Given I am in notebooklist buffer
  Given I clear log expr "ein:log-all-buffer-name"
  And I click on "New Notebook"
  And no notebooks pending
  And I switch to buffer like "Untitled"
  And I am in notebooklist buffer
  And I clear log expr "ein:log-all-buffer-name"
  And I click on "Delete"
  And I wait for buffer to not say "Stop"
  And I dump buffer
  Then eval "(should-not (ein:notebook-opened-notebooks)))"
  Then eval "(should-not (seq-some (lambda (b) (cl-search "Untitled" (buffer-name b))) (buffer-list)))"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "kernel-delete-session--success"
  Then I should see "notebooklist-delete-notebook--complete"

@content
Scenario: Read a massive directory
  Given I create a directory "/var/tmp/fg7Cv8" with depth 4 and width 8
  And I get into notebook mode "/var/tmp/fg7Cv8" "8/4/3/bar.ipynb"
  And I open notebook "bar.ipynb"
  And I open file "foo.txt"
  And notebooklist-list-paths does not contain "4/4/4/foo.txt"
  And notebooklist-list-paths contains "foo.txt"

@login
Scenario: No token server
  Given I start the server configured "c.NotebookApp.token = u''\n"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"

@login
Scenario: With token server, get from server buffer
  Given I start the server configured "\n"
  And I login disabling crib token
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"

@login
Scenario: With password server
  Given I start the server configured "c.NotebookApp.password=u'sha1:712118ed6c09:bc02227d84b76b720cc320b855e1006d0b120f98'\n"
  And I login with password "foo"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"

@login
Scenario: To the cloud with password
  Given I start the server configured "c.NotebookApp.password=u'sha1:712118ed6c09:bc02227d84b76b720cc320b855e1006d0b120f98'\n"
  And I login forcing ping with password "foo"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"

@login
Scenario: Logging into nowhere
  Given I login erroneously to 0
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "[error] Login to http://127.0.0.1:0 failed"

@login
Scenario: Logging into nowhere
  Given I login erroneously to adfljdsf.org:8432
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "[error] Login to https://adfljdsf.org:8432 failed"

@login
Scenario: Bad curl invocation produces sensible error message
  Given I start the server configured "\n"
  And I login with bad curl
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "no-such-option"

@login
Scenario: jupyter not found
  And I start bad jupyter path

@jupyter-notebook
Scenario: Someone uses jupyter-notebook
  Given I customize "ein:jupyter-default-server-command" to "jupyter-notebook"
  And I set "ein:jupyter-server-use-subcommand" to eval "nil"
  Given I start and login to the server configured "\n"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
  And I customize "ein:jupyter-default-server-command" to "jupyter"
  And I set "ein:jupyter-server-use-subcommand" to "notebook"

@login
Scenario: With token server
  Given I start and login to the server configured "\n"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
