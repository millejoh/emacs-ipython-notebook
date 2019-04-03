@connect
Scenario: Company completion in a python buffer
  Given I set "ein:completion-backend" to eval "(quote ein:use-company-backend)"
  Given I kill all websocket buffers
  Given new default notebook
  When I open temp file "connect.py"
  And I switch to buffer like "connect.py"
  And I call "python-mode"
  And I connect to default notebook
  And I type "import itertools"
  And I press "RET"
  And I call "ein:connect-run-buffer"
  And I type "itertools."
  And I call "company-complete"
  And I wait for completions "itertools.chain"
  And I press "C-a"
  And I press "C-k"
  And I clear websocket log
  And I type "itertool"
  And I call "company-complete"
  Then I should see "itertools"
  And I type ".chai"
  And I call "company-complete"
  Then I should see "itertools.chain"
  Then no completion traffic

@connect
Scenario: Test shared eval
  Given new default notebook
  When I open temp file "connect.py"
  And I switch to buffer like "connect.py"
  And I connect to default notebook
  And I evaluate the python code "1+1"
  And I switch to buffer like "*ein:shared-output*"
  And I wait for buffer to say "2"

@connect
Scenario: Connect buffer to a running notebook
  Given new default notebook
  When I open temp file "connect.py"
  And I switch to buffer like "connect.py"
  And I connect to default notebook
  And I type "import math"
  And I press "RET"
  And I type "import itertools"
  And I press "RET"
  And I press "RET"
  And I type "def test01():"
  And I press "RET"
  And I press "TAB"
  And I type "return 'hello'"
  And I press "RET"
  And I press "RET"
  And I call "ein:connect-eval-buffer"
  And I switch to log expr "ein:log-all-buffer-name"
  And I wait for buffer to say "test01"
  And I switch to buffer like "Untitled"
  And I evaluate the python code "test01()"
  And I switch to buffer like "*ein:shared-output*"
  And I wait for buffer to say "'hello'"

