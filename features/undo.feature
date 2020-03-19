@undo
Scenario: Undo turned off
  Given I disable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "import math"
  And I wait for cell to execute
  And I undo demoting errors
  Then I should see message "demoted: (user-error No undo information in this buffer)"

@undo
Scenario: Kill yank doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "import math"
  And I press "M-RET"
  And I type "[i for i in [1,2]]"
  And I press "M-RET"
  And I type "math.log(math.exp(1.0))"
  And I wait for cell to execute
  And I press "C-<up>"
  And I press "C-<up>"
  And I press "C-c C-k"
  And I press "C-<down>"
  And I press "C-c C-y"
  And I press "C-/"
  Then the cursor should be at point "75"

@undo
Scenario: Collapse doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "from time import sleep"
  And I press "RET"
  And I wait for cell to execute
  And I dump buffer
  And I press "C-c C-b"
  And I type "1 + 1"
  And I press "RET"
  And I press "C-<up>"
  And I press "C-n"
  And I type "print("abba\nabba")"
  And I press "RET"
  And I type "1.618"
  And I wait for cell to execute
  And I press "C-<down>"
  And I press "C-n"
  And I type "9"
  And I press "C-<up>"
  And I dump buffer
  And I press "C-c C-e"
  And I press "C-/"
  And I dump buffer
  Then the cursor should be at point "77"
  And I undo again
  Then the cursor should be at point "53"

@undo
Scenario: Test the conflagrative commands
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "import math"
  And I press "RET"
  And I press "M-RET"
  And I type "[i for i in [1,2]]"
  And I press "M-RET"
  And I type "math.log(math.exp(1.0))"
  And I wait for cell to execute
  And I press "C-<up>"
  And I press "C-<up>"
  And I press "C-n"
  And I type "print("m")"
  And I wait for cell to execute
  And I press "C-u C-c C-v"
  And I press "C-/"
  Then the cursor should be at point "22"
  And I undo again
  And I dump buffer
  And I press "C-c C-v"
  And I press "C-/"
  And I undo again
  Then the cursor should be at point "22"
  And I press "C-c C-S-l"
  And I press "C-/"
  And I undo again
  And I undo again
  Then the cursor should be at point "22"

@undo
Scenario: Clear output doesn't break undo, throw in multiple-cursors
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  And I clear log expr "ein:log-all-buffer-name"
  When I type "from time import sleep"
  And I press "RET"
  And I press "C-c C-b"
  And I type "1 + 1"
  And I press "RET"
  And I press "C-<up>"
  And I press "C-n"
  And I type "print("abba\nabba")"
  And I press "RET"
  And I type "1.618"
  And I add fake cursor to undo list
  And I wait for cell to execute
  And I press "C-<down>"
  And I press "C-n"
  And I type "undo meee"
  And I press "C-<up>"
  And I press "C-c C-l"
  And I press "C-/"
  Then the cursor should be at point "74"
  And I undo again
  Then the cursor should be at point "53"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "multiple-cursors-mode exception"

@undo
Scenario: Moving cells doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "100"
  And I press "C-c C-b"
  And I type "200"
  And I press "C-c C-b"
  And I type "print("hello")"
  And I wait for cell to execute
  And I press "C-<up>"
  And I wait for cell to execute
  And I press "C-<down>"
  And I press "C-c <up>"
  And I press "C-/"
  Then the cursor should be at point "54"
  And I press "C-<up>"
  And I press "C-<up>"
  And I wait for cell to execute
  And I press "C-c <down>"
  And I press "C-/"
  Then the cursor should be at point "67"

@undo
Scenario: Split and merge don't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "print("hello")"
  And I press "C-c C-b"
  And I type "1111"
  And I press "RET"
  And I press "RET"
  And I press "RET"
  And I type "2222"
  And I press "RET"
  And I type "3333"
  And I press "C-c C-b"
  And I type "4444"
  And I press "C-<up>"
  And I press "C-n"
  And I press "C-c C-s"
  And I wait for cell to execute
  And I press "C-<up>"
  And I wait for cell to execute
  And I press "C-<up>"
  And I wait for cell to execute
  And I press "C-/"
  And I press "C-<up>"
  And I type "5555"
  And I press "RET"
  And I type "6666"
  And I wait for cell to execute
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "70"
  And I press "C-c C-m"
  And I press "C-c C-m"
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "50"

@undo
Scenario: Undo needs to at least work for reopened notebooks
  Given I enable "ein:worksheet-enable-undo"
  Given old notebook "undo.ipynb"
  And I type "howdy"
  And I press "RET"
  And I press "C-<down>"
  And I press "C-<down>"
  And I type "rowdy"
  And I press "RET"
  And I press "C-<up>"
  And I press "C-<up>"
  And I press "C-c C-k"
  And I type "bowdy"
  And I press "RET"
  And I press "C-c C-y"
  And I press "C-/"
  Then the cursor should be at point "15"
  And I press "C-/"
  And I press "C-n"
  And I press "C-n"
  And I press "C-c C-s"
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "20"
  And I undo again
  Then the cursor should be at point "88"
  And I press "C-<down>"
  And I press "C-k"
  And I press "C-k"
  And I press "C-k"
  And I type "1.618"
  And I wait for cell to execute
  And I press "C-<up>"
  And I press "C-c C-m"
  And I press "C-n"
  And I press "C-n"
  And I press "C-c C-s"
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "124"

@undo
Scenario: Execute all cells, mod some cells, get outputs, undo mods
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type "from time import sleep"
  And I press "RET"
  And I type "sleep(3)"
  And I press "C-c C-b"
  And I type "sleep(1)"
  And I press "RET"
  And I type "import math"
  And I press "RET"
  And I type "2*math.asin(1)"
  And I press "C-c C-b"
  And I press "C-c C-t"
  And I type "mark"
  And I call "ein:worksheet-execute-all-cells-above"
  And I press "C-c C-b"
  And I type "undo"
  And I press "C-<up>"
  And I press "C-<up>"
  And I type "surprise"
  And I wait for buffer to say "3.14159"
  And I press "C-/"
  Then the cursor should be at point "51"
  And I undo again
  Then the cursor should be at point "139"
  And I undo again
  And dump diagnostic
  Then the cursor should be at point "125"

@undo
Scenario: Toggling between markdown and codecell does not break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new python notebook
  When I type ""to be markdown""
  And I press "C-c C-b"
  And I type "200"
  And I wait for cell to execute
  And I press "C-<up>"
  And I press "C-c C-t"
  And I press "C-/"
  Then the cursor should be at point "38"
  And I press "C-<up>"
  And I press "C-c C-t"
  And I press "C-/"
  Then the cursor should be at point "33"
  And I press "C-<up>"
  And I press "C-c C-t"
  And I wait for cell to execute
  And I press "C-/"
  Then the cursor should be at point "62"
