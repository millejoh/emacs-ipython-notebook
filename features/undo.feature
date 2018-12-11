@default
Scenario: Undo by default turned off
  Given new default notebook
  When I type "import math"
  And I wait for cell to execute
  And I undo demoting errors
  Then I should see message "demoted: (user-error No undo information in this buffer)"

@yank
Scenario: Kill yank doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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
  Then the cursor should be at point "74"

Scenario: Collapse doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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
  And I wait for cell to execute
  And I press "C-<down>"
  And I press "C-n"
  And I type "9"
  And I press "C-<up>"
  And I press "C-c C-e"
  And I press "C-/"
  Then the cursor should be at point "76"
  And I undo again
  Then the cursor should be at point "55"

@prob
Scenario: Test the conflagrative commands
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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

Scenario: Clear output doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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
  And I wait for cell to execute
  And I press "C-<down>"
  And I press "C-n"
  And I type "undo meee"
  And I press "C-<up>"
  And I press "C-c C-l"
  And I press "C-/"
  Then the cursor should be at point "74"
  And I undo again
  Then the cursor should be at point "55"

Scenario: Moving cells doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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

@forlorn
Scenario: Split and merge don't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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

@reopened
Scenario: Undo needs to at least work for reopened notebooks
  Given I start the server configured "\n"
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
  Then the cursor should be at point "125"

@toggle
Scenario: Toggling between markdown and codecell does not break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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

@timestamp
Scenario: Undo (kind of) needs to work when someone explicitly requires ein-timestamp
  Given I start the server configured "\n"
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
  Then the cursor should be at point "125"

@timestamp
Scenario: Kill yank doesn't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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
  Then the cursor should be at point "117"

@timestamp
Scenario: Split and merge don't break undo
  Given I enable "ein:worksheet-enable-undo"
  Given new default notebook
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
  Then the cursor should be at point "156"
  And I press "C-c C-m"
  And I press "C-c C-m"
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "93"
