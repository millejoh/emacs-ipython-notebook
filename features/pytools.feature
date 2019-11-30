@pytools
Scenario: Check that pytools are being loaded.
  Given new python notebook
  And I type "__ein_pytools_version"
  And I press "M-RET"
  And I wait for the smoke to clear
  Then I should see "1.0.0"

@pytools @matplotlib
Scenario: Setting matplotlib figure size.
  Given new python notebook
  And I type "import matplotlib.pyplot as plt"
  And I press "M-RET"
  And I wait for the smoke to clear
  And I eval (ein:pytools-set-figure-size 8.0 6.0)
  And I type "__ein_rcParams['figure.figsize']"
  And I press "M-RET"
  And I wait for the smoke to clear
  Then I should see "[8.0, 6.0]"

@pytools @matplotlib
Scenario: Use generic command to set a matplotlib parameter.
  Given new python notebook
  And I type "import matplotlib.pyplot as plt"
  And I press "M-RET"
  And I wait for the smoke to clear
  And I eval (ein:pytools-set-matplotlib-parameter "figure.dpi" 120)
  And I type "__ein_rcParams['figure.dpi']"
  And I press "M-RET"
  And I wait for the smoke to clear
  Then I should see "120"
