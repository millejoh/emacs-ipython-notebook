@org
Scenario: Specific port, portless localhost refers to same, concurrent execution
  Given I stop the server
  When I open temp file "ecukes.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type session port 8317
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I dump buffer
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I type "import math ; 4 * math.atan(1.0)"
  And I dump buffer
  And I clear log expr "ein:log-all-buffer-name"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "3.14159"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "Login to"
  And I switch to buffer like "ecukes.org"
  And I clear the buffer
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I ctrl-c-ctrl-c
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I type "import math ; 4 * math.atan(1.0)"
  And I ctrl-c-ctrl-c
  And I dump buffer
  And I wait for buffer to say "1.618"
  And I dump buffer
  And I wait for buffer to say "3.1415"

@org
Scenario: portless url with path and image
  When I open temp file "path.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost/undo.ipynb :results raw drawer"
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost/undo.ipynb :results raw drawer"
  And I press "RET"
  And I insert percent sign
  And I type "matplotlib inline"
  And I press "RET"
  And I type "import matplotlib.pyplot as plt ; import numpy as np ; x = np.linspace(0, 1, 100) ; y = np.random.rand(100,1) ; plt.plot(x,y)"
  And I ctrl-c-ctrl-c
  And I dump buffer
  And I wait for buffer to say "file:ein-image"
