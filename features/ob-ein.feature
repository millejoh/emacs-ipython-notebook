@memory
Scenario: R and Julia in the same org file
  Given I stop the server
  When I open temp file "ecukes.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein-r :session localhost :results raw drawer"
  And I press "RET"
  And I type "data.frame(x=c("a", "b c", "d"), y=1:3)"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "  x   y\n1 a   1\n2 b c 2\n3 d   3"
  And I should not see "[....]"
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein-julia :session localhost :results raw drawer"
  And I press "RET"
  And I type "isapprox(Base.MathConstants.e ^ (pi * im), -1)"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "true"
  And I dump buffer

@org
Scenario: no session defaults to localhost, and no output shouldn't leave [....]
  Given I stop the server
  When I open temp file "ecukes.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein :results raw drawer"
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I should not see "[....]"
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I type "foo = 5"
  And I ctrl-c-ctrl-c
  And I wait for buffer to not say "[....]"

@org
Scenario: ein-python can be python2 or python3
  When I open temp file "ecukes.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I should not see "[....]"
  And I place the cursor before ":results"
  And I type ":kernelspec python5 "
  And I ctrl-c-ctrl-c
  And I switch to log expr "ein:log-all-buffer-name"
  And I wait for buffer to say "ob-ein--initiate-session: switching"
  And I switch to buffer like "ecukes.org"
  And I wait for buffer to say "1.618"
  And I dump buffer

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
  And I should not see "[....]"
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
  And I should not see "[....]"
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
  And I should not see "[....]"

@org @complete @skip-travis
Scenario: Completion in an anonymous source block
  Given I set "ein:completion-backend" to eval "(quote ein:use-company-backend)"
  Given I eval (global-company-mode +1)
  Given I stop the server
  When I open temp file "complete.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein-python :kernelspec python3 :results raw drawer"
  And I press "RET"
  And I type "import itertools"
  And I press "RET"
  And I ctrl-c-ctrl-c
  And I wait 5 seconds
  And I should not see "[....]"
  And I press "C-c '"
  And I press "RET"
  And I type "itertools.chai"
  And I call "company-complete"
  Then I should see "itertools.chain"

@org
Scenario Anonymous blocks with a kernelspec
  Given I set "ein:completion-backend" to eval "(quote ein:use-none-backend)"
  Given I stop the server
  When I open temp file "path.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein-python :kernelspec python3 :results raw drawer"
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I should not see "[....]"

@org
Scenario: portless url with path, image, C-c ' lets you C-c C-c as well
  Given I set "ein:completion-backend" to eval "(quote ein:use-none-backend)"
  Given I stop the server
  When I open temp file "path.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost/undo.ipynb :results raw drawer"
  And I press "RET"
  And I type "(1 + 5 ** 0.5) / 2"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I should not see "[....]"
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I insert percent sign
  And I type "matplotlib inline"
  And I press "RET"
  And I type "import matplotlib.pyplot as plt ; import numpy as np ; x = np.linspace(0, 1, 100) ; y = np.random.rand(100,1) ; plt.plot(x,y)"
  And I ctrl-c-ctrl-c
  And I press "M->"
  And I type "<s"
  And I press "TAB"
  And I type "ein :session localhost :results raw drawer"
  And I press "RET"
  And I insert percent sign
  And I type "matplotlib inline"
  And I press "RET"
  And I type "import matplotlib.pyplot as plt ; import numpy as np ; x = np.linspace(0, 1, 100) ; y = np.random.rand(100,1) ; plt.plot(x,y)"
  And I ctrl-c-ctrl-c
  And I dump buffer
  And I wait for buffer to say "file:ein-image"

@export
Scenario: Test ob-exp captures code and results.
  When I open temp file "ecukes.org"
  And I call "org-mode"
  And I type "<s"
  And I press "TAB"
  And I type "ein :results scalar :exports both"
  And I press "C-n"
  And I type "ratio = (1 + 5 ** 0.5) / 2"
  And I press "RET"
  And I type "ratio"
  And I ctrl-c-ctrl-c
  And I wait for buffer to say "1.618"
  And I press "M->"
  And I press "RET"
  And I press "RET"
  And I type "<s"
  And I press "TAB"
  And I type "ein :results scalar :exports both"
  And I press "RET"
  And I type "ratio"
  And I dump buffer
  And I call "org-latex-export-to-latex"
