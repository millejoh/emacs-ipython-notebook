@paren
Scenario: show-paren-mode should behave
  Given new python notebook
  And I press "C-c C-b"
  And I press "C-c C-p"
  And I type "x = np.arange(0"
  And I press "C-b"
  And I press "C-b"
  And eval "(let ((orig (point))) (funcall show-paren-data-function) (cl-assert (equal (point) orig)))"

@rename
Scenario: rename notebook
  Given new python notebook
  And I press "C-c C-/"
  And I switch to buffer like "Untitled"
  And rename notebook to "Renamed" succeeds

@image
Scenario: in ipython<=7.10, image failed to materialize initially
  Given new python notebook
  And I type "import numpy, math, matplotlib.pyplot as plt"
  And I press "RET"
  And I type "x = numpy.linspace(0, 2*math.pi)"
  And I press "RET"
  And I type "plt.plot(x, numpy.sin(x))"
  And I press "RET"
  And I clear log expr "ein:log-all-buffer-name"
  And I wait for cell to execute
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "msg_type=display_data"

@switch
Scenario: switch kernel
  Given new python notebook
  And I type "import itertools"
  And I wait for cell to execute
  And I switch kernel to "ir"

@pager
Scenario: so-called paging in ein:kernel--handle-payload
  Given new python notebook
  And I type "help?"
  And I wait for cell to execute
  And I switch to buffer like "*ein:pager"
  And I should see "help()"
  And I press "C-x k"

@traceback
Scenario: native json parsing broke traceback
  Given new python notebook
  And I type "def func1():"
  And I press "RET"
  And I type "    func2()"
  And I press "RET"
  And I type "def func2():"
  And I press "RET"
  And I type "    1/0"
  And I press "RET"
  And I type "func1()"
  And I press "RET"
  And I wait for cell to execute
  Then I should see "Traceback"
  And I press "C-c C-$"
  And I switch to buffer like "*ein:tb"
  Then I should see "ZeroDivisionError"

@reconnect
Scenario: kernel reconnect succeeds
  Given new python notebook
  When I type "import math"
  And I wait for cell to execute
  And I kill processes like "websocket"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "WS closed unexpectedly"
  And I switch to buffer like "Untitled"
  And header says "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command-km]"
  And I clear log expr "ein:log-all-buffer-name"
  And I press "C-c C-r"
  And I wait for the smoke to clear
  And header does not say "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command-km]"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
  And I should see "ein:kernel-retrieve-session--complete"
  And I switch to buffer like "Untitled"
  And I kill processes like "websocket"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "WS closed unexpectedly"
  And I switch to buffer like "Untitled"
  And header says "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command-km]"
  And I clear log expr "ein:log-all-buffer-name"
  And I wait for cell to execute
  And header does not say "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command-km]"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
  And I should see "ein:kernel-retrieve-session--complete"
  And I switch to buffer like "Untitled"
  And I clear log expr "ein:log-all-buffer-name"
  And I restart kernel
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
  And I should see "ein:kernel-retrieve-session--complete"
  And I switch to buffer like "Untitled"
  And I kill kernel
  And header says "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command-km]"
  And I clear log expr "ein:log-all-buffer-name"
  And my reconnect is questioned

@exit
Scenario: Saving fails upon quit, need to consult user
  Given new python notebook
  When I type "import math"
  And I wait for cell to execute
  And I cannot save upon quit

@kill
Scenario: Assign variable, save, kill notebook buffer, get it back, check variable
  Given new python notebook
  When I type "import math"
  And I press "RET"
  And I type "b = math.pi"
  And I press "RET"
  And I wait for cell to execute
  And I clear log expr "ein:log-all-buffer-name"
  And I press "C-x C-s"
  And I switch to log expr "ein:log-all-buffer-name"
  And I wait for buffer to say "Notebook is saved"
  And I switch to buffer like "Untitled"
  And I kill buffer and reopen
  And I press "C-c C-b"
  And I dump buffer
  And I type "b"
  And I wait for cell to execute
  Then I should see "3.1415"

@julia
Scenario: Smoke test julia
  Given new julia notebook
  When I type "isapprox(Base.MathConstants.e ^ (pi * im), -1)"
  And I wait for cell to execute
  Then I should see "true"
  And I dump buffer
