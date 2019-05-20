@eldoc
Scenario: remote eldoc (largely unused)
  Given I enable "ein:enable-eldoc-support"
  Given I clear log expr "ein:log-all-buffer-name"
  Given new python notebook
  And I type "import math"
  And I press "C-a"
  And I call eldoc-documentation-function
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "ein:completions--prepare-oinfo"

@complete
Scenario: auto completion
  Given I set "ein:ac-direct-matches" to eval "nil"
  Given I set "ein:completion-backend" to eval "(quote ein:use-ac-backend)"
  Given new python notebook
  And I type "import itertool"
  And I press "C-c C-i"
  And I wait for the smoke to clear
  Then "ein:ac-direct-matches" should include "itertools"

@complete
Scenario: no auto completion
  Given I set "ein:ac-direct-matches" to eval "nil"
  Given I set "ein:completion-backend" to eval "(quote ein:use-none-backend)"
  Given new python notebook
  And I type "import itertool"
  And I press "C-c C-i"
  And I wait for the smoke to clear
  Then "ein:ac-direct-matches" should not include "itertools"

@complete
Scenario: company completion
  Given I set "ein:completion-backend" to eval "(quote ein:use-company-backend)"
  Given I kill all websocket buffers
  Given new python notebook
  And I type "import itertools"
  And I press "M-RET"
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
  Given I set "ein:completion-backend" to eval "(quote ein:use-ac-backend)"
  Given new python notebook
  Given I set "ein:completion-backend" to eval "(quote ein:use-none-backend)"

@rename
Scenario: rename notebook
  Given new python notebook
  And I press "C-c C-/"
  And I switch to buffer like "Untitled"
  And rename notebook to "Renamed" succeeds

@image
Scenario: image fails to materialize initially.  Document this in a test.
  Given new python notebook
  And I type "import numpy, math, matplotlib.pyplot as plt"
  And I press "RET"
  And I type "x = numpy.linspace(0, 2*math.pi)"
  And I press "RET"
  And I type "plt.plot(x, numpy.sin(x))"
  And I press "RET"
  And I clear log expr "ein:log-all-buffer-name"
  And I wait for cell to execute
  And I dump buffer
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "msg_type=display_data"
  And I switch to buffer like "Untitled"
  And I wait for cell to execute
  And I dump buffer
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "msg_type=display_data"

@switch
Scenario: switch kernel
  Given new python notebook
  And I type "import itertools"
  And I wait for cell to execute
  And I switch kernel to "ir"

@reconnect
Scenario: kernel reconnect succeeds
  Given new python notebook
  When I type "import math"
  And I wait for cell to execute
  And I kill processes like "websocket"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "WS closed unexpectedly"
  And I switch to buffer like "Untitled"
  And header says "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command]"
  And I clear log expr "ein:log-all-buffer-name"
  And I press "C-c C-r"
  And I wait for the smoke to clear
  And header does not say "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command]"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"
  And I should see "ein:kernel-retrieve-session--complete"
  And I switch to buffer like "Untitled"
  And I kill processes like "websocket"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "WS closed unexpectedly"
  And I switch to buffer like "Untitled"
  And header says "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command]"
  And I clear log expr "ein:log-all-buffer-name"
  And I wait for cell to execute
  And header does not say "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command]"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"
  And I should see "ein:kernel-retrieve-session--complete"
  And I switch to buffer like "Untitled"
  And I clear log expr "ein:log-all-buffer-name"
  And I restart kernel
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"
  And I should see "ein:kernel-retrieve-session--complete"
  And I switch to buffer like "Untitled"
  And I kill kernel
  And header says "Kernel requires reconnect \<ein:notebook-mode-map>\[ein:notebook-reconnect-session-command]"
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
  And I press "C-x C-s"
  And I wait for the smoke to clear
  And I kill buffer and reopen
  And I press "C-c C-b"
  And I type "b"
  And I wait for cell to execute
  Then I should see "3.1415"
  And I press "C-x k"

@julia
Scenario: Smoke test julia
  Given new julia notebook
  When I type "isapprox(Base.MathConstants.e ^ (pi * im), -1)"
  And I wait for cell to execute
  Then I should see "true"
  And I dump buffer