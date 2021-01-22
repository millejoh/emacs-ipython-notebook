@ipdb
Scenario: ein-ipdb works... poorly
  Given new python notebook
  And I type "from IPython.core.debugger import set_trace"
  And I press "RET"
  And I type "for i in range(2):"
  And I press "RET"
  And I type "    set_trace()"
  And I press "RET"
  And I type "    print(i)"
  And I press "RET"
  And I press "C-c C-c"
  And I switch to buffer like "*ipdb: "
  And I wait for buffer process
  And I type "c"
  And I press "RET"
  And I wait for buffer to say "ipython-input"
  And I type "c"
  And I press "RET"
  And I wait for buffer to say "Finished"
  And eval "(kill-buffer)"

@ipdb
Scenario: ein-ipdb needs to not flail on C-d
  Given new python notebook
  And I type "from IPython.core.debugger import set_trace"
  And I press "RET"
  And I type "for i in range(2):"
  And I press "RET"
  And I type "    set_trace()"
  And I press "RET"
  And I type "    print(i)"
  And I press "RET"
  And I press "C-c C-c"
  And I switch to buffer like "*ipdb: "
  And I wait for buffer process
  And I type "c"
  And I press "RET"
  And I wait for buffer to say "ipython-input"
  And I press "C-d"
  And I wait for buffer to say "Finished"
  And eval "(kill-buffer)"

@ipdb
Scenario: kill buffer
  Given new python notebook
  And I type "from IPython.core.debugger import set_trace"
  And I press "RET"
  And I type "for i in range(2):"
  And I press "RET"
  And I type "    set_trace()"
  And I press "RET"
  And I type "    print(i)"
  And I press "RET"
  And I press "C-c C-c"
  And I switch to buffer like "*ipdb: "
  And I wait for buffer process
  And I type "c"
  And I press "RET"
  And I wait for buffer to say "ipython-input"
  And I switch to buffer like "Untitled"
  Then I should see "In [*]"
  And header says "Kernel busy..."
  And I switch to buffer like "*ipdb: "
  And I press "C-x k"
  And I switch to buffer like "Untitled"
  And I wait for buffer to not say "In [*]"
  And header does not say "Kernel busy..."
