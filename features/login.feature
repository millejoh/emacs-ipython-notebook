@login
Scenario: Logging into nowhere
Given I login to 0
Then I should see message "demoted: (error Connection refused: [error] http://127.0.0.1:0)"