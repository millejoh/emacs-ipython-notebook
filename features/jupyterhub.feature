@jupyterhub
Scenario: basic login
  Given I start and login to jupyterhub configured "c.JupyterHub.answer_yes=True\nc.JupyterHub.authenticator_class='dummyauthenticator.DummyAuthenticator'\nc.JupyterHub.cookie_secret_file = '/var/tmp/jupyterhub_cookie_secret'"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
  Given I start and login to the server configured "\n"
