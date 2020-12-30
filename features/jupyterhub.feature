@jupyterhub
Scenario: basic spawn
  Given I start jupyterhub configured "c.JupyterHub.answer_yes=True\nc.JupyterHub.authenticator_class='dummyauthenticator.DummyAuthenticator'\nc.JupyterHub.cookie_secret_file = '/var/tmp/jupyterhub_cookie_secret'\nc.JupyterHub.spawner_class = 'simplespawner.SimpleLocalProcessSpawner'\n"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see ": [error]"
  Then I login to jupyterhub
