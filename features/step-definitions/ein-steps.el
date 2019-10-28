(When "^I insert percent sign$" ;; https://github.com/ecukes/ecukes/issues/58
      (lambda ()
        (insert-char 37)))

(When "^I type session port \\([0-9]+\\)$"
      (lambda (port)
        (ein:process-refresh-processes)
        (assert (not (ein:process-url-match (ein:url port))))
        (When (format "I type \"ein :session localhost:%s :results raw drawer\"" port))))

(When "^I ctrl-c-ctrl-c$"
      (lambda ()
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest args) ein:testing-jupyter-server-root)))
          (When "I press \"C-c C-c\""))))

(When "^with no opened notebooks call \"\\(.+\\)\"$"
      (lambda (func)
        (cl-letf (((symbol-function 'ein:notebook-opened-buffer-names) #'ignore))
          (When (format "I call \"%s\"" func)))))

(When "^header \\(does not \\)?says? \"\\(.+\\)\"$"
      (lambda (negate says)
        (let ((equal-p (string= (substitute-command-keys says)
                                (substitute-command-keys
                                 (slot-value
                                  (slot-value ein:%notification% 'kernel)
                                  'message)))))
          (if negate (should-not equal-p) (should equal-p)))))

(When "^I switch kernel to \"\\(.+\\)\"$"
      (lambda (kernel-name)
        (cl-letf (((symbol-function 'R-mode) #'ignore))
          (let ((notebook (ein:notebook-switch-kernel (ein:get-notebook) kernel-name)))
            (loop repeat 10
                  until (ein:kernel-live-p (ein:$notebook-kernel notebook))
                  do (sleep-for 0 500)
                  finally do (should (string= "R" (ein:$kernelspec-language
                                                   (ein:$notebook-kernelspec notebook)))))))))

(When "^I kill kernel$"
      (lambda ()
        (ein:kernel-delete-session (ein:$notebook-kernel ein:%notebook%))
        (And "I wait for the smoke to clear")))

(When "^my reconnect is questioned"
      (lambda ()
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest ignore) t)))
          (ein:kernel-reconnect-session (ein:$notebook-kernel ein:%notebook%)
                                        (lambda (kernel session-p)
                                          (should-not session-p))))))
(When "^I cannot save upon quit$"
      (lambda ()
        (let ((always-errback
               (lambda (args)
                 (cl-destructuring-bind (notebook callback cbargs errback)
                     args
                   (list notebook errback nil errback)))))
          (add-function :filter-args (symbol-function 'ein:notebook-save-notebook)
                        always-errback)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (message "%s" prompt) t)))
            (should (ein:notebook-opened-notebooks))
            (ein:notebook-close-notebooks)
            (Then "I should see message \"Some notebooks could not be saved.  Exit anyway?\""))
          (remove-function (symbol-function 'ein:notebook-save-notebook) always-errback))))

(When "I restart kernel$"
      (lambda ()
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest ignore) t)))
          (ein:notebook-restart-session-command))
        (And "I wait for the smoke to clear")))

(When "I call eldoc-documentation-function$"
      (lambda ()
        (funcall (symbol-value 'eldoc-documentation-function))
        (And "I wait for the smoke to clear")
        ))

(When "I kill processes like \"\\(.+\\)\"$"
      (lambda (substr)
        (mapc (lambda (p) (if (search substr (process-name p)) (delete-process p)))
              (process-list))))

(When "^I clear websocket log$"
      (lambda ()
        (let ((buffer (cl-some (lambda (x) (and (search "*websocket" x) x))
                               (mapcar #'buffer-name (buffer-list)))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer))))))

(When "^I kill all websocket buffers$"
      (lambda ()
        (dolist (b (buffer-list))
          (when (search "*websocket" (buffer-name b))
            (kill-buffer b)))))

(When "^no completion traffic"
      (lambda ()
        (let ((buffer (cl-some (lambda (x) (and (search "*websocket" x) x))
                                (mapcar #'buffer-name (buffer-list)))))
          (with-current-buffer buffer
            (should-not (search "\"matches\"" (buffer-string)))))))

(When "^I clear log expr \"\\(.+\\)\"$"
      (lambda (log-expr)
        (let ((buffer (get-buffer (symbol-value (intern log-expr)))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)))))))

(When "^I switch to log expr \"\\(.+\\)\"$"
      (lambda (log-expr)
        (switch-to-buffer (symbol-value (intern log-expr)))))

(When "^I switch to buffer like \"\\(.+\\)\"$"
      (lambda (substr)
        (switch-to-buffer (car (-non-nil (mapcar (lambda (b) (if (search substr (buffer-name b)) b)) (buffer-list)))))))

(When "^rename notebook to \"\\(.+\\)\" succeeds$"
      (lambda (new-name)
        (let* ((old-name (ein:$notebook-notebook-name ein:%notebook%))
               (old-count
                (length (seq-filter (lambda (b)
                                      (search old-name (buffer-name b)))
                                    (buffer-list)))))
          (ein:notebook-rename-command new-name)
          (ein:testing-wait-until
           (lambda ()
             (= old-count
                (length (seq-filter (lambda (b)
                                      (search new-name (buffer-name b)))
                                    (buffer-list)))))))))

(When "^I am in notebooklist buffer$"
      (lambda ()
        (switch-to-buffer (ein:notebooklist-get-buffer (car (ein:jupyter-server-conn-info))))
        (ein:testing-wait-until (lambda () (eq major-mode 'ein:notebooklist-mode)))))

(When "^I wait \\([.0-9]+\\) seconds?$"
      (lambda (seconds)
        (sleep-for (string-to-number seconds))))

(When "^I am in log buffer$"
      (lambda ()
        (switch-to-buffer ein:log-all-buffer-name)))

(When "^new \\(.+\\) notebook$"
      (lambda (prefix)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (let (notebook)
            (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
              (ein:and-let* ((kslist (mapcar #'car (ein:list-available-kernels url-or-port)))
                             (found (seq-some (lambda (x) (and (search prefix x) x)) kslist))
                             (ks (ein:get-kernelspec url-or-port found)))
                (setq notebook (ein:testing-new-notebook url-or-port ks))))
            (should notebook)
            (let ((buf-name (format ein:notebook-buffer-name-template
                                    (ein:$notebook-url-or-port notebook)
                                    (ein:$notebook-notebook-name notebook))))
              (switch-to-buffer buf-name)
              (Then "I should be in buffer \"%s\"" buf-name))))))

(When "^case sensitive checked notebook$"
      (lambda ()
        (let ((case-fold-search nil))
          (Then "new python notebook"))))

(When "^I kill buffer and reopen$"
      (lambda ()
        (let ((name (ein:$notebook-notebook-name ein:%notebook%)))
          (When "I press \"C-x k\"")
          (And (format "old notebook \"%s\"" name)))))

(When "^I \\(finally \\)?stop the server\\(\\)$"
      (lambda (final-p _workaround)
        (cancel-function-timers #'ein:notebooklist-reload)
        (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
          (ein:jupyter-server-stop t))
        (loop repeat 10
              with buffer = (get-buffer ein:jupyter-server-buffer-name)
              until (null (get-buffer-process buffer))
              do (sleep-for 0 1000)
              finally do (ein:aif (get-buffer-process buffer) (delete-process it)))
        (condition-case err
            (ein:testing-wait-until (lambda ()
                                      (null (ein:notebooklist-keys)))
                                    nil 10000 1000)
          (error (ein:log 'warn "Stopping server: orphaned %s" (ein:notebooklist-keys))
                 (clrhash ein:notebooklist-map)))
        (unless final-p
          (When "I clear log expr \"ein:log-all-buffer-name\"")
          (When "I clear log expr \"ein:jupyter-server-buffer-name\""))))

(When "^I start and login to jupyterhub configured \"\\(.*\\)\"$"
      (lambda (config)
        (When "I stop the server")
        (cl-letf (((symbol-function 'ein:notebooklist-ask-user-pw-pair)
                   (lambda (&rest args) (list (intern (user-login-name)) ""))))
          (with-temp-file ".ecukes-temp-config.py" (insert (s-replace "\\n" "\n" config)))
          (let ((ein:jupyter-server-args
                 '("--debug" "--no-db" "--config=.ecukes-temp-config.py")))
            (ein:jupyter-server-start (executable-find "jupyterhub") nil))
          (ein:testing-wait-until (lambda () (ein:notebooklist-list)) nil 20000 1000))))

(When "^newlined region should be \"\\(.*\\)\"$"
      (lambda (region)
        (should (string= (s-replace "\\n" "\n" region)
                         (buffer-substring-no-properties (region-beginning) (region-end))))))

(When "^I start \\(and login to \\)?the server configured \"\\(.*\\)\"$"
      (lambda (login config)
        (When "I stop the server")
        (with-temp-file ".ecukes-temp-config.py" (insert (s-replace "\\n" "\n" config)))
        (let ((ein:jupyter-server-args '("--no-browser" "--debug" "--config=.ecukes-temp-config.py")))
          (ein:jupyter-server-start (executable-find ein:jupyter-default-server-command)
                                    ein:testing-jupyter-server-root (not login)))
        (if login
            (ein:testing-wait-until (lambda () (ein:notebooklist-list)) nil 20000 1000))))

(When "^I login erroneously to \\(.*\\)$"
      (lambda (port)
        (cl-letf (((symbol-function 'ein:notebooklist-ask-url-or-port)
                   (lambda (&rest args) (ein:url port)))
                  ((symbol-function 'read-passwd)
                   (lambda (&rest args) "foo")))
          (with-demoted-errors "demoted: %s"
            (When "I call \"ein:notebooklist-login\"")
            (And "I wait for the smoke to clear")))))

(When "^I login with bad curl$"
      (lambda ()
        (let ((request-curl-options '("--no-such-option")))
          (cl-letf (((symbol-function 'ein:notebooklist-ask-url-or-port)
                     (lambda (&rest args) (ein:url 8888)))
                    ((symbol-function 'read-passwd)
                     (lambda (&rest args) "foo")))
            (When "I call \"ein:notebooklist-login\"")
            (And "I wait for the smoke to clear")))))

(When "^I login if necessary$"
      (lambda ()
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (when token
            (cl-letf (((symbol-function 'ein:notebooklist-ask-url-or-port)
                       (lambda (&rest args) url-or-port))
                      ((symbol-function 'read-passwd)
                       (lambda (&rest args) token)))
              (When "I call \"ein:notebooklist-login\"")
              (And "I wait for the smoke to clear"))))))

(When "^I login disabling crib token$"
      (lambda ()
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (cl-letf (((symbol-function 'ein:notebooklist-ask-url-or-port)
                     (lambda (&rest args) url-or-port))
                    ((symbol-function 'ein:crib-token)
                     (lambda (&rest args) (list nil nil))))
            (When "I call \"ein:notebooklist-login\"")
            (And "I wait for the smoke to clear")))))

(When "^I login \\(forcing ping \\)?with password \"\\(.+\\)\"$"
      (lambda (no-crib password)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (let ((bindings '(((symbol-function 'ein:notebooklist-ask-url-or-port)
                             (lambda (&rest args) url-or-port))
                            ((symbol-function 'read-passwd)
                             (lambda (&rest args) password)))))
            (if no-crib
                (setq bindings (append bindings
                                       (list '((symbol-function 'ein:notebooklist-token-or-password) (lambda (&rest args) nil))))))
            (eval `(cl-letf ,bindings
                     (When "I call \"ein:notebooklist-login\"")
                     (And "I wait for the smoke to clear")))))))

(When "^I wait for completions \"\\(.+\\)\"$"
      (lambda (key)
        (loop repeat 10
              until (gethash key (ein:$kernel-oinfo-cache (ein:get-kernel)))
              do (sleep-for 0 500)
              finally do (should (gethash key (ein:$kernel-oinfo-cache (ein:get-kernel)))))))

(When "^I wait for the smoke to clear"
      (lambda ()
        (ein:testing-flush-queries)
        (And "I wait 2 seconds"))) ;; eldoc-documentation-function not flushing

(When "^I keep clicking \"\\(.+\\)\" until \"\\(.+\\)\"$"
      (lambda (go stop)
        (loop repeat 10
              until (search stop (buffer-string))
              do (And (format "I click on \"%s\"" go))
              do (sleep-for 0 1000)
              finally do (should (search stop (buffer-string))))))

(When "^I click\\( without going top\\)? on \"\\(.+\\)\"$"
      (lambda (stay word)
        ;; from espuds "go to word" without the '\\b's
        (when (not stay)
          (goto-char (point-min)))
        (let ((search (re-search-forward (format "\\[%s\\]" word) nil t))
              (msg "Cannot go to link '%s' in buffer: %s"))
          (should search)
          (backward-char)
          (let ((was (widget-at)))
            (When "I press \"RET\"")
            (loop until (not (equal was (widget-at)))
                  do (sleep-for 0 500))))))

(When "^I click on dir \"\\(.+\\)\"$"
      (lambda (dir)
        (When (format "I go to word \"%s\"" dir))
        (re-search-backward "Dir" nil t)
        (let ((was (widget-at)))
          (When "I press \"RET\"")
          (loop until (not (equal was (widget-at)))
                do (sleep-for 0 500)))))

(When "^old notebook \"\\(.+\\)\"$"
      (lambda (path)
        (lexical-let ((url-or-port (car (ein:jupyter-server-conn-info))) notebook)
          (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
            (loop repeat 2
                  until (and notebook
                             (ein:aand (ein:$notebook-kernel notebook)
                                       (ein:kernel-live-p it)))
                  do (ein:notebook-open url-or-port path nil
                                        (lambda (nb created) (setq notebook nb)))
                  do (sleep-for 0 1000)))
          (let ((buf-name (format ein:notebook-buffer-name-template
                                  (ein:$notebook-url-or-port notebook)
                                  (ein:$notebook-notebook-name notebook))))
            (switch-to-buffer buf-name)
            (Then "I should be in buffer \"%s\"" buf-name)))))

(When "^I dump buffer"
      (lambda () (message "%s" (buffer-string))))

(When "^I wait for buffer to\\( not\\)? say \"\\(.+\\)\"$"
      (lambda (negate bogey)
        (ein:testing-wait-until
         (lambda ()
           (let* ((says (s-contains? (s-replace "\\n" "\n" bogey) (buffer-string))))
             (ein:aif (if negate (not says) says)
                 it
               (when (with-current-buffer ein:log-all-buffer-name
                       (search "WS closed unexpectedly" (buffer-string)))
                 (And "I clear log expr \"ein:log-all-buffer-name\"")
                 (Then "I ctrl-c-ctrl-c"))
               nil)))
         nil 40000 2000)))

(When "^I wait for cell to execute$"
      (lambda ()
        (poly-ein-base
         (let* ((cell (ein:worksheet-get-current-cell :cell-p #'ein:codecell-p))
                (orig (if (slot-boundp cell 'input-prompt-number)
                          (slot-value cell 'input-prompt-number))))
           (call-interactively #'ein:worksheet-execute-cell)
           (ein:testing-wait-until
            (lambda ()
              (ein:aand (and (slot-boundp cell 'input-prompt-number)
                             (slot-value cell 'input-prompt-number))
                        (and (numberp it)
                             (not (equal orig it)))))
            nil 10000 2000)))))

(When "^I undo again$"
      (lambda ()
        (undo-more 1)))

(When "^I enable \"\\(.+\\)\"$"
      (lambda (flag-name)
        (set (intern flag-name) t)))

(When "^I disable \"\\(.+\\)\"$"
      (lambda (flag-name)
        (set (intern flag-name) nil)))

(When "^I undo demoting errors$"
      (lambda ()
        (with-demoted-errors "demoted: %s"
          (undo))))

(When "^I start bad jupyter path$"
      (lambda ()
        (condition-case err
            (let* ((*ein:last-jupyter-command* "not-jupyter")
                   (ein:jupyter-default-server-command *ein:last-jupyter-command*))
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (&rest args) ein:jupyter-default-server-command))
                        ((symbol-function 'read-directory-name)
                         (lambda (&rest args) ein:jupyter-default-notebook-directory)))
                (call-interactively #'ein:jupyter-server-start))
              ;; should err before getting here
              (should-not t))
          (error (should (search "not-jupyter not found" (error-message-string err)))))))

(When "^I create a directory \"\\(.+\\)\" with depth \\([0-9]+\\) and width \\([0-9]+\\)$"
      (lambda (dir depth width)
        (when (f-exists? dir)
          (f-delete dir t))
        (f-mkdir dir)
        (ein:testing-make-directory-level dir 1
                                          (string-to-number width)
                                          (string-to-number depth))
        (call-process-shell-command (format "find %s -print | xargs du" dir))))

(When "^\"\\(.+\\)\" should \\(not \\)?include \"\\(.+\\)\"$"
      (lambda (variable negate value)
        (let ((member-p (member value (symbol-value (intern variable)))))
          (if negate (should-not member-p) (should member-p)))))

(When "^I customize \"\\(.+\\)\" to \"\\(.+\\)\"$"
      (lambda (variable value)
        (custom-set-variables `(,(intern variable) ,value t))))

(When "^I set \"\\(.+\\)\" to \"\\(.+\\)\"$"
      (lambda (variable value)
        (set (intern variable) value)))

(When "^I set \"\\(.+\\)\" to eval \"\\(.+\\)\"$"
      (lambda (variable value)
        (set (intern variable) (eval (car (read-from-string value))))))

(When "^I eval \"\\(.+\\)\"$"
      (lambda (expr)
        (eval (car (read-from-string expr)))))

(When "^I connect to default notebook"
      (lambda ()
        (ein:connect-to-notebook-buffer (car (ein:notebook-opened-buffer-names
                                             #'(lambda (bufname)
                                                 (string-match "Untitled" bufname)))))))

(When "^I connect to \"\\(.+\\)\"$"
      (lambda (name)
        (ein:connect-to-notebook-buffer (car (ein:notebook-opened-buffer-names
                                              #'(lambda (bufname)
                                                  (string-match name bufname)))))))

(When "^I fset \"\\(.+\\)\" to \"\\(.+\\)\"$"
      (lambda (variable value)
        (fset (intern variable) (function value))))

(When "^the value of \"\\(.+\\)\" is \\(.+\\)$"
      (lambda (variable value)
        (should (equal (symbol-value (intern variable)) (symbol-value (intern value))))))

(When "^I get into notebook mode \"\\(.+\\)\" \"\\(.+\\)\"$"
      (lambda (notebook-dir file-path)
        (When "I stop the server")
        (When (format "I find file \"%s\"" (concat (file-name-as-directory notebook-dir) file-path)))
        (When "I press \"C-c C-z\"")
        (ein:testing-wait-until (lambda () (ein:notebooklist-list)) nil 20000 1000)))

(When "^I find file \"\\(.+\\)\"$"
      (lambda (file-name)
        (find-file file-name)
        (when (string= (file-name-extension file-name) "ipynb")
          (ein:ipynb-mode))
))

(When "notebooklist-list-paths\\( does not\\)? contains? \"\\(.+\\)\"$"
      (lambda (negate file-name)
        (Given "I am in notebooklist buffer")
        (let* ((nbpath (ein:url (car (ein:jupyter-server-conn-info)) file-name))
               (contains-p (member nbpath (ein:notebooklist-list-paths))))
          (should (if negate (not contains-p) contains-p)))))

(When "I open \\(notebook\\|file\\) \"\\(.+\\)\"$"
      (lambda (content-type file-name)
        (Given "I am in notebooklist buffer")
        (And "I clear log expr \"ein:log-all-buffer-name\"")
        (lexical-let ((nbpath (ein:url (car (ein:jupyter-server-conn-info)) file-name)))
          (cl-letf (((symbol-function 'ein:notebooklist-ask-path)
                     (lambda (&rest args) nbpath)))
            (When (format "I press \"C-c C-%s\"" (if (string= content-type "file") "f" "o")))
            (And "I wait for the smoke to clear")
            (Given "I switch to log expr \"ein:log-all-buffer-name\"")
            (Then (format "I should see \"Opened %s %s\"" content-type file-name))))))

(When "I evaluate the python code \"\\(.+\\)\"$"
      (lambda (code-str)
        (ein:shared-output-eval-string nil code-str nil)))

(When "^text property at point includes \"\\(.+\\)\"$"
      (lambda (properties)
        (should-not
         (mapcan (lambda (prop)
                   (not (get-text-property (point) (intern prop))))
                 (split-string properties ",")))))

(When "^jedi completions should contain \"\\(.+\\)\"$"
      (lambda (str)
        (let ((completions (jedi:ac-direct-matches)))
          (should (some #'(lambda (x)
                            (string= x str))
                        completions)))))

(When "^jedi completion environment$"
      (lambda ()
        (require 'jedi)
        (jedi:install-server-block)
        (add-hook 'python-mode-hook 'jedi:setup)
        (setq jedi:complete-on-dot t)))
