(When "^with no opened notebooks call \"\\(.+\\)\"$"
      (lambda (func)
        (cl-letf (((symbol-function 'ein:notebook-opened-buffer-names) #'ignore))
          (When (format "I call \"%s\"" func)))))

(When "^header \\(does not \\)?says? \"\\(.+\\)\"$"
      (lambda (negate says)
        (let ((equal-p (string= (substitute-command-keys says) (substitute-command-keys (slot-value (slot-value ein:%notification% 'kernel) 'message)))))
          (cl-assert (if negate (not equal-p) equal-p)))))

(When "^I kill kernel$"
      (lambda ()
        (ein:kernel-delete-session (ein:$notebook-kernel ein:%notebook%))
        (And "I wait for the smoke to clear")))

(When "^my reconnect is questioned"
      (lambda ()
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest ignore) t)))
          (ein:kernel-reconnect-session (ein:$notebook-kernel ein:%notebook%)
                                        (lambda (kernel session-p)
                                          (assert (not session-p)))))))

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

(When "^I am in notebooklist buffer$"
      (lambda ()
        (switch-to-buffer (ein:notebooklist-get-buffer (car (ein:jupyter-server-conn-info))))))

(When "^I wait \\([.0-9]+\\) seconds?$"
      (lambda (seconds)
        (sleep-for (string-to-number seconds))))

(When "^I am in log buffer$"
      (lambda ()
        (switch-to-buffer ein:log-all-buffer-name)))

(When "^new \\(.+\\) notebook$"
      (lambda (kernel)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (lexical-let (notebook)
            (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
              (lexical-let ((ks (ein:get-kernelspec url-or-port kernel)))
                (loop repeat 2
                      until notebook
                      do (setq notebook (ein:testing-new-notebook url-or-port ks)))))
            (let ((buf-name (format ein:notebook-buffer-name-template
                                    (ein:$notebook-url-or-port notebook)
                                    (ein:$notebook-notebook-name notebook))))
              (switch-to-buffer buf-name)
              (Then "I should be in buffer \"%s\"" buf-name))))))

(When "^I stop the server$"
      (lambda ()
        (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
          (ein:jupyter-server-stop t))
        (loop repeat 10
              with buffer = (get-buffer ein:jupyter-server-buffer-name)
              until (null (get-buffer-process buffer))
              do (sleep-for 1)
              finally do (ein:aif (get-buffer-process buffer) (delete-process it)))
        (When "I clear log expr \"ein:log-all-buffer-name\"")
        (When "I clear log expr \"ein:jupyter-server-buffer-name\"")))

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

(When "^I wait for the smoke to clear"
      (lambda ()
        (ein:testing-flush-queries)
        (And "I wait 1 second"))) ;; eldoc-documentation-function not flushing

(When "^I keep clicking \"\\(.+\\)\" until \"\\(.+\\)\"$"
      (lambda (go stop)
        (loop repeat 10
              until (search stop (buffer-string))
              do (And (format "I click on \"%s\"" go))
              do (sleep-for 0 1000)
              finally do (if (not (search stop (buffer-string))) (assert nil)))))

(When "^I click\\( without going top\\)? on \"\\(.+\\)\"$"
      (lambda (stay word)
        ;; from espuds "go to word" without the '\\b's
        (when (not stay)
          (goto-char (point-min)))
        (let ((search (re-search-forward (format "\\[%s\\]" word) nil t))
              (msg "Cannot go to link '%s' in buffer: %s"))
          (cl-assert search nil msg word (buffer-string))
          (backward-char)
          (When "I press \"RET\"")
          (sit-for 0.8)
          (When "I wait for the smoke to clear"))))

(When "^I click on dir \"\\(.+\\)\"$"
      (lambda (dir)
        (When (format "I go to word \"%s\"" dir))
        (re-search-backward "Dir" nil t)
        (When "I press \"RET\"")
        (sit-for 0.8)
        (When "I wait for the smoke to clear")))

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

(When "^I wait for cell to execute$"
      (lambda ()
        (let* ((cell (ein:worksheet-get-current-cell :cell-p #'ein:codecell-p))
               (orig (if (slot-boundp cell 'input-prompt-number)
                         (slot-value cell 'input-prompt-number))))
          (call-interactively #'ein:worksheet-execute-cell)
          (ein:testing-wait-until
           (lambda ()
             (ein:aand (and (slot-boundp cell 'input-prompt-number)
                            (slot-value cell 'input-prompt-number))
                       (and (numberp it)
                            (not (equal orig it)))))))))

(When "^I undo again$"
      (lambda ()
        (undo-more 1)))

(When "^I enable \"\\(.+\\)\"$"
      (lambda (flag-name)
        (set (intern flag-name) t)))

(When "^I undo demoting errors$"
      (lambda ()
        (with-demoted-errors "demoted: %s"
          (undo))))

(When "^I create a directory \"\\(.+\\)\" with depth \\([0-9]+\\) and width \\([0-9]+\\)$"
      (lambda (dir depth width)
        (when (f-exists? dir)
          (f-delete dir t))
        (f-mkdir dir)
        (ein:testing-make-directory-level dir 1 (string-to-number width) (string-to-number depth))))

(When "^\"\\(.+\\)\" should \\(not \\)?include \"\\(.+\\)\"$"
      (lambda (variable negate value)
        (let ((member-p (member value (symbol-value (intern variable)))))
          (if negate (should-not member-p) (should member-p)))))

(When "^I set \"\\(.+\\)\" to \"\\(.+\\)\"$"
      (lambda (variable value)
        (set (intern variable) value)))

(When "^I set \"\\(.+\\)\" to eval \"\\(.+\\)\"$"
      (lambda (variable value)
        (set (intern variable) (eval (car (read-from-string value))))))

(When "^I fset \"\\(.+\\)\" to \"\\(.+\\)\"$"
      (lambda (variable value)
        (fset (intern variable) (function value))))

(When "^I get into notebook mode \"\\(.+\\)\" \"\\(.+\\)\"$"
      (lambda (notebook-dir file-path)
        (When "I stop the server")
        (When (format "I find file \"%s\"" (concat (file-name-as-directory notebook-dir) file-path)))
        (When "I press \"C-c C-z\"")
        (ein:testing-wait-until (lambda () (ein:notebooklist-list)) nil 20000 1000)
        ))

(When "^I find file \"\\(.+\\)\"$"
      (lambda (file-name)
        (find-file file-name)
        (when (string= (file-name-extension file-name) "ipynb")
          (ein:ipynb-mode))
))

(When "notebooklist-list-paths does not contain \"\\(.+\\)\"$"
      (lambda (file-name)
        (Given "I am in notebooklist buffer")
        (let ((nbpath (ein:url (car (ein:jupyter-server-conn-info)) file-name)))
          (assert (not (member nbpath (ein:notebooklist-list-paths)))))))

(When "notebooklist-list-paths contains \"\\(.+\\)\"$"
      (lambda (file-name)
        (Given "I am in notebooklist buffer")
        (let ((nbpath (ein:url (car (ein:jupyter-server-conn-info)) file-name)))
          (assert (member nbpath (ein:notebooklist-list-paths))))))

(When "I open \\(notebook\\|file\\) \"\\(.+\\)\"$"
      (lambda (content-type file-name)
        (Given "I am in notebooklist buffer")
        (And "I clear log expr \"ein:log-all-buffer-name\"")
        (let ((nbpath (ein:url (car (ein:jupyter-server-conn-info)) file-name)))
          (cl-letf (((symbol-function 'ein:notebooklist-ask-path)
                     (lambda (&rest args) nbpath)))
            (When (format "I press \"C-c C-%s\"" (if (string= content-type "file") "f" "o")))
            (And "I wait for the smoke to clear")
            (Given "I switch to log expr \"ein:log-all-buffer-name\"")
            (Then (format "I should see \"Opened %s %s\"" content-type file-name))))))
