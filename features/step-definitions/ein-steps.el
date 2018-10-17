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

(When "^I am in notebooklist buffer$"
      (lambda ()
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (switch-to-buffer (ein:notebooklist-get-buffer url-or-port))
          (sit-for 0.8)
          )))

(When "^I wait \\([.0-9]+\\) seconds?$"
      (lambda (seconds)
        (sit-for (string-to-number seconds))))

(When "^I am in log buffer$"
      (lambda ()
        (switch-to-buffer ein:log-all-buffer-name)))

(defun ein:testing-new-notebook (url-or-port ks)
  (lexical-let (notebook)
    (condition-case err
        (progn
          (ein:notebooklist-new-notebook url-or-port ks nil
                                         (lambda (nb created &rest ignore)
                                           (setq notebook nb)))
          (ein:testing-wait-until (lambda () 
                                    (and notebook
                                         (ein:aand (ein:$notebook-kernel notebook)
                                                   (ein:kernel-live-p it))))
                                  nil 10000 2000)
          notebook)
      (error (message "ein:testing-new-notebook: %s" (error-message-string err))
             (when notebook
               (ein:notebook-close notebook))
             nil))))

(When "^new \\(.+\\) notebook$"
      (lambda (kernel)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
            (lexical-let ((ks (ein:get-kernelspec url-or-port kernel)) notebook)
              (loop repeat 2
                    until notebook
                    do (setq notebook (ein:testing-new-notebook url-or-port ks)))
              (let ((buf-name (format ein:notebook-buffer-name-template
                                      (ein:$notebook-url-or-port notebook)
                                      (ein:$notebook-notebook-name notebook))))
                (switch-to-buffer buf-name)
                (Then "I should be in buffer \"%s\"" buf-name)))))))

(When "^I start \\(and login to \\)?the server configured \"\\(.*\\)\"$"
      (lambda (login config)
        (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
          (ein:jupyter-server-stop t))
        (loop repeat 10
              with buffer = (get-buffer ein:jupyter-server-buffer-name)
              until (null (get-buffer-process buffer))
              do (sleep-for 1) 
              finally do (ein:aif (get-buffer-process buffer) (delete-process it)))
        (When "I clear log expr \"ein:log-all-buffer-name\"")
        (When "I clear log expr \"ein:jupyter-server-buffer-name\"")
        (clrhash ein:notebooklist-map)
        (with-temp-file ".ecukes-temp-config.py" (insert (s-replace "\\n" "\n" config)))
        (setq ein:jupyter-server-args '("--no-browser" "--debug" "--config=.ecukes-temp-config.py"))
        (ein:jupyter-server-start (executable-find "jupyter") 
                                  ein:testing-jupyter-server-root (not login))
        (if login
            (ein:testing-wait-until (lambda () (ein:notebooklist-list)) nil 20000 1000))))

(When "^I login to \\([.0-9]+\\)$"
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

(When "^I login \\(disabling token cribbing \\)?with password \"\\(.+\\)\"$"
      (lambda (no-crib password)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (let ((bindings '(((symbol-function 'ein:notebooklist-ask-url-or-port)
                             (lambda (&rest args) url-or-port)) 
                            ((symbol-function 'read-passwd)
                             (lambda (&rest args) password)))))
            (if no-crib 
                (setq bindings (append bindings
                                       (list '((symbol-function 'ein:notebooklist-token-or-password) (lambda (&rest args) nil))))))
            (message "%s" (macroexpand `(cl-letf ,bindings
                                            (When "I call \"ein:notebooklist-login\"")
                                            (And "I wait for the smoke to clear"))))
            (eval `(cl-letf ,bindings
                     (When "I call \"ein:notebooklist-login\"")
                     (And "I wait for the smoke to clear")))))))

(When "^I wait for the smoke to clear"
      (lambda ()
        (ein:testing-flush-queries)))

(When "^I click on \"\\(.+\\)\"$"
      (lambda (word)
        ;; from espuds "go to word" without the '\\b's
        (goto-char (point-min))
        (let ((search (re-search-forward (format "\\[%s\\]" word) nil t))
              (message "Cannot go to link '%s' in buffer: %s"))
          (cl-assert search nil message word (buffer-string))
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
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (with-current-buffer (ein:notebooklist-get-buffer url-or-port)
            (lexical-let (notebook)
              (ein:notebooklist-open-notebook ein:%notebooklist% path
                                              (lambda (nb created &rest -ignore-)
                                                (setq notebook nb)))
              (ein:testing-wait-until (lambda () (and (not (null notebook))
                                                 (ein:aand (ein:$notebook-kernel notebook)
                                                           (ein:kernel-live-p it)))))
              (let ((buf-name (format ein:notebook-buffer-name-template
                                      (ein:$notebook-url-or-port notebook)
                                      (ein:$notebook-notebook-name notebook))))
                (switch-to-buffer buf-name)
                (Then "I should be in buffer \"%s\"" buf-name)))))))

(When "^I dump buffer"
      (lambda () (message "%s" (buffer-string))))

(When "^I wait for cell to execute$"
      (lambda ()
        (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
          (ein:testing-wait-until (lambda () (not (slot-value cell 'running)))))))

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
