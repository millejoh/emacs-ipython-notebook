(When "^I clear log expr \"\\(.+\\)\"$"
      (lambda (log-expr)
        (with-current-buffer (symbol-value (intern log-expr))
          (let ((inhibit-read-only t))
            (erase-buffer)))))

(When "^I switch to log expr \"\\(.+\\)\"$"
      (lambda (log-expr)
        (switch-to-buffer (symbol-value (intern log-expr)))))

(When "^I am in notebooklist buffer$"
      (lambda ()
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (switch-to-buffer (ein:notebooklist-get-buffer url-or-port))
          (sit-for 0.8)
          )))

(When "^I wait \\([.0-9]+\\) seconds$"
      (lambda (seconds)
        (sit-for (string-to-number seconds))))

(When "^I am in log buffer$"
      (lambda ()
        (switch-to-buffer ein:log-all-buffer-name)))

(When "^new \\(.+\\) notebook$"
      (lambda (kernel)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (lexical-let ((ks (ein:get-kernelspec url-or-port kernel)) notebook)
            (ein:notebooklist-new-notebook url-or-port ks nil
                (lambda (nb created &rest -ignore-)
                  (setq notebook nb)))
            (ein:testing-wait-until (lambda () (and (not (null notebook))
                                                    (ein:aand (ein:$notebook-kernel notebook)
                                                              (ein:kernel-live-p it)))))
            (let ((buf-name (format ein:notebook-buffer-name-template
                                    (ein:$notebook-url-or-port notebook)
                                    (ein:$notebook-notebook-name notebook))))
              (switch-to-buffer buf-name)
              (Then "I should be in buffer \"%s\"" buf-name))))))

(When "^I click on \"\\(.+\\)\"$"
      (lambda (word)
        ;; from espuds "go to word" without the '\\b's
        (goto-char (point-min))
        (let ((search (re-search-forward (format "\\[%s\\]" word) nil t))
              (message "Cannot go to link '%s' in buffer: %s"))
          (cl-assert search nil message word (buffer-string))
          (backward-char)
          (When "I press \"RET\"")
          (sit-for 0.8))))

(When "^I click on dir \"\\(.+\\)\"$"
      (lambda (dir)
        (When (format "I go to word \"%s\"" dir))
        (re-search-backward "Dir" nil t)
        (When "I press \"RET\"")
        (sit-for 0.8)
))

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
