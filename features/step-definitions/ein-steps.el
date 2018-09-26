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

(When "^I enable undo$"
      (lambda ()
        (setq ein:worksheet-enable-undo t)))

(When "^I undo demoting errors$"
      (lambda ()
        (with-demoted-errors "demoted: %s"
          (undo))))
