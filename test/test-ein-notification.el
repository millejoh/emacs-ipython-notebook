(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notification)

(defun ein:testing-notification-tab-mock ()
  (make-instance 'ein:notification-tab
                 :get-list (lambda () '(a b c))
                 :get-current (lambda () 'a)
                 :get-name #'ignore))

(ert-deftest ein:header-line-normal ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (kernel (oref ein:%notification% :kernel)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (should (equal (ein:header-line)
                   "IP[y]: /1\\ /2\\ /3\\ [+]"))))

(ert-deftest ein:header-line-kernel-status-busy ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (kernel (oref ein:%notification% :kernel)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (ein:notification-status-set kernel
                                 'status_busy.Kernel)
    (should (equal (ein:header-line)
                   "IP[y]: Kernel is busy... | /1\\ /2\\ /3\\ [+]"))))

(ert-deftest ein:header-line-notebook-status-busy ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (notebook (oref ein:%notification% :notebook)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (ein:notification-status-set notebook
                                 'notebook_saved.Notebook)
    (should (equal (ein:header-line)
                   "IP[y]: Notebook is saved | /1\\ /2\\ /3\\ [+]"))))

(ert-deftest ein:header-line-notebook-complex ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (kernel (oref ein:%notification% :kernel))
         (notebook (oref ein:%notification% :notebook)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (ein:notification-status-set kernel
                                 'status_dead.Kernel)
    (ein:notification-status-set notebook
                                 'notebook_saving.Notebook)
    (should (equal
             (ein:header-line)
             (concat "IP[y]: Saving Notebook... | "
                     "Kernel is dead. Need restart. | "
                     "/1\\ /2\\ /3\\ [+]")))))

(ert-deftest ein:notification-and-events ()
  (let* ((notification (ein:notification "NotificationTest"))
         (kernel (oref notification :kernel))
         (notebook (oref notification :notebook))
         (events (ein:events-new))
         (event-symbols
          '(notebook_saved.Notebook
            notebook_saving.Notebook
            notebook_save_failed.Notebook
            execution_count.Kernel
            status_restarting.Kernel
            status_idle.Kernel
            status_busy.Kernel
            status_dead.Kernel
            ))
         (callbacks (oref events :callbacks)))
    (ein:notification-bind-events notification events)
    (mapc (lambda (s) (should (gethash s callbacks))) event-symbols)
    (should (= (hash-table-count callbacks) (length event-symbols)))
    (should (equal (oref kernel :status) nil))
    (loop for et in (mapcar #'car (oref kernel :s2m))
          do (ein:events-trigger events et)
          do (should (equal (oref kernel :status) et))
          do (should (equal (oref notebook :status) nil)))
    (loop for et in (mapcar #'car (oref notebook :s2m))
          do (ein:events-trigger events et)
          do (should (equal (oref kernel :status) 'status_dead.Kernel))
          do (should (equal (oref notebook :status) et)))))
