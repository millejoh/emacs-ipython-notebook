(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-notification)

(ert-deftest ein-header-line-kernel-status-busy ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (kernel (oref ein:%notification% :kernel)))
    (ein:notification-status-set kernel
                                 'status_busy.Kernel)
    (should (equal (ein:header-line) "IP[y]: Kernel is busy..."))))

(ert-deftest ein-header-line-notebook-status-busy ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (notebook (oref ein:%notification% :notebook)))
    (ein:notification-status-set notebook
                                 'notebook_saved.Notebook)
    (should (equal (ein:header-line) "IP[y]: Notebook is saved"))))

(ert-deftest ein-header-line-notebook-complex ()
  (let* ((ein:%notification% (ein:notification "NotificationTest"))
         (kernel (oref ein:%notification% :kernel))
         (notebook (oref ein:%notification% :notebook)))
    (ein:notification-status-set kernel
                                 'status_dead.Kernel)
    (ein:notification-status-set notebook
                                 'notebook_saving.Notebook)
    (should (equal
             (ein:header-line)
             "IP[y]: Saving Notebook... | Kernel is dead. Need restart."))))

(ert-deftest ein-notification-and-events ()
  (let* ((notification (ein:notification "NotificationTest"))
         (kernel (oref notification :kernel))
         (notebook (oref notification :notebook))
         (events (ein:events "EventsTest"
                             :buffer (get-buffer-create "*dummy buffer*"))))
    (ein:notification-bind-events notification events)
    (should (= (hash-table-count (oref events :callbacks)) 7))
    (should (equal (oref kernel :status) nil))
    (loop for et in (mapcar #'car (oref kernel :s2m))
          do (ein:events-trigger events et)
          do (should (equal (oref kernel :status) et))
          do (should (equal (oref notebook :status) nil)))
    (loop for et in (mapcar #'car (oref notebook :s2m))
          do (ein:events-trigger events et)
          do (should (equal (oref kernel :status) 'status_dead.Kernel))
          do (should (equal (oref notebook :status) et)))))
