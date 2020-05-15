;; -*- lexical-binding:t -*-
(require 'ert)

(require 'ein-notification)

(defun ein:testing-notification-tab-mock ()
  (make-instance 'ein:notification-tab
                 :get-list (lambda () '(a b c))
                 :get-current (lambda () 'a)))

(ert-deftest ein:header-line-normal ()
  (let* ((ein:%notification% (ein:notification))
         (kernel (oref ein:%notification% :kernel)))
    (ignore kernel)
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (should (string-prefix-p "IP[y]: " (ein:header-line)))))

(ert-deftest ein:header-line-kernel-status-busy ()
  (let* ((ein:%notification% (ein:notification))
         (kernel (oref ein:%notification% :kernel)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (ein:notification-status-set kernel
                                 'status_busy.Kernel)
    (should (string-prefix-p "IP[y]: Kernel busy..."
                             (ein:header-line)))))

(ert-deftest ein:header-line-notebook-status-busy ()
  (let* ((ein:%notification% (ein:notification))
         (notebook (oref ein:%notification% :notebook)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (ein:notification-status-set notebook
                                 'notebook_saved.Notebook)
    (should (string-prefix-p "IP[y]: Notebook saved"
                             (ein:header-line)))))

(ert-deftest ein:header-line-notebook-complex ()
  (let* ((ein:%notification% (ein:notification))
         (kernel (oref ein:%notification% :kernel))
         (notebook (oref ein:%notification% :notebook)))
    (oset ein:%notification% :tab (ein:testing-notification-tab-mock))
    (ein:notification-status-set kernel
                                 'status_dead.Kernel)
    (ein:notification-status-set notebook
                                 'notebook_saving.Notebook)
    (should (string-prefix-p
             (concat "IP[y]: Saving notebook... | "
                     (substitute-command-keys "Kernel requires restart \\<ein:notebook-mode-map>\\[ein:notebook-restart-session-command-km]"))
	     (ein:header-line)))))

(ert-deftest ein:notification-and-events ()
  (let* ((notification (ein:notification))
         (kernel (oref notification :kernel))
         (notebook (oref notification :notebook))
         (events (ein:events-new))
         (event-symbols
          '(notebook_saved.Notebook
            notebook_saving.Notebook
            notebook_save_failed.Notebook
            execution_count.Kernel
            status_idle.Kernel
            status_busy.Kernel
            status_restarting.Kernel
            status_restarted.Kernel
            status_dead.Kernel
            status_reconnecting.Kernel
            status_reconnected.Kernel
            status_disconnected.Kernel
            ))
         (callbacks (oref events :callbacks)))
    (ein:notification-bind-events notification events)
    (mapc (lambda (s) (should (gethash s callbacks))) event-symbols)
    (should (= (hash-table-count callbacks) (length event-symbols)))
    (should (equal (oref kernel :status) nil))
    (cl-loop for et in (mapcar #'car (oref kernel :s2m))
          do (ein:events-trigger events et)
          do (should (equal (oref kernel :status) et))
          do (should (equal (oref notebook :status) nil)))
    (cl-loop for et in (mapcar #'car (oref notebook :s2m))
          do (ein:events-trigger events et)
          do (should (equal (oref kernel :status) 'status_disconnected.Kernel))
          do (should (equal (oref notebook :status) et)))))
