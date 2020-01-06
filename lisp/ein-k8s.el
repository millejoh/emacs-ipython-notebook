;;; ein-k8s.el --- hooks to chrisbarrett's kubernetes*.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 The Authors

;; Authors: dickmao <github id: dickmao>

;; This file is NOT part of GNU Emacs.

;; ein-k8s.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-k8s.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-k8s.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'kubernetes)

(defcustom ein:k8s-name-deployment "jupyter-deployment"
  "Regexp by which we recognize jupyter services."
  :type 'string
  :group 'ein)

(defcustom ein:k8s-name-service "jupyter-service"
  "Regexp by which we recognize jupyter services."
  :type 'string
  :group 'ein)

(defun ein:k8s-select-context ()
  (interactive)
  (kubernetes-contexts-refresh-now)
  (if-let ((contexts (ein:k8s-get-contexts)))
      (let ((desired-context
             (ein:completing-read "Select context: " contexts nil t)))
        (kubernetes-state-clear)
        (let ((response (kubernetes-kubectl-await
                         (apply-partially #'kubernetes-kubectl
                                          kubernetes-props
                                          (kubernetes-state)
                                          (split-string (format "config use-context %s"
                                                                desired-context)))
                         (lambda (buf)
                           (with-current-buffer buf
                             (string-match (rx bol "Switched to context \""
                                               (group (+? nonl)) "\"." (* space) eol)
                                           (buffer-string))
                             (match-string 1 (buffer-string)))))))
          (if (string= response desired-context)
              (progn
                (kubernetes-state-update-config (kubernetes-kubectl-await-on-async
                                                 kubernetes-props
                                                 (kubernetes-state)
                                                 #'kubernetes-kubectl-config-view))
                (let ((current-name (alist-get
                                     'name
                                     (kubernetes-state-current-context
                                      (kubernetes-state)))))
                  (unless (string= current-name desired-context)
                    (error "ein:k8s-select-context': could not update state for %s"
                           desired-context))
                  (if (kubernetes-kubectl-await
                       (apply-partially #'kubernetes-kubectl
                                        kubernetes-props
                                        (kubernetes-state)
                                        (split-string "get nodes -o json"))
                       (lambda (buf)
                         (prog1 t
                           (with-current-buffer buf
                             (kubernetes-state-update-nodes
                              (json-read-from-string (buffer-string))))))
                       nil #'ignore)
                      (message "Selected %s" current-name)
                    (error "ein:k8s-select-context: %s is down" current-name))))
            (error "ein:k8s-select-context: use-context returned %s, expected %s"
                   response desired-context))))
    (error "ein:k8s-select-context: No contexts found")))

(defun ein:k8s-get-contexts ()
  (kubernetes-contexts-refresh-now)
  (let ((response (kubernetes-kubectl-await-on-async kubernetes-props
                                                     (kubernetes-state)
                                                     #'kubernetes-kubectl-config-view)))
    (-let* [((&alist 'contexts contexts 'current-context current) response)
            (names (--map (alist-get 'name it) (append contexts nil)))]
      (when (member current names)
        (setq names (cons current (-remove-item current names))))
      names)))

(defun ein:k8s-get-deployment ()
  (kubernetes-deployments-refresh-now)
  (-let* [(deployments (kubernetes-state-deployments (kubernetes-state)))
          ((&alist 'items items) deployments)]
    (seq-some (lambda (it)
                (-let [(&alist 'metadata (&alist 'name)) it]
                  (and (string= name ein:k8s-name-deployment) it)))
              items)))

(defun ein:k8s-get-pod ()
  (kubernetes-pods-refresh-now)
  (when-let ((deployment (ein:k8s-get-deployment)))
    (cl-first (kubernetes-overview--pods-for-deployment (kubernetes-state)
                                                        deployment))))
(defun ein:k8s-get-service ()
  (kubernetes-services-refresh-now)
  (-let* [(services (kubernetes-state-services (kubernetes-state)))
          ((&alist 'items items) services)]
    (seq-some (lambda (it)
                (-let [(&alist 'metadata (&alist 'name)) it]
                  (and (string= name ein:k8s-name-service) it)))
              items)))

(defun ein:k8s-get-node ()
  (kubernetes-nodes-refresh-now)
  (-when-let* ((pod (ein:k8s-get-pod))
               ((&alist 'spec (&alist 'nodeName)) pod)
               (node (kubernetes-state-lookup-node nodeName (kubernetes-state)))
               ((&alist 'metadata (&alist 'name)) node))
    node))

(defsubst ein:k8s-p ()
  (and (executable-find kubernetes-kubectl-executable)
       (or (kubernetes-state-current-context (kubernetes-state))
           (unless noninteractive
             (condition-case err
                 (ein:k8s-select-context)
               (error (ein:log 'info "ein:k8s-p %s" (error-message-string err))
                      nil))))))

(defsubst ein:k8s-in-cluster (addr)
  "Is ein client inside the k8s cluster?"
  (if-let ((ip-command (executable-find "ip")))
      (with-temp-buffer
        (apply #'call-process ip-command nil t nil
               (split-string (format "n ls %s" addr)))
        (goto-char (point-min))
        (search-forward addr nil t))
    ;; hack if ip command not found
    (string= "minikube"
             (alist-get 'name (kubernetes-state-current-context
                               (kubernetes-state))))))

(defun ein:k8s-service-url-or-port ()
  (-when-let* ((k8s-p (ein:k8s-p))
               (service (ein:k8s-get-service))
               ((&alist 'spec (&alist 'ports [(&alist 'nodePort)])) service)
               (node (ein:k8s-get-node))
               ((&alist 'status (&alist 'addresses)) node)
               (host-ip
                (seq-some (lambda (address)
                            (when (string= (alist-get 'type address) "InternalIP")
                              (alist-get 'address address)))
                          addresses)))
    (if (ein:k8s-in-cluster host-ip)
        (ein:url (concat "http://" host-ip ":" (number-to-string nodePort)))
      (when-let ((ips (kubernetes-kubectl-await-command ingress
                        (lambda (item)
                          (-let* (((&alist 'status
                                           (&alist 'loadBalancer
                                                   (&alist 'ingress
                                                           [(&alist 'ip)])))
                                   item))
                            ip)))))
        (ein:url (concat "http://" (car ips)))))))

(provide 'ein-k8s)
