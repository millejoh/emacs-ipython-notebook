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
            (names (--map (alist-get 'name it) (append contexts nil)))
            (state (kubernetes-state))
            (current (alist-get 'name (kubernetes-state-current-context state)))]
      (when (member current names)
        (setq names (cons current (-remove-item current names))))
      names)))

(defun ein:k8s-get-deployment ()
  (kubernetes-deployments-refresh-now)
  (-let* [(deployments (kubernetes-state-deployments (kubernetes-state)))
          ((&alist 'items items) deployments)]
    (seq-some (lambda (it)
                (-let [(&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                               'spec (&alist 'replicas desired)
                               'status (&alist 'replicas current
                                               'availableReplicas available
                                               'updatedReplicas up-to-date))
                       it]
                  (and (string= name ein:k8s-name-deployment) it)))
              items)))

(defun ein:k8s-get-service ()
  (kubernetes-services-refresh-now)
  (-let* [(services (kubernetes-state-services (kubernetes-state)))
          ((&alist 'items items) services)]
    (seq-some (lambda (it)
                (-let [(&alist 'metadata (&alist 'name 'creationTimestamp)
                               'spec (&alist 'selector)
                               'status)
                       it]
                  (and (string= name ein:k8s-name-service) it)))
              items)))

(defun ein:k8s-service-url-or-port ()
  (when-let ((service (ein:k8s-get-service)))
    (-let [(&alist 'spec (&alist 'ports [(&alist 'nodePort)])) service]
      nodePort)))

(defun ein:k8s-get-node ()
  (kubernetes-nodes-refresh-now)
  (when-let ((pod (ein:k8s-get-pod)))
    (-let* (((&alist 'spec (&alist 'nodeName)) pod))
      (when-let ((node (kubernetes-state-lookup-node
                        nodeName
                        (kubernetes-state))))
        (-let (((&alist 'metadata (&alist 'name)) node))
          name)
        node))))

(defun ein:k8s-get-pod ()
  (kubernetes-pods-refresh-now)
  (when-let ((deployment (ein:k8s-get-deployment)))
    (cl-first (kubernetes-overview--pods-for-deployment (kubernetes-state)
                                                        deployment))))

(provide 'ein-k8s)
