;;; ein-org.el --- Org-mode link support for EIN

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-org.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-org.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-org.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'ein-notebooklist)

;; FIXME: Separate org-unrelated cores from the following code and
;; expose them as API in ein-link.el.

(defun* ein:org-goto-link (notebook created
                                    &key
                                    worksheet-index
                                    search
                                    &allow-other-keys)
  (if created
      (ein:log 'info "Linked notebook did not exist.  Created a new one.")
    (if worksheet-index
        (ein:notebook-worksheet-open-ith notebook worksheet-index
                                         #'pop-to-buffer)
      (pop-to-buffer (ein:notebook-buffer notebook)))
    (when search
      (goto-char (point-min))
      (search-forward search nil t))
    ;; More to come here:
    ))

;;;###autoload
(defun ein:org-open (link-path)
  "Open IPython notebook specified by LINK-PATH.
This function is to be used for FOLLOW function of
`org-add-link-type'."
  (let ((link (read link-path)))
    (destructuring-bind (&key url-or-port name &allow-other-keys)
        link
      (ein:notebooklist-open-notebook-by-name name url-or-port
                                              #'ein:org-goto-link link))))

;;;###autoload
(defun ein:org-store-link ()
  "Call `org-store-link-props' when in notebook buffer.
This function is to be used for `org-store-link-functions'.

Examples::

  ipynb:(:url-or-port 8888 :name \"My_Notebook\")
  ipynb:(:url-or-port \"http://notebook-server\" :name \"My_Notebook\")

Note that spaces will be escaped in org files.

As how IPython development team supports multiple directory in
IPython notebook server is unclear, it is not easy to decide the
format for notebook links.  Current approach is to use
S-expression based (rather verbose) serialization, so that
extending link spec without loosing backward compatibility is
easier.  For the examples of link format in general, see Info
node `(org) External links' and Info node `(org) Search options'"
  (ein:and-let* (((ein:worksheet-buffer-p))
                 (notebook (ein:get-notebook))
                 (name (ein:notebook-name notebook))
                 (link (list :url-or-port (ein:get-url-or-port)
                             :name name))
                 (description name))
    (ein:aif (ein:notebook-worksheet-index notebook)
        (unless (= it 0)
          (plist-put link :worksheet-index it))
      (error "[ein] Cannot link to scratch sheet!"))
    (when (region-active-p)
      ;; FIXME: It does not work when selecting muli-line.
      (plist-put link :search (buffer-substring-no-properties
                               (region-beginning) (region-end))))
    (org-store-link-props
     :type "ipynb"
     :link (let ((print-length nil)
                 (print-level nil))
             (format "ipynb:%S" link))
     :description description)))

;;;###autoload
(eval-after-load "org"
  '(progn
     (org-add-link-type "ipynb" 'ein:org-open)
     (add-hook 'org-store-link-functions 'ein:org-store-link)))
;; The above expression is evaluated via loaddef file.  At the moment,
;; org.el nor ein-org.el need not be loaded.  When org-mode is used,
;; the above `progn' is executed but still ein-org.el is not loaded.
;; When `ein:org-open' or `ein:org-store-link' is called for opening
;; or storing ipynb link, ein-org.el is loaded finally.  (When
;; ein-org.el is loaded the above expression is evaluated again, but
;; that's OK as the expression is idempotent.)


(provide 'ein-org)

;;; ein-org.el ends here
