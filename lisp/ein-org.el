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

;;;###autoload
(defun ein:org-open (link-path)
  "Open IPython notebook specified by LINK-PATH.
This function is to be used for FOLLOW function of
`org-add-link-type'."
  (error "Not implemented!"))

;;;###autoload
(defun ein:org-store-link ()
  "Call `org-store-link-props' when in notebook buffer.
This function is to be used for `org-store-link-functions'."
  (error "Not implemented!"))

;;;###autoload
(eval-after-load "org"
  '(progn
     (org-add-link-type "ipynb" 'ein:org-open)
     (add-hook 'org-store-link-functions 'ein:org-store-link)))


(provide 'ein-org)

;;; ein-org.el ends here
