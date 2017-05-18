;;; ein-file.el --- Editing files downloaded from jupyter

;; Copyright (C) 2017- John M. Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-file.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-file.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebooklist.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


(defvar *ein:file-buffername-template* "'/ein:%s:%s")
(ein:deflocal ein:content-file-buffer--content nil)

;; (push '("^ein:.*" . ein:content-file-handler)
;;       file-name-handler-alist)

(defun ein:file-buffer-name (urlport path)
  (format *ein:file-buffername-template*
          urlport
          path))

(defun ein:file-open (url-or-port path)
  (ein:content-query-contents path url-or-port nil
                              #'ein:file-open-finish))

(defun ein:file-open-finish (content)
  (with-current-buffer (get-buffer-create (ein:file-buffer-name (ein:$content-url-or-port content)
                                                                (ein:$content-path content)))
    (setq ein:content-file-buffer--content content)
    (let ((raw-content (ein:$content-raw-content content)))
      (if (eql system-type 'windows-nt)
          (insert (decode-coding-string raw-content 'utf-8))
        (insert raw-content)))
    (set-visited-file-name (buffer-name))
    (set-auto-mode)
    (add-hook 'write-contents-functions 'ein:content-file-save) ;; FIXME Brittle, will not work
                                                                ;; if user changes major mode.
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (pop-to-buffer (buffer-name))))

(defun ein:content-file-save ()
  (when (boundp 'ein:content-file-buffer--content)
    (setf (ein:$content-raw-content ein:content-file-buffer--content) (buffer-string))
    (ein:content-save ein:content-file-buffer--content)
    (set-buffer-modified-p nil)
    t))

(provide 'ein-file)

