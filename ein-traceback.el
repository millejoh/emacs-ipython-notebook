;;; ein-traceback.el --- Traceback module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-traceback.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-traceback.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-traceback.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)
(require 'ansi-color)

(require 'ein-utils)

(defclass ein:traceback ()
  ((tb-data :initarg :tb-data :type list)
   (buffer-name :initarg :buffer-name :type string)
   (buffer :initarg :buffer :type buffer)
   (ewoc :initarg :ewoc :type ewoc)))

(ein:deflocal ein:@traceback nil
  "Buffer local variable to store an instance of `ein:traceback'.")

(defun ein:tb-new (buffer-name)
  (ein:traceback "Traceback" :buffer-name buffer-name))

(defmethod ein:tb-get-buffer ((traceback ein:traceback))
  (unless (and (slot-boundp traceback :buffer)
               (buffer-live-p (oref traceback :buffer)))
    (let ((buf (get-buffer-create (oref traceback :buffer-name))))
      (oset traceback :buffer buf)))
  (oref traceback :buffer))

(defun ein:tb-pp (ewoc-data)
  (insert (ansi-color-apply ewoc-data)))

(defmethod ein:tb-render ((traceback ein:traceback) tb-data)
  (with-current-buffer (ein:tb-get-buffer traceback)
    (setq ein:@traceback traceback)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t)
          (ewoc (ewoc-create #'ein:tb-pp)))
      (erase-buffer)
      (oset traceback :ewoc ewoc)
      (oset traceback :tb-data tb-data)
      (mapc (lambda (data) (ewoc-enter-last ewoc data)) tb-data))
    (ein:traceback-mode)))

(defmethod ein:tb-popup ((traceback ein:traceback) tb-data)
  (ein:tb-render traceback tb-data)
  (pop-to-buffer (ein:tb-get-buffer traceback)))

(defmethod ein:tb-range-of-node-at-point ((traceback ein:traceback))
  (let* ((ewoc (oref traceback :ewoc))
         (ewoc-node (ewoc-locate ewoc))
         (beg (ewoc-location ewoc-node))
         (end (ein:aand (ewoc-next ewoc ewoc-node) (ewoc-location it))))
    (list beg end)))

(defmethod ein:tb-file-path-at-point ((traceback ein:traceback))
  (destructuring-bind (beg end)
      (ein:tb-range-of-node-at-point traceback)
    (let* ((file-tail (next-single-property-change beg 'font-lock-face nil end))
           (file (when file-tail
                   (buffer-substring-no-properties beg file-tail))))
      (if (string-match "\\.pyc$" file)
          (concat (file-name-sans-extension file) ".py")
        file))))

(defmethod ein:tb-file-lineno-at-point ((traceback ein:traceback))
  (destructuring-bind (beg end)
      (ein:tb-range-of-node-at-point traceback)
    (when (save-excursion
            (goto-char beg)
            (search-forward-regexp "^[-]+> \\([0-9]+\\)" end t))
      (string-to-number (match-string 1)))))

(defmethod ein:tb-jump-to-source-at-point ((traceback ein:traceback)
                                           &optional select)
  (let ((file (ein:tb-file-path-at-point traceback))
        (lineno (ein:tb-file-lineno-at-point traceback)))
    (assert (file-exists-p file) nil "File %s does not exit." file)
    (let ((buf (find-file-noselect file))
          (scroll (lambda ()
                    (goto-char (point-min))
                    (forward-line (1- lineno)))))
      (if select
          (progn (pop-to-buffer buf)
                 (funcall scroll))
        (with-selected-window (display-buffer buf)
          (funcall scroll))))))

(defun ein:tb-jump-to-source-at-point-command (&optional select)
  (interactive "P")
  (ein:tb-jump-to-source-at-point ein:@traceback select))


;;; ein:traceback-mode

(defun ein:tb-prev-item ()
  (interactive)
  (ewoc-goto-prev (oref ein:@traceback :ewoc) 1))

(defun ein:tb-next-item ()
  (interactive)
  (ewoc-goto-next (oref ein:@traceback :ewoc) 1))

(define-derived-mode ein:traceback-mode fundamental-mode "ein:tb"
  (font-lock-mode))

(let ((map ein:traceback-mode-map))
  (define-key map (kbd "RET") 'ein:tb-jump-to-source-at-point-command)
  (define-key map "p" 'ein:tb-prev-item)
  (define-key map "n" 'ein:tb-next-item)
  (define-key map "q" 'bury-buffer))

(provide 'ein-traceback)

;;; ein-traceback.el ends here
