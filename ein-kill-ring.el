;;; ein-kill-ring.el --- Kill-ring for cells

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-kill-ring.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-kill-ring.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kill-ring.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stolen from simple.el.

;;; Code:

(defvar ein:kill-ring nil)
(defvar ein:kill-ring-yank-pointer nil)
(defvar ein:kill-ring-max kill-ring-max)

(defun ein:kill-new (obj)
  "Make OBJ the latest kill in the kill ring `ein:kill-ring'.
Set `ein:kill-ring-yank-pointer' to point to it."
  (push obj ein:kill-ring)
  (if (> (length ein:kill-ring) ein:kill-ring-max)
      (setcdr (nthcdr (1- ein:kill-ring-max) ein:kill-ring) nil))
  (setq ein:kill-ring-yank-pointer ein:kill-ring))

(defun ein:current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually
move the yanking point; just return the Nth kill forward."
  (unless ein:kill-ring (error "Kill ring is empty"))
  (let ((ARGth-kill-element
         (nthcdr (mod (- n (length ein:kill-ring-yank-pointer))
                      (length ein:kill-ring))
                 ein:kill-ring)))
    (unless do-not-move
      (setq ein:kill-ring-yank-pointer ARGth-kill-element))
    (car ARGth-kill-element)))

(provide 'ein-kill-ring)

;;; ein-kill-ring.el ends here
