;;; ein-pseudo-console.el --- Pseudo console mode

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-pseudo-console.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ein-pseudo-console.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pseudo-console.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar ein:pseudo-console-mode-map (make-sparse-keymap))

(let ((map ein:pseudo-console-mode-map))
  (define-key map "\C-m" 'ein:worksheet-execute-cell-and-insert-below))

;;;###autoload
(define-minor-mode ein:pseudo-console-mode
  "Pseudo console mode.  Hit RET to execute code."
  :lighter " ein:pseudo"
  :keymap ein:pseudo-console-mode-map
  :group 'ein)

;; To avoid MuMaMo to discard `ein:pseudo-console-mode', make it
;; permanent local.
(put 'ein:pseudo-console-mode 'permanent-local t)

(provide 'ein-pseudo-console)

;;; ein-pseudo-console.el ends here
