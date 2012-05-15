;;; ein-completer.el --- Completion module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; ein-completer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-completer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-completer.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun ein:completer-beginning (matched-text)
  (save-excursion
    (re-search-backward (concat matched-text "\\="))))

(defun ein:completer-finish-completing (matched-text matches)
  (let* ((end (point))
         (beg (ein:completer-beginning matched-text))
         (word (if (and beg matches)
                   (completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

(provide 'ein-completer)

;;; ein-completer.el ends here
