;;; ein-ipdb.el --- Support ipython debugger (ipdb)

;; Copyright (C) 2015 - John Miller

;; Author: John Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-ipdb.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ipdb.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar *ein:ipd-session-buffer* nil)

(defun ein:run-ipdb-session (kernel packet)
  (let* ((command (read-from-minibuffer "ipdb> "))
         (content (list :value command))
         (msg (ein:kernel--get-msg kernel "input_reply" content)))
    (plist-put (plist-get msg :header) :msg_id (plist-get (plist-get packet :header) :msg_id))
    (ein:websocket-send-stdin-channel kernel msg)))

(provide 'ein-ipdb)
