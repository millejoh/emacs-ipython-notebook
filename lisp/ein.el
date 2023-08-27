;;; ein.el --- jupyter notebook client    -*- lexical-binding:t -*-

;; Copyright (C) 2012-2019 The Authors of the Emacs IPython Notebook (EIN)

;; Authors:  dickmao <github id: dickmao>
;;           John Miller <millejoh at millejoh.com>
;;           Takafumi Arakaki <aka.tkf at gmail.com>
;; Version: 0.17.1pre
;; Package-Requires: ((emacs "26.1") (websocket "1.12") (anaphora "1.0.4") (request "0.3.3") (deferred "0.5") (polymode "0.2.2") (dash "2.13.0") (with-editor "0pre"))
;; URL: https://github.com/dickmao/emacs-ipython-notebook
;; Keywords: jupyter, literate programming, reproducible research

;; This file is NOT part of GNU Emacs.

;; ein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs IPython Notebook (EIN), despite its name, is a jupyter client for all
;; languages.  It does not work under non-WSL Windows environments.
;;
;; As of 2023, EIN has been sunset for a number of years having been
;; unable to keep up with jupyter's web-first ecosystem.  Even during
;; its heyday EIN never fully reconciled emac's monolithic buffer
;; architecture to the notebook's by-cell discretization, leaving
;; gaping functional holes like crippled undo.
;;
;; Certainly in 2012 when jupyter was much smaller, an emacs client
;; made perfect sense.  With many years of hindsight, it's now clear
;; the json-driven, git-averse notebook format is anathema to emacs's
;; plain text ethos.

;;; Code:

(when (boundp 'mouse-buffer-menu-mode-groups)
  (add-to-list 'mouse-buffer-menu-mode-groups
               '("^ein:" . "ein")))

(provide 'ein)

;;; ein.el ends here
