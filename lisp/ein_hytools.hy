
;; Hy utilities useable from ein.

;; Copyright (C) 2020- John Miller

;; Author: John Miller <millejoh at mac.com>

;; ein_hytools.hy is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; ein_hytools.hy is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; ein.py. If not, see <http://www.gnu.org/licenses/>.


(setv --ein-hytools-version-- "1.0.0")

(try
  (import [matplotlib [rc :as --ein-rc
                       rcParams as --ein-rcParams]])
  (setv ein-matplotlib-available True)
  (except [ImportError]
    (setv ein-matplotlib-available False)))


(defn ein-find-edit-target [name]
  (import [inspect [getsourcefile getsourcelines]])
  (import hy)
  (try
    (setv obj (eval (hy.read-str name)))
    (except [NameError]
      (return False))
    (else
      (setv sfile (getsourcefile obj))
      (setv sline (get (getsourcelines obj) -1))
      (if (and sfile sline)
          (return (, sfile sline False))
          (return False)))))

(defn ein-find-source [name]
  (setv ret (--ein-find-edit-target name))
  (if ret
      (do
        (setv (, filename lineno use-temp) ret)
        (if (not use-temp)
            (do
              (print filename)
              (print lineno)
              (return)))))
  (raise (RuntimeError (.format "Source code for {0} cannot be found" name))))


(defn ein-print-object-info-for [obj]
  (import json)
  (import [IPython.core [oinspect]])
  (setv inspector (oinspect.Inspector))
  (try
    (setv oinfo (inspector.info obj))
    (except [Exception]
      (setv oinfo (inspector.info None))))
  (print (json.dumps oinfo)))
