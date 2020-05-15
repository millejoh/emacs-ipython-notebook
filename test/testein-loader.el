;; -*- lexical-binding:t -*-
(load "testein.el" nil t)
(dolist (file (directory-files (file-name-directory load-file-name) t "test-ein.*\\.el"))
  (load file nil t))
