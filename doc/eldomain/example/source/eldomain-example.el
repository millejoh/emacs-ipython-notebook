(defgroup eldomain-example nil
  "Group for `eldomain-example' package."
  :group 'application
  :prefix "ee:")

(defface ee:face-example
  '((t :inherit header-line))
  "Face documentation example."
  :group 'eldomain-example)

(defvar ee:variable-example nil
  "Variable documentation example referring to `ee:function-example'.")

(defmacro ee:macro-example (x y &optional z &rest args)
  "Macro documentation example referring to `ee:variable-example'.")

(defun ee:function-example (x y &optional z &rest args)
  "Function documentation example referring to `ee:macro-example'.")

(provide 'eldomain-example)
