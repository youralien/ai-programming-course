(defun has-number-p (expr)
  (cond
    ((atom expr) (numberp expr))
    (t (some #'has-number-p expr))))