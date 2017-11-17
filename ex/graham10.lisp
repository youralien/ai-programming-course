(defmacro nth-expr (n &rest exprs)
  `(case (1- ,n)
    ,@(let ((key -1))
      (mapcar #'(lambda (expr)
                  `(,(incf key) ,expr))
              exprs))))

; nth-expr inspired by random-choice
(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
    ,@(let ((key -1))
      (mapcar #'(lambda (expr)
                  `(,(incf key) ,expr))
              exprs))))
