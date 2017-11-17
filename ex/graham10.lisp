(defmacro nth-expr (n &rest exprs)
  `(case ,n
    ,@(let ((key 0))
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

(defmacro n-of (n expr)
  (let ((counter (gensym))
        (stopsym (gensym))
        (accum (gensym)))
    `(let ((,stopsym ,n))
      (do ((,counter 0 (1+ ,counter))
           (,accum nil (cons ,expr ,accum)))
          ((>= ,counter ,stopsym) (reverse ,accum))))))