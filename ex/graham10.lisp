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

; (defmacro n-of (n &rest expr)
;   )


(defmacro n-of (n expr)
  (let ((g (gensym))
        (h (gensym))
        (i (gensym)))
    `(let ((,h ,n))
      (do ((,g 0 (1+ ,g))
           (,i nil (cons ,expr ,i)))
          ((>= ,g ,h) (reverse ,i))))))