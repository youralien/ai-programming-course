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

; last submission
(defmacro preserve (syms &rest body)
  (if (null (cdr syms))
      (car syms)
    `(let ,(mapcar #'(lambda (s)
                      `(,s (gensym)))
                   syms)
      ,@body)))

; ;PRESERVE: Illegal function object: (SETQ *READ-BASE* 2).
; (defmacro preserve (syms &rest body)
  ; `(let ,(mapcar #'(lambda (s)
  ;                   `(,s (gensym))) syms)
;     (let ((toexec (gensym))
;           (exprs ,body))
;       `(do ((,toexec ,exprs (cdr ,toexec)))
;            ((= 1 (length ,toexec)))
;           (car ,toexec)))))

; lambda (s) ,syms, ,body, @,syms
; (defmacro preserve (syms &rest body)
;   `(let ,(mapcar #'(lambda (s)
;                      `(,s (gensym)))
;                  syms)
;      ,@body)
;   syms)

; (defmacro preserve (syms &rest body)
;   `(let ,(mapcar #'(lambda (s) (,s (gensym))) ,syms) ,@body)

