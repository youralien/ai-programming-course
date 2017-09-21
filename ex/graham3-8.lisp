(defun show-dots (lst)
  (show-dots-helper lst 0))

(defun show-dots-helper (lst n)
  (cond ((null lst) (progn
                      (format t "NIL")
                      (do ((i 0 (1+ i)))
                          ((= i n))
                        (format t ")"))))
        ((not (listp lst)) (format t "~A)" lst))
        ((listp (car lst)) (progn
                             (format t "(")
                             (show-dots-helper (car lst) 0)
                             (format t " . ")
                             (show-dots-helper (cdr lst) (1+ n))))
        (t (progn
             (format t "(~A . " (car lst))
             (show-dots-helper (cdr lst) (1+ n))))))

  