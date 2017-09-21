(defun show-dots (lst)
  (show-dots-helper lst 0))

(defun show-dots-helper (lst n)
  (cond ((or (null lst) (not (listp lst))) (format t "~A" lst)
         (do ((i 0 (1+ i)))
             ((= i n))
           (format t ")")))
        ((listp (car lst)) (format t "(")
         (show-dots-helper (car lst) 0)
         (format t " . ")
         (show-dots-helper (cdr lst) (1+ n)))
        (t (format t "(~A . " (car lst))
           (show-dots-helper (cdr lst) (1+ n)))))

