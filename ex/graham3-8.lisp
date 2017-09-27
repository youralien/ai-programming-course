(defun show-dots (lst)
  (cond ((atom lst) (format t "~A" lst))
        (t (format t "(")
           (show-dots (car lst))
           (format t " . ")
           (show-dots (cdr lst))
           (format t ")"))))

(defun show-list (lst)
  (cond ((atom lst) (format t "~A" lst))
        (t (format t "[")
           (cond ((atom (car lst)) (format t "~A" (car lst)))
                 (t (show-list (car lst))))
           (do ((rest (cdr lst) (cdr rest)))
                ((atom rest) (unless (null rest)
                               (format t " . ~A" rest))
                 (format t "]"))
               (cond ((atom (car rest)) (format t " ~A" (car rest)))
                     (t (format t " ")
                        (show-list (car rest))))))))
