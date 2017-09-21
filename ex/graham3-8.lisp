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

(defun show-list (lst)
  (show-new-list lst))

(defun show-new-list (lst)
  (cond ((or (null lst) (not (listp lst))) (format t "~A" lst))
        (t (format t "[")
           (show-continued-list lst)
           (format t "]"))))

(defun show-continued-list (lst)
  (cond ((not (listp lst)) (format t "~A" lst))
        ((and (listp (car lst)) (null (cdr lst)))
         (show-new-list (car lst)))
        ((and (listp (car lst)) (not (null (cdr lst))))
         (show-new-list (car lst))
         (format t " ")
         (show-continued-list (cdr lst)))
        ((and (not (listp (car lst))) (null (cdr lst)))
         (format t "~A" (car lst)))
        (t
         (format t "~A " (car lst))
         (show-continued-list (cdr lst)))))