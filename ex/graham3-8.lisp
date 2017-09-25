(defun show-dots (lst)
  (cond ((or (null lst) (not (listp lst))) (format t "~A" lst))
        (t (format t "(")
           (show-dots (car lst))
           (format t " . ")
           (show-dots (cdr lst))
           (format t ")"))))

(defun show-list (lst)
  (show-new-list lst))

(defun show-new-list (lst)
  (cond ((or (null lst) (not (listp lst))) (format t "~A" lst))
        (t (format t "[")
           (show-continued-list lst)
           (format t "]"))))

(defun show-rest-list (lst)
 (unless (null (cdr lst))
   (format t " ")
   (unless (listp (cdr lst))
     (format t ". "))
   (show-continued-list (cdr lst))))

(defun show-continued-list (lst)
  (cond ((not (listp lst)) (format t "~A" lst)) ; 'NIL, 12'
        ((listp (car lst)) (show-new-list (car lst))
         (show-rest-list lst))
        (t (format t "~A" (car lst))
           (show-rest-list lst))))

