;;; Do excercises 4, 7, 8, 9
;;; Using names greater, has-list-p, print-dots, get-a-count, summit
;;; I had only read up to 2.8 when trying these, as a warm down excercise for bed'

;;; Define a function that takes two arguments and returns the greater of the  
(defun greater (x y)
  (if (> x y)
      x
    y))

;;; define a function that takes a list as an argument and returns true if one of its elements is a list'
(defun has-list-p (lst)
  (cond ((null lst) nil)
        ((listp (car lst)) t)
        (t (has-list-p (cdr lst)))))
;;; old way
;;;(defun has-list-p (lst)
;;;  (if (null lst)
;;;      nil
;;;    (if (listp (car lst))
;;;        t
;;;      (has-list-p (cdr lst)))))

;;; takes a positive integer and prints that many dots
;;; iterative

;;; recursive
;;;(defun print-dots (n)
;;;  (if (eql n 1)
;;;      .
;;;    (if (- n 1) .)))
