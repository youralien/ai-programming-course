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
(defun print-dots (n)
  (do ((i 1 (1+ i)))
      ((> i n) nil)
    (format t ".")))

;;; recursive
(defun print-dots (n)
  (cond ((< n 1) nil)
        (t (format t ".") (print-dots (1- n) ))))

;;; takes a list and returns the number of times the symbol `a` occurs in it
;;; iterative
(defun nth-is-a (n lst)
  (if (eql 'a (nth n lst))
      1
    0))
(defun get-a-count (lst)
  (do ((i 0 (1+ i))
       (count 0 (+ (nth-is-a i lst) count)))
      ((>= i (length lst)) count)))
    

;;; recursive
(defun head-is-a (lst)
  (if (eql (car lst) 'a)
      1
    0))
(defun get-a-count (lst)
  (cond ((null lst) 0)
        (t (+ (get-a-count (cdr lst)) (head-is-a lst)))))

;;; The following function is wrong because remove does not mutate lst
;;;(defun summit (1st)
;;;  (remove nil 1st)
;;;  (apply #'+ 1st))
;;; We can fix the function like so
(defun summit (1st)
  (apply #'+ (remove nil 1st)))

;;; The following function is wrong because the base case of when the lst is null is not handled.
;;; Recursion leads to a stack overflow.
;;;(defun summit (1st)
;;;  (let ((x (car 1st)))
;;;    (if (null x)
;;;        (summit (cdr 1st))
;;;      (+ x (summit (cdr 1st))))))
;;; We can fix the function like so...
(defun summit (lst)
  (let ((x (car lst)))
    (cond ((and (null x) (null (cdr lst))) 0)
          ((and (null x) (not (null (cdr lst)))) (summit (cdr lst)))
          (t (+ x (summit (cdr lst)))))))

;;; HOW ARE THESE DIFFERENT?!?!
;;;(defun summit (lst)
;;;  (let ((x (car lst)))
;;;    (cond ((null x) 0)
;;;          (t (+ x (summit (cdr lst)))))))
;;;
;;;(defun summit (1st)
;;;  (let ((x (car 1st))
;;;        (rest (cdr lst)))
;;;    (cond ((null x) 0)
;;;          (t (+ x (summit rest))))))

