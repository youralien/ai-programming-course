(defun greater (x y)
  (if (> x y)
      x
    y))

(defun has-list-p (lst)
  (cond ((null lst) nil)
        ((listp (car lst)) t)
        (t (has-list-p (cdr lst)))))

(defun print-dots (n)
  (do ((i 1 (1+ i)))
      ((> i n) nil)
    (format t ".")))

(defun print-dots (n)
  (cond ((< n 1) nil)
        (t (format t ".") (print-dots (1- n) ))))

(defun get-a-count (lst)
  (do ((i 0 (1+ i))
       (rest lst (cdr rest))
       (count 0 (+ (if (eql (car rest) 'a)
                       1
                     0)
                   count)))
      ((null rest) count)))

(defun get-a-count (lst)
  (cond ((null lst) 0)
        (t (+ (get-a-count (cdr lst))
              (if (eql (car lst) 'a)
                  1
                0)))))

;;; The following function is wrong because apply was not passed a list without nils because
;;; the symbol lst was not modified by remove! The output of the remove statement needed to be
;;; captured in some way, either in another variable or by passing the output directly to the
;;; apply
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
  (cond ((null lst) 0)
        ((null (car lst)) (summit (cdr lst)))
        (t (+ (car lst) (summit (cdr lst))))))
