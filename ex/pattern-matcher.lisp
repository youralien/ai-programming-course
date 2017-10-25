;; to enable tests, do (in-package #:match-tests) in the lisp listener

(in-package #:exmatch)

; (defun ?not (x y lsts)
;   ; (format t "~C x: ~A y: ~A ~C" #\linefeed x y #\linefeed)
;   (let ((match-res (match-p (car x) y lsts)))
;     ; (format t "~C match res: ~A ~C" #\linefeed match-res #\linefeed)
;     ; (format t "~C lsts: ~A ~C" #\linefeed lsts #\linefeed)
;     (cond 
;       ((null match-res) lsts)
;       (t nil))))

(defun ?not (x y lsts)
  (if (match-p (car x) y lsts)
      nil
    lsts))

(defun ?or (x y lsts)
  (mapcan #'(lambda (ptrn)
             (match-p ptrn y lsts))
          x))

(defun ?= (x y lsts)
  (destructuring-bind (sub-pattern function-name . args) x
    (match-p sub-pattern (apply function-name (cons y args)) lsts)))

; (defun ?contains (x y lsts)
;   (mapcan #'(lambda (subexp)
;              (match-p '))

  ; (do* ((head (car y) (car remain))
  ;       (remain (cdr y) (cdr remain))
  ;       (out nil ))
  ;      ((null remain) )
      
  ;     (nil))

(defun subexp (y)
  (do ((i 0 (1+ i))
        (expr y (if (atom expr) nil (cdr expr)))
        (total nil
             (cond
               ((atom expr) (cons expr total))
               ((null (cdr expr))
                  (cond ((and (equal y expr) (atom (car expr)))  ; '(nil) or '(a)
                         (cons (car expr) (cons expr total)))
                        ((atom (car expr)) ; '(b)
                         (cons (car expr) total))
                        (t                 ; '((a))
                         (append (cons expr total) (subexp (car expr))))))
               (t (append (cons expr total) (subexp (car expr)))))))
       ((null expr) (or total (list nil)))))
