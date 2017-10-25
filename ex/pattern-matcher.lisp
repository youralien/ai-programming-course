;; to enable tests, do (in-package #:match-tests) in the lisp listener

(in-package #:exmatch)

;;; The base code

(defun match-p (x y &optional (lsts '(nil)))
  (cond ((var-p x) (update-bindings x y lsts))
        ((atom x) (and (eql x y) lsts))
        ((?-function-p (car x))
         (funcall (car x) (cdr x) y lsts))
        ((atom y) nil)
        (t (match-p (cdr x) (cdr y)
                    (match-p (car x) (car y) lsts)))))

(defun var-p (x)
  (and (symbolp x) 
       (eql (char (symbol-name x) 0) #\?)))

(defun update-bindings (x y lsts)
  (cond ((eql x '?) lsts)
        ((null lsts) nil)
        (t
         (append (bind-var x y (car lsts))
                 (update-bindings x y (cdr lsts))))))

(defun bind-var (x y lst)
  (let ((a (assoc x lst)))
    (cond ((null a)
           (list (cons (cons x y) lst)))
          ((equal (cdr a) y) (list lst))
          (t nil))))

;;; Extension code

(defun ?-function-p (x)
  (and (var-p x) (fboundp x)))

(defun ?? (x y lsts)
  (and (apply (car x) y (cdr x)) lsts))

(defun ?* (x y lsts)
  (append (match-p x y lsts)
          (and (consp y)
               (?* x (cdr y) lsts))))

(defun ?and (x y lsts)
  (cond ((null lsts) nil)
        ((null x) lsts)
        (t (?and (cdr x) y (match-p (car x) y lsts)))))

; (defun ?and (x y lsts)
;   (cond ((null lsts) nil)
;         ((null x) lsts)
;         (t (let ((match-res (match-p (car x) y lsts)))
;              (format t "~C match res: ~A ~C" #\linefeed match-res #\linefeed)
;              (format t "~C lsts: ~A ~C" #\linefeed lsts #\linefeed)
;              (?and (cdr x) y match-res)))))

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
               ((atom expr)
                (format t "(atom expr)~C" #\linefeed)
                (format t "(append total (list expr)): ~A ~C" (append total (list expr)) #\linefeed)
                (append total (list expr)))
               ((null (cdr expr))
                  (cond ((and (equal y expr) (atom (car expr)))  ; '(nil) or '(a)
                         (append total (list expr) (list (car expr))))
                        ((atom (car expr)) ; '(b)
                         (append total (list (car expr))))
                        (t                 ; '((a))
                         (append total (list expr) (subexp (car expr))))))
               (t (append total (list expr) (subexp (car expr)))))))
       ((null expr) (or total (list nil)))))
