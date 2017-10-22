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
  (when (null (match-p (car x) y lsts))
    lsts))

; (defun ?or (x y lsts)
;   (format t "~C x: ~A y: ~A ~C" #\linefeed x y #\linefeed)
;   (let ((match-res (match-p (car x) y (list nil))))
;     (format t "~C match res: ~A ~C" #\linefeed match-res #\linefeed)
;     (format t "~C lsts: ~A ~C" #\linefeed lsts #\linefeed)
;     (cond
;       ((null x) (remove nil lsts :count 1))
;       (t (?or (cdr x) y (append match-res lsts))))))

; (defun ?or (x y lsts)
;   (format t "~C x: ~A y: ~A ~C" #\linefeed x y #\linefeed)
;   (let ((match-res (match-p (car x) y lsts)))
;     (format t "~C match res: ~A ~C" #\linefeed match-res #\linefeed)
;     (format t "~C lsts: ~A ~C" #\linefeed lsts #\linefeed)
;     (cond
;       ((null x) (remove nil lsts :count 1))
;       (t (?or (cdr x) y (append match-res lsts))))))

(defun ?or (x y lsts)
  (format t "~Cpatterns: ~A~C" #\linefeed x #\linefeed)
  (format t "lsts: ~A~C" lsts #\linefeed)
  (format t "input: ~A~C" y #\linefeed)
  (mapcan #'(lambda (ptrn)
             (match-p ptrn y lsts))
          x))
  

; (defun ?or (x y lsts)
;   (format t "~C x: ~A y: ~A ~C" #\linefeed x y #\linefeed)
;   (when (and (> (length lsts) 1)
;              (member nil lsts))
;     (setf lsts (remove nil lsts :count 1)))
;   (let ((match-res (match-p (car x) y lsts)))
;     (format t "~C match res: ~A ~C" #\linefeed match-res #\linefeed)
;     (format t "~C lsts: ~A ~C" #\linefeed lsts #\linefeed)
;     (cond
;       ((null x) lsts)
;       (t (?or (cdr x) y (append match-res lsts))))))


(defun ?= (x y lsts)
  (let* ((sub-pattern (car x))
         (function-name (cadr x))
         (arguments (cddr x))
         (tmp1 (apply function-name (cons y arguments))))
    ; (format t "sub-pattern: ~A~C" sub-pattern #\linefeed)
    ; (format t "function-name: ~A~C" function-name #\linefeed)
    ; (format t "arguments: ~A~C" arguments #\linefeed)
    ; (format t "form: ~A~C" y #\linefeed)
    ; (format t "result-out: ~A~C" tmp1 #\linefeed)
    (match-p sub-pattern tmp1 lsts)))
  
