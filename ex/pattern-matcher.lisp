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

; ; previous submission
; (defun ?contains (x y lsts)
;   (if (atom y)
;       (match-p (car x) y)
;     (do ((expr y (if (atom expr) nil (cdr expr)))
;          (total (match-p (car x) y)
;             (cond
;               ((atom expr) total)
;               ((atom (car expr))
;                  (append (match-p (car x) (car expr)) total))
;               (t (append (?contains x (car expr) lsts) total)))))
;         ((null expr) total))))

; (defun ?contains (x y lsts)
;   (if (atom y)
;       (progn
;         (format t "~Cmatch: ~A" #\linefeed (car (match-p (car x) y)))
;         (car (match-p (car x) y)))
;     (maplist
;             #'(lambda (expr)
;                 (cond
;                   ((null (cdr expr))
;                    (format t "~Ccontains: ~A" #\linefeed (?contains x (car expr) lsts))
;                    ; (append (match-p (car x) expr))
;                    ;         (?contains x (car expr) lsts))
;                    ; (append (?contains x (car expr) lsts)
;                    ;         (match-p (car x) expr)))
;                    (?contains x (car expr) lsts))

;                   ((atom (car expr))
;                    ; (format t "~Cexpr: ~A" #\linefeed expr)
;                    (append (match-p (car x) (car expr))
;                            (match-p (car x) expr)))
;                   (t (append (match-p (car x) expr)
;                              (?contains x (car expr) lsts)))))

;                   ; (t (append (?contains x (car expr) lsts)
;                   ;            (match-p (car x) expr)))))
;             y)))

; new idea - ?contains (x y lsts) will reduce by calling
; (?contains (car x) (cdr y)
;            (?contains (car x) (car y) lsts)


; (defun ?contains (x y lsts)
;   (cond ((atom y)
;          ; (format t "~C (x y lsts) ~A ~A ~A" #\linefeed x y lsts)
;          ; (format t "~C match ~A " #\linefeed (match-p (car x) y lsts))
;          ; (format t "~C dont be equal: ~A" #\linefeed (equal '(nil) lsts))
;          (if (equal '(nil) lsts)
;               (match-p (car x) y)
;             (if (null y)
;                 lsts
;               (append (match-p (car x) y) lsts))))
;         ; ^^^ WE DONT want match-p to have the blsts, since this is the disjunctive?
;         ; so don't do (match-p (car x) y lsts)
        
;         ; ((null (cdr y))
;         ;  (cond ((atom (car y))
;         ;         (?contains x (car y) lsts))
;         ;        (t (append (match-p (car x) y lsts)
;         ;                   (?contains x (car y) lsts)))))
;         (t
;           ; (format t "~C match nil: ~A" #\linefeed (match-p (car x) y lsts))
;           (append (match-p (car x) y lsts)
;                   (?contains x (cdr y)
;                             (?contains x (car y) lsts))))))

(defun ?contains (x y lsts)
  (cond ((atom y)
         (cond ((equal '(nil) lsts) (match-p (car x) y))
               ((null y) lsts)
               (t (append (match-p (car x) y) lsts))))
        (t (append (match-p (car x) y lsts)
                   (?contains x (cdr y)
                              (?contains x (car y) lsts))))))

(defun car-subexp (y)
  (let ((accum nil))
    (mapc #'(lambda (x)
               (cond
                 ((atom x) (setq accum
                                 (cons x accum)))
                 (t (setq accum
                          (append accum
                                  (list x)
                                  (car-subexp x))))))
            y)
    accum))

; expects y to not be atom
(defun cdr-subexp (y)
  (let ((accum (if (null (cdr y)) (list y) nil)))
    (mapl #'(lambda (x)
              (cond
                ((null (cdr x)) nil)
                (t (push x accum))))
          y)
    accum))

(defun subexp (y)
  (if (atom y)
      (list y)
    (append (car-subexp y) (cdr-subexp y))))

; (defun show-subexp (y)
;   (if (atom y)
;       y
;     (maplist #'(lambda (x)
;                 (cond
;                   ((null (cdr x)) (car x))
;                   ((atom (car x)) (cons (car x) (list x)))
;                   (t (append (show-subexp (car x))
;                              x))))
;               y)))

; not working...
; (defun ?contains (x y lsts)
;   (if (atom y)
;       (match-p (car x) y)
;     (car (maplist #'(lambda (expr)
;                   (cond
;                     ((atom (car expr))
;                      (if (null (cdr expr))
;                           (append (match-p (car x) expr)
;                                   (match-p (car x) (car expr)))
;                         (match-p (car x) expr)))
;                     (t (append (match-p (car x) expr)
;                                (?contains x (car expr) lsts)))))
;              y))))
  
; passes 3 out of 3 but misses the '((a)) etc.
; (defun subexp (y)
;   (cond
;     ((atom y) (list y))
;     (t (let ((accum nil))
;           (mapl #'(lambda (x)
;                     (cond
;                       ((null (cdr x)) (push (car x) accum))
;                       ((atom (car x)) (push (car x) accum)
;                                       (push x accum))
;                       (t (setq accum
;                                (append (subexp (car x))
;                                        x
;                                        accum)))))
;                 y)
;           accum))))

; attempt at reduce... getting super complex
; (defun ?contains (x y lsts)
;   (cond
;     ((atom y) (match-p (car x) y))
;     (t (reduce #'(lambda (thecar thecdr)
;                     (cond ((null thecdr)
;                            (?contains (car x) thecar lsts))
;                           ((atom thecar)
;                            (append (match-p (car x) thecar)
;                                    (match-p (car x) (cons thecar thecdr))))
;                           (t (append (?contains x thecar lsts)
;                                      (match-p (car x) (cons thecar thecdr))))))
;                y 
;                :from-end t))))
; 2 test cases away from PURE car-cdr recursion
; (defun ?contains (x y lsts)
;   (let ((total (match-p (car x) y)))
;     (cond
;       ((atom y) (if (null y) nil total))
;       ; ((null (cdr y)) (append total (match-p x (car y))))
;       ((null (cddr y)) (append total
;                                (?contains x (car y) lsts)
;                                (?contains x (cadr y) lsts)))
;       (t (append total (?contains x (cdr y) lsts))))))

; (defun ?contains (x y lsts)
;   (let ((total (match-p (car x) y)))
;     (cond
;       ((atom y) total)
;       (t (append total (?contains-subexp x y lsts))))))

; (defun ?contains-subexp (x y lsts)
;   (cond
;     ((atom y) (match-p (car x) y))
;     ((null (cdr y))
;        (append (match-p (car x) y)
;                (match-p (car x) (car y) lsts)))
;     (t (append (match-p (car x) y)
;                (?contains-subexp x (car y) lsts)
;                (?contains-subexp x (cdr y) lsts)))))

    ; (do ((expr y (if (atom expr) nil (cdr expr)))
    ;      (total nil
    ;         (cond
    ;           ((atom expr))
    ;           ((atom (car expr))
    ;              (append (match-p (car x) (car expr)) total))
    ;           (t (append (?contains x (car expr) lsts) total)))))
    ;     ((null expr) total))))    

;;; contains, long form
; (defun ?contains (x y lsts)
;   (do ((expr y (if (atom expr) nil (cdr expr)))
;        (total nil
;         (cond
;           ((atom expr)
;            (append (match-p (car x) expr) total))
;           ((atom (car expr))
;            (if (eql y expr)
;                (append (match-p (car x) (car expr)) (match-p (car x) expr) total)
;              (append (match-p (car x) (car expr)) total)))
;           (t (append (match-p (car x) expr) total
;                      (?contains x (car expr) lsts))))))
;       ((null expr) (or total (match-p (car x) y)))))

;;; contains, with subexp
; (defun ?contains (x y lsts)
;   (mapcan #'(lambda (expr)
;              (match-p (car x) expr))
;           (subexp y)))

; (defun subexp (y)
;   (do ((expr y (if (atom expr) nil (cdr expr)))
;        (total nil
;             (cond
;               ((atom expr) (cons expr total))
;               ((atom (car expr))
;                    (if (eql y expr)
;                        (list* (car expr) expr total)
;                      (cons (car expr) total)))
;               (t (append (cons expr total) (subexp (car expr)))))))
;       ((null expr) (or total (list nil)))))
