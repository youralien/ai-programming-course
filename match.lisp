#|
Copyright (C) 2014 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; A Simple Extensible Lisp Pattern Matcher
;;; Author: Chris Riesbeck
;;; 
;;; Update history:
;;;
;;; 2016-11-17 added some basic tests to RENAME-VARS [CKR]
;;; 2014-11-09 added tests for RENAME-VARS, exported VAR-P from EXMATCH [CKR]
;;; 2014-10-26 added clarifying tests to ?CONTAINS [CKR]
;;; 2014-10-22 renamed package because Lispworks has reserved the package name MATCH [CKR]
;;; 2014-10-17 created file [CKR]

;;; For exercises in extending this code, see
;;;   http://www.cs.northwestern.edu/academics/courses/325/exercises/match-exs.php

(defpackage #:exmatch
  (:use #:common-lisp)
  (:export #:match-p #:var-p #:? #:?* #:?? #:?= #:?and #:?or #:?not #:?contains))

(defpackage #:match-tests
  (:use #:common-lisp #:lisp-unit #:exmatch))


(in-package :match-tests)

(define-test match-? 
  (assert-true (match-p '? 'a))
  (assert-true (match-p 'a 'a))
  (assert-false (match-p 'b 'a))
  (assert-true (match-p '() '()))
  (assert-true (match-p '(?) '(a)))
  (assert-false (match-p '(?) '(a b)))
  (assert-true (match-p '? '(a b)))
  (assert-true (match-p '(? b ?) '(a b c)))
  (assert-false (match-p '((?)) '((a b))))
  (assert-true (match-p '((? ?)) '((a b))))
  (assert-false (match-p '(? ?) '((a b))))
  (assert-false (match-p '(?) '()))
  (assert-true (match-p '? '()))
  (assert-false (match-p '(?) 'a))
  )

(define-test match-var  
  (assert-equal '(((?x . a))) (match-p '?x 'a))
  (assert-equal '(((?x . nil))) (match-p '?x nil))
  (assert-false (match-p '(?x ?x) '(a b)))
  (assert-equality set-equal '(((?y . a) (?x . a))) (match-p '(?x ?y) '(a a)))
  (assert-equality set-equal '(((?y . b) (?x . a))) (match-p '(?x ?y) '(a b)))
  )

(define-test match-*
  (assert-equal '(((?x . a))) (match-p '(?* ?x) '(a)))
  (assert-equal '(((?x . d))) (match-p '(?* ?x) '(a b c d)))
  (assert-equal '(nil) (match-p '(?*) '(a b c d)))
  (assert-equal '(nil) (match-p '(?*) '()))
  (assert-false (match-p '(?* ?x) '()))
  (assert-equality set-equal '(((?x . a)) ((?x . b)) ((?x . c))) (match-p '(?* ?x ?*) '(a b c)))
  )

(define-test match-?
  (assert-equal '(nil)  (match-p '(?? numberp) '12))
  (assert-false  (match-p '(?? numberp) 'a))
  (assert-equal '(nil)  (match-p '(?? stringp) "hello"))
  (assert-equal '(nil)  (match-p '(?? > 5) 12))
  (assert-false  (match-p '(?? > 5) 3))
  )

(define-test match-and
  (assert-equal '(nil) (match-p '(?and) nil))
  (assert-equal '(((?x . 12)))  (match-p '(?and (?? numberp) ?x) 12))
  (assert-false  (match-p '(?and (?? > 15) ?x) 12))
  
  (assert-equality set-equal '(((?x . 24)) ((?x . 30)))  (match-p '(?* (?and (?? > 20) ?x) ?*) '(24 11 3 30)))
  )

(define-test match-not
  (assert-false (match-p '(?not (a b c)) '(a b c)))
  (assert-equal '(nil) (match-p '(?not (a b c)) 12))
  (assert-false (match-p '(?not ?x) 'a))
  (assert-equal '(((?x . a))) (match-p '(?x (?not ?x)) '(a b)))
  (assert-equal '(nil) (match-p '(?not (?? > 5)) 1))
  (assert-false (match-p '(?not (?? > 5)) 8))
  
  (assert-equal '(((?x . 1))) (match-p '(?and (?not > 5) ?x) 1))
  (assert-false (match-p '(?and (?not (?? > 5)) ?x) 8))
  )

(define-test match-or
  (assert-equal '(nil) (match-p '(?or a b) 'a))
  (assert-equal '(nil) (match-p '(?or a b) 'b))
  (assert-false (match-p '(?or a b) 'c))
  
  (assert-equality set-equal '(((?x . a)) ((?y . a))) (match-p '(?or ?x ?y) 'a))
  (assert-equality set-equal '(((?y . b) (?x . a)) ((?z . a) (?y . b) (?x . a))) 
                (match-p '(?x ?y (?or ?x ?y ?z)) '(a b a)))
  
  (assert-equal '(nil) (match-p '(?or (?? > 5) (?? < 0)) 10))
  (assert-equal '(nil) (match-p '(?or (?? > 5) (?? < 0)) -3))
  (assert-false (match-p '(?or (?? > 5) (?? < 0)) 3))
  
  (assert-equal '(((?x . 1))) (match-p '(?and (?or (?? < -10) (?? > 0)) ?x) 1))
  )

(define-test match-=
  (assert-equal '(nil) (match-p '(?= 9 square) 3))
  (assert-false (match-p '(?= 9 square) 2))
  (assert-equal '(nil) (match-p '(?= (?not nil) contains "foo") "some food"))
  (assert-equal '(((?x . 9))) (match-p '(?= ?x square) 3))
  (assert-equal '(((?x . 9))) (match-p '((?= ?x square) ?x) '(3 9)))
  (assert-false (match-p '((?= ?x square) ?x) '(3 6)))
  )

(defun square (x) (* x x))
(defun contains (x y) (and (stringp x) (search y x)))


(define-test match-contains
  (assert-equal '(((?x . a))) (match-p '(?contains ?x) 'a))
  (assert-equality set-equal '(((?x . ((a)))) ((?x . (a))) ((?X . a)))
                   (match-p '(?contains ?x) '((a))))
  (assert-equal '(((?x . nil))) (match-p '(?contains ?x) nil))
  (assert-equality set-equal '(((?x . (nil))) ((?x . nil)))
                   (match-p '(?contains ?x) '(nil)))
  (assert-equality set-equal '(((?x . (a b))) ((?x . b)) ((?x . a))) (match-p '(?contains ?x) '(a b)))
  (assert-equality set-equal '(((?x . 5)) ((?x . 12))) (match-p '(?contains (?and (?? numberp) ?x)) '((a 12) c (((5))))))
  )        

(define-test subexp
  (assert-equal '(a) (subexp 'a))
  (assert-equal '((a) a) (subexp '(a))
  (assert-equal '(((a)) (a) a) (subexp '((a))))
  (assert-equal '((a b) a b) (subexp '(a b)))
  (assert-equal '(nil) (subexp nil))
  (assert-equal '((nil) nil) (subexp '(nil)))
)

(define-test rename-vars
  (assert-true (valid-renaming-p '?x))
  (assert-true (valid-renaming-p '(?x ?x)))
  (assert-true (valid-renaming-p '(?x ?y)))
  (assert-true (valid-renaming-p '(foo ?x ?y (baz ?x ?y ?z))))
  (assert-equal nil (needless-cons '(foo b c)))
  (assert-equal nil (needless-cons '(foo ?x ?y (baz ?x ?y ?z))))
  (assert-equal nil (needless-cons '(foo (a (b c)) (d (e ?x)) (f g))))
  )


(defun valid-renaming-p (old &optional (new (rename-vars old)) (pairs '(nil)))
  (cond ((var-p old) (valid-pair-p old new pairs))
        ((atom old) (and (eql old new) pairs))
        (t 
         (valid-renaming-p (cdr old) (cdr new)
                           (valid-renaming-p (car old) (car new) pairs)))))

(defun valid-pair-p (old new pairs)
  (and (not (eql old new))
       (let ((pair (assoc old pairs)))
         (cond ((null pair) 
                (cons (cons old new) pairs))
               ((eql new (cdr pair)) pairs)
               (t nil)))))

;;; NOTE: do not use CONTAINS-VAR-P in RENAME-VARS. It would
;;; make your recursion very inefficient, because it would
;;; rescan subtrees many times.
(defun needless-cons (old &optional (new (rename-vars old)))
  (and (consp old)
       (or (and (not (contains-var-p old))
                (not (contains-var-p new))
                (not (eq old new))
                old)
           (needless-cons (car old) (car new))
           (needless-cons (cdr old) (cdr new)))))

(defun contains-var-p (x)
  (or (var-p x)
      (and (consp x)
           (or (contains-var-p (car x))
               (contains-var-p (cdr x))))))
  

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