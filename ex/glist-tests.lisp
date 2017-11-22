(defpackage #:glist-tests
  (:use #:common-lisp #:lisp-unit #:glist)
  )

(in-package :glist-tests)

;;; Examples of how to use the GLIST generated lists
;;; package.
;;;
;;; Updates:
;;; 03/12/08 Fixed missing DELAY in GFLATTEN [CKR]
;;; 03/11/08 Replaced GEXTRACT with GEXTRACT [CKR]
;;; 01/18/05 Updated tests for new lisp-unit [CKR]
;;; 01/15/05 Added GEXTRACT utility [CKR]
;;; 01/15/05 Replaced printing code with unit tests [CKR]
;;; 05/03/04 ReplaCed GPOP with GLIST:GPOP in SILLY-EXAMPLE [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test basic generators made from a list.

(define-test glists
  (assert-equal '(a b c) (gextract (delay '(a b c))))
  (assert-equal nil (gextract (delay nil)))
  (assert-equal '(a b c) 
                (gextract (delay '(a b c d e f)) 3))
  (assert-equal nil
                (gextract (delay '(a b c d e f))  0))
  )
  

;;; Test a list generator that "flattens" a list.

(define-test gflatten
  (assert-equal '(a b c d e f) 
                (gextract (gflatten '(((a b) c d) e f))))
  (assert-equal nil (gextract (gflatten nil)))
  )

;;; Test a list generator that generates an infinite list of integers.

(define-test gintegers
  (assert-equal '(0 1 2 3 4) (gextract (gintegers) 5))
  (assert-equal '(3 4 5 6 7) (gextract (gintegers 3) 5))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (GFLATTEN x) => list generator
;;;   Returns a list generator that returns the non-nil atoms
;;;   from a nested list one at a time. 

(defun gflatten (x)
  (cond ((null x) nil)
        ((atom x) (list x))
        (t (gappend (gflatten (car x))
                    (delay (gflatten (cdr x)))))))

;;; (GINTEGERS n) => generated list
;;;   returns a generated list for all integers,
;;;   starting at n.

(defun gintegers (&optional (n 0))
  (gcons n (gintegers (1+ n))))