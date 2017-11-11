(in-package #:ddr-tests)

; an element is a member of a list if the 
; car is equal to the element, or the element is a 
; member of the cdr of the list
(defparameter *member-kb*
  '(
    ; -- examples of direct matching working --
    ; (member ?x (cons ?x nil))
    ; (member ?x (cons ?l1 (cons ?x (cons ?l2 nil))))
    
    ; -- examples of infinite loop --
    ; (<- (member ?x (cons ?x ?l2))
    ;     (member ?x (cons ?y nil)))
    
    ; -- best working example to date! --
    ; this returns NIL but expected T
    ; (ASK (MEMBER B (CONS A (CONS B (CONS C NIL)))))
    ; (member ?x (cons ?x nil))
    ; (<- (member ?x (cons ?l1 ?l2))
    ;     (member ?x ?l2))
    
    ; -- passes all the tests! --
    (member ?x (cons ?x ?l1))
    (<- (member ?x (cons ?l1 ?l2))
        (member ?x ?l2))
    
    ; (<- (member ?x (cons ?x nil) (cons ?x nil))
    ;     (member ?x ?x nil))
    ))


; test cases
#|
  (assert-false (ask '(member a nil)))
  (assert-true (ask '(member a (cons a nil))))
  (assert-false (ask '(member nil (cons a nil))))
  (assert-true (ask '(member b (cons a (cons b (cons c nil))))))
  (assert-true (ask '(member c (cons a (cons b (cons c nil))))))
  (assert-false (ask '(member d (cons a (cons b (cons c nil))))))
  (assert-false (ask '(member nil nil)))
  (assert-false (ask '(member a a)))
  (assert-false (ask '(member a (cons (cons a nil) (cons b nil)))))
|#