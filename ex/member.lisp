(in-package #:ddr-tests)

; an element is a member of a list if the 
; car is equal to the element, or the element is a 
; member of the cdr of the list
(defparameter *member-kb*
  '(
    (member ?x (cons ?x nil))
    (member ?x (cons ?l1 (cons ?x (cons ?l2 nil))))
    (<- (member ?x (cons ?l1 (cons ?l2 nil)))
        (member ?x ?l2))
    ; (<- (member ?x (cons ?x nil) (cons ?x nil))
    ;     (member ?x ?x nil))
    ))