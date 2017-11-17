(in-package #:ddr-tests)

(defparameter *all-different-kb*
  '(
    (-> (all-different (cons ?a (cons ?b ?lst)))
        (different ?a ?b)
        (all-different (cons ?a ?lst))
        (all-different (cons ?b ?lst)))
    (-> (different ?x ?y) (different ?y ?x))
    ))


; test cases
#|
(assert-false (ask '(different a b)))
(tell '(all-different nil))
(assert-false (ask '(different a b)))
(tell '(all-different (cons a (cons b (cons c nil)))))
(assert-true (ask '(different a b)))
(assert-true (ask '(different a c)))
(assert-true (ask '(different b a)))
(assert-true (ask '(different b c)))
(assert-true (ask '(different c a)))
(assert-true (ask '(different c b)))
(assert-false (ask '(different a a)))
(assert-false (ask '(different a d))
|#