(defparameter *triples*
  '((chilly isa penguin) (penguin isa bird)))

(define-test isa-p
  (assert-true (isa-p 'chilly 'penguin))
  (assert-true (isa-p 'penguin 'bird))
  (assert-true (isa-p 'chilly 'bird))
  (assert-false (isa-p 'bird 'penguin))
  )

; A -> child(A) -> B.
; If A is a direct parent of B, and A isa B, true
; If A is a distant parent of B, check if child(A) is direct parent of B



(defun isa-p (x y)
  (dolist (elm, *triples)
    (if ((eql x (car elm))
        (when (eql y (third (elm)))
          t)
        
        ((eql y (car elm)))

(defun isa-p (x y)

samuelhill2022@u.northwestern.edu