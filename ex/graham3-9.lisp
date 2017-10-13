(defun longest-path (start end net)
  (reverse (dfs (list (list start))
                nil
                (lambda (n) (eql n end))
                (lambda (path) (cdr (assoc (car path) net))))))

(defun dfs (stack bestpath pred gen)
  (let* ((currpath (car stack))
         (nextstates (funcall gen currpath))
         (nextpaths (append (remove-if-not #'no-internal-cyclic-loops
                                           (mapcar #'(lambda (n) (cons n currpath))
                                                   nextstates))
                            (cdr stack))))
    ; (format t "stack ~A ~C" stack #\linefeed)
    (if (null stack)
        bestpath
      (if (funcall pred (car currpath))
          (when (> (length currpath) (length bestpath))
              (dfs (cdr stack) currpath pred gen))
        (dfs nextpaths bestpath pred gen)))))


(defun no-internal-cyclic-loops (lst)
  (every (lambda (sym) (null (member sym (cdr (member sym (cdr lst)))))) lst))

(define-test no-internal-cyclic-loops
    (assert-true (no-internal-cyclic-loops '(A B C)))
    (assert-true (no-internal-cyclic-loops '(A B A)))
    (assert-false (no-internal-cyclic-loops '(A B A B)))
    (assert-false (no-internal-cyclic-loops '(A B C B)'))
  )


#|
LONGEST-PATH: (LONGEST-PATH 'A 'A '((A B) (B A C))) failed: 
Expected (A B A) but saw (A)
LONGEST-PATH: (LONGEST-PATH 'A 'F
                            '((A B C A) (B C D) (C E A) (D E F)
                              (E D F))) failed: 
Expected (A B C E D F) but saw NIL
LONGEST-PATH: (LONGEST-PATH 'A 'A
                            '((A B C A) (B C D) (C E A) (D E F)
                              (E D F))) failed: 
Expected (A B C A) but saw (A)
LONGEST-PATH: (LONGEST-PATH 'A 'A '((A A B) (B C))) failed: 
Expected (A A) but saw (A)
LONGEST-PATH: (LONGEST-PATH 'A 'A '((A B A) (B C))) failed: 
Expected (A A) but saw (A)
LONGEST-PATH: 8 assertions passed, 5 failed.

-------

LONGEST-PATH: (LONGEST-PATH 'A 'F
                            '((A B C A) (B C D) (C E A) (D E F)
                              (E D F))) failed: 
Expected (A B C E D F) but saw NIL
LONGEST-PATH: (LONGEST-PATH 'A 'A
                            '((A B C A) (B C D) (C E A) (D E F)
                              (E D F))) failed: 
Expected (A B C A) but saw NIL
LONGEST-PATH: (LONGEST-PATH 'A 'B '((A B) (B C) (C B))) failed: 
Expected (A B) but saw (A B C B)
LONGEST-PATH: (LONGEST-PATH 'A 'B '((A B C) (B C) (C B))) failed: 
Expected (A C B) but saw NIL
LONGEST-PATH: 9 assertions passed, 4 failed.

#|

