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
    (cond
      ((null stack) bestpath)
      ((and (funcall pred (car currpath))
           (> (length currpath) (length bestpath)))
       (dfs nextpaths currpath pred gen))
      (t (dfs nextpaths bestpath pred gen)))))


(defun only-one-internally (sym lst)
  (let* ((first-tail (member sym lst))
         (second-tail (member sym (cdr first-tail))))
    (if (eql (length first-tail) (length lst))
        (null (cdr second-tail))
        (null second-tail))))

(defun no-internal-cyclic-loops (lst)
  (every (lambda (sym)
           (only-one-internally sym lst))
         lst))

(define-test no-internal-cyclic-loops
    (assert-true (no-internal-cyclic-loops '(A B C)))
    (assert-true (no-internal-cyclic-loops '(A B A)))
    (assert-false (no-internal-cyclic-loops '(A B A B)))
    (assert-false (no-internal-cyclic-loops '(A B C B)))
    (assert-false (no-internal-cyclic-loops '(B C B A)))
  )
