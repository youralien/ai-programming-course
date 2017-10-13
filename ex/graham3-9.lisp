(defun longest-path (start end net)
  (reverse (dfs (list (list start))
                nil
                (lambda (n) (eql n end))
                (lambda (path) (cdr (assoc (car path) net))))))

(defun dfs (stack bestpath pred gen)
  (let* ((currpath (car stack))
         (nextstates (funcall gen currpath)))
    (if (null stack)
        bestpath
      (if (funcall pred (car currpath))
          (when (> (length currpath) (length bestpath))
              (dfs (cdr stack) currpath pred gen))
        (dfs (append (remove-if-not #'no-internal-cyclic-loops
                                    (mapcar #'(lambda (n) (cons n currpath))
                                      nextstates))
                     (cdr stack))
             bestpath
             pred
             gen)))))


(defun no-internal-cyclic-loops (lst)
  (every (lambda (sym) (null (member sym (cdr (member sym (cdr lst)))))) lst))

(define-test no-internal-cyclic-loops
    (assert-true (no-internal-cyclic-loops '(A B C)))
    (assert-true (no-internal-cyclic-loops '(A B A)))
    (assert-false (no-internal-cyclic-loops '(A B A B)))
  )
