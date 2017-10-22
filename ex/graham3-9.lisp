(defun longest-path (start end net)
  (reverse (dfs (list (list start))
                nil
                (lambda (n) (eql n end))
                (lambda (path) (cdr (assoc (car path) net))))))

; (defun dfs (stack bestpath pred gen)
;   (let* ((currpath (car stack))
;          (nextstates (funcall gen currpath))
;          (nextpaths (append (remove-if-not #'no-internal-cyclic-loops
;                                            (mapcar #'(lambda (n) (cons n currpath))
;                                                    nextstates))
;                             (cdr stack))))
;     (cond
;       ((null stack) bestpath)
;       ((and (funcall pred (car currpath))
;            (> (length currpath) (length bestpath)))
;        (dfs nextpaths currpath pred gen))
;       (t (dfs nextpaths bestpath pred gen)))))
      
(defun path-thru-node-to-end (path pred gen)
  (format t "path: ~A~C" path #\linefeed)
  (do* ((n-to-explore (funcall gen path) (cdr n-to-explore))
       (n (car n-to-explore) (car n-to-explore)))
      ((null n-to-explore))
    (if (funcall pred n)
        (return (cons n path))
      (unless (member n path)
        (return (path-thru-node-to-end (cons n path) pred gen))))))