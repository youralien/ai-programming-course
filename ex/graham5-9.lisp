(defun shortest-path (start end net)
  (catch 'abort
    (bfs end (list (list start)) '() net)))

(defun bfs (end queue seen net)
  (if (empty-queue-p queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (cond ((eql node end) (throw 'abort (reverse path)))
              ((not (null (member node seen))) (throw 'abort nil))
              (t (bfs end 
                    (append (cdr queue)
                      (new-paths path node net)) (append seen (list node)) net)))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
    (cons n path))
  (cdr (assoc node net))))
