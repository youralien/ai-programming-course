(defun shortest-path (start end net)
  (reverse (bfs (list (list start)) 
                 #'(lambda (state) (eql state end)) 
                 #'(lambda (path) (cdr (assoc (car path) net))))))

(defun bfs (paths pred gen)
  (if (empty-queue-p paths)
      nil
    (let* ((newpaths (new-paths (car paths) gen))
           (found-end
            (do ((tmppaths newpaths (cdr tmppaths)))
                ((or (null tmppaths)
                     (funcall pred (car (car tmppaths))))
                 (car tmppaths)))))
      (or found-end (bfs (append (cdr paths) newpaths) pred gen)))))

(defun new-paths (path gen)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (list (cons n path))))
    (funcall gen path)))
