(defun shortest-path (start end net)
  (reverse (bfs (list (list start)) 
                 #'(lambda (state) (eql state end)) 
                 #'(lambda (path) (cdr (assoc (car path) net))))))

(defun bfs (paths pred gen)
  (if (empty-queue-p paths)
      nil
    (let* ((path (car paths))
           (newpaths (new-paths path gen))
           (found-end
            (do* ((tmppaths newpaths (cdr tmppaths))
                  (npath (car tmppaths) (car tmppaths)))
                 ((or (null tmppaths)
                      (funcall pred (car npath)))
                  npath))))
      (if found-end
          found-end
        (bfs (append (cdr paths) newpaths) pred gen)))))
    
(defun new-paths (path gen)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (list (cons n path))))
    (funcall gen path)))
