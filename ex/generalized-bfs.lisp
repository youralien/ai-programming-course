(defun shortest-path (start end net)
  (let ((paths (list (list start))))
    (reverse (bfs paths 
                  #'(lambda (state) (eql state end)) 
                  #'(lambda (path) (cdr (assoc (car path) net)))))))

(defun bfs (paths pred gen)
  (if (empty-queue-p paths)
      nil
    (let* ((path (car paths))
           (newpaths (new-paths path gen)))
      (dolist (npath newpaths)
        (when (funcall pred (car npath))
          (return-from bfs npath)))
      (bfs (append (cdr paths)
                   newpaths)
           pred
           gen))))
    
(defun new-paths (path gen)
  (mapcan #'(lambda (n)
              (unless (member n path)
                (list (cons n path))))
    (funcall gen path)))
