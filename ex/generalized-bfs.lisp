(defun shortest-path (start end net)
  (let ((paths (list (list start))))
    (reverse (bfs paths 
                  #'(lambda (state) (eql state end)) 
                  #'(lambda (path) (cdr (assoc (car path) net)))))))

(defun bfs (paths pred gen)
  (if (empty-queue-p paths)
      nil
    (let* ((path (car paths))
           (newpaths (remove-if-not
                      #'only-unique-elements
                      (mapcar #'(lambda (n) (cons n path)) (funcall gen path)))))
      (dolist (npath newpaths)
        (when (funcall pred (car npath))
          (return-from bfs npath)))
      (bfs (append (cdr paths)
                   newpaths)
           pred
           gen))))
    
(defun only-one (sym lst)
  (null (member sym (cdr (member sym lst)))))

(defun only-unique-elements (lst)
  (every (lambda (sym) (only-one sym lst)) lst))
