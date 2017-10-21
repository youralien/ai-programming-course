(defun shortest-path (start end net)
  (catch 'abort
         (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue))
           (node (car path))
           (newpaths (new-paths path node net)))
      (dolist (npath newpaths)
        (when (eql end (car npath))
          (throw 'abort (reverse npath))))
      (bfs end
           (append (cdr queue)
                   newpaths)
           net))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue))
           (node (car path))
           (newpaths (new-paths path node net)))
      (dolist (npath newpaths)
        (when (eql end (car npath))
          (return-from bfs (reverse npath))))
      (bfs end
           (append (cdr queue)
                   newpaths)
           net))))

(defun new-paths (path node net)
  (mapcan #'(lambda (n)
              (unless (member n path)
                (list (cons n path))))
    (cdr (assoc node net))))
