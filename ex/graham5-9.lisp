(defun shortest-path (start end net)
  (catch 'abort
         (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue))
           (node (car path)))
      (bfs end
           (append (cdr queue)
                   (new-paths path node end net))
           net))))

(defun new-paths (path node end net)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (let ((npath (cons n path)))
                  (if (eql n end)
                      (throw 'abort (reverse npath))
                    (list npath)))))
    (cdr (assoc node net))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue))
           (node (car path))
           (next-nodes (cdr (assoc (car path) net)))
           (found-end (member end next-nodes)))
      (if found-end
          (reverse (cons (car found-end) path))
        (bfs end
             (append (cdr queue)
                     (new-paths path node net))
             net)))))

(defun new-paths (path node net)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (list (cons n path))))
    (cdr (assoc node net))))
