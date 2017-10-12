(defun shortest-path (start end net)
  (catch 'abort
    (bfs end (list (list start)) '() net)))

(defun bfs (end queue seen net)
  (if (empty-queue-p queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (cond ((eql node end) (reverse path))
              ; ((not (null (member node seen))) (throw 'abort nil))
              (t (let ((nextpaths (new-paths path node net))
                       (newqueue (cdr queue)))
                   (dolist (npath nextpaths path)                   
                     (if (null (some (lambda (n) (member n seen)) npath)) 
                         (throw 'abort nil)
                       (setq newqueue (append newqueue (list npath)))))
                   (bfs end newqueue (append seen (list node)) net))))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
    (cons n path))
  (cdr (assoc node net))))
