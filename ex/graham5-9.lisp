; (defun shortest-path (start end net)
;   (catch 'abort
;          (bfs end (list (list start)) net)))

; (defun bfs (end queue net)
;   (if (empty-queue-p queue)
;       nil
;     (let* ((path (car queue))
;            (node (car path)))
;       (bfs end
;            (append (cdr queue)
;                    (new-paths path node end net))
;            net))))

; (defun new-paths (path node end net)
;   (mapcan #'(lambda (n)
;               (if (member n path)
;                   nil
;                 (let ((npath (cons n path)))
;                   (if (eql n end)
;                       (throw 'abort (reverse npath))
;                     (list npath)))))
;     (cdr (assoc node net))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue))
           (node (car path))
           (new-paths-out (new-paths path node end net)))
      (if (atom (car new-paths-out))
          (reverse new-paths-out)
        (bfs end
             (append (cdr queue) new-paths-out)
             net)))))

(defun new-paths (path node end net)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (let ((npath (cons n path)))
                  (if (eql n end)
                      (return-from new-paths npath)
                    (list npath)))))
    (cdr (assoc node net))))
