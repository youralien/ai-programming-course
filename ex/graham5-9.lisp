(defun shortest-path (start end net)
  (catch 'abort
         (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue))
           (node (car path))
           (newpaths (remove-if-not #'only-unique-elements (new-paths path node net))))
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
           (newpaths (remove-if-not #'only-unique-elements (new-paths path node net))))
      (dolist (npath newpaths)
        (when (eql end (car npath))
          (return-from bfs (reverse npath))))
      (bfs end
           (append (cdr queue)
                   newpaths)
           net))))

(defun only-one (sym lst)
  (null (member sym (cdr (member sym lst)))))

(defun only-unique-elements (lst)
  (every (lambda (sym) (only-one sym lst)) lst))

(define-test only-unique-elements
    (assert-true (only-unique-elements '(A B C)))
    (assert-false (only-unique-elements '(A B A)))
    (assert-false (only-unique-elements '(A B A B)))
  )

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
    (cdr (assoc node net))))
