(defun shortest-path (start end net)
  (catch 'abort
    (bfs end (list (list start)) net)))

;;;(defun bfs (end queue seen net)
;;;  (if (empty-queue-p queue)
;;;    nil
;;;    (let ((path (car queue)))
;;;      (let ((node (car path)))
;;;        (cond ((eql node end) (reverse path))
;;;              ; ((not (null (member node seen))) (throw 'abort nil))
;;;              (t (let ((nextpaths (new-paths path node net))
;;;                       (newqueue (cdr queue)))
;;;                   (dolist (npath nextpaths path)                   
;;;                     (if (null (every (lambda (n) (member n seen)) npath)) 
;;;                         ; (throw 'abort nil)
;;;                         (setq newqueue (append newqueue (list npath)))
;;;                       (setq newqueue (append newqueue (list npath)))))
;;;                   (format t "~A" newqueue)
;;;                   (bfs end newqueue (append seen (list node)) net))))))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
            (reverse path)
          (bfs end
               (append (cdr queue)
                       (remove-if-not #'only-unique-elements (new-paths path node net)))
               net))))))

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
