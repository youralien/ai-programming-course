(load "/Users/ryan/quicklisp/local-projects/cs325/bst.lisp")

(defun bst-elements (bst)
  (cond ((null bst) nil)
        (t (append (bst-elements (node-r bst))
                   (list (node-elt bst))
                   (bst-elements (node-l bst))))))

(defun bst-elements (bst)
  (let ((out nil))
    (bst-traverse #'(lambda (val) (push val out))
                  bst)
    out))