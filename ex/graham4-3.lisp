; Structure name: 3tree
; Function names: 3tree-clone, 3tree-member

(defstruct 3tree
  data
  left
  middle
  right)


; make-point, point-p, copy-point, point-x, and point-y.
; use setf for the values, i.e. (setf (point-x point-instance) val)

(defun 3tree-clone (tree)
  (if (null tree)
      nil
    (make-3tree :data (3tree-data tree)
                :left (3tree-clone (3tree-left tree))
                :middle (3tree-clone (3tree-middle tree))
                :right (3tree-clone (3tree-right tree)))))