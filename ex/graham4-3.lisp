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
  ; (let ((clone (make-3tree :data tree-data
  ;                          :left tree-left
  ;                          :middle tree-middle
  ;                          :right tree-right)))
  (if (null tree)
      nil
    (let ((clone (copy-3tree tree)))
      (make-3tree :data (3tree-data clone)
                  :left (3tree-clone (3tree-left clone))
                  :middle (3tree-clone (3tree-middle clone))
                  :right (3tree-clone (3tree-right clone))))))
                  ; :left (or (3tree-left clone) (3tree-clone (3tree-left clone)))
                  ; :middle (or (3tree-middle clone) (3tree-clone (3tree-middle clone)))
                  ; :right (or (3tree-right clone) (3tree-clone (3tree-right clone)))))))