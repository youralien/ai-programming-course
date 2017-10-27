(defun map-stream (fn in)
  (do ((expr (read in nil -) (read in nil -)))
      ((eql expr -))
    (funcall fn expr)))