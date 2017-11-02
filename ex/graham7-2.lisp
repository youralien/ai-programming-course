(defun map-file (fn name)
  (with-open-file (str name :direction :input)
    (map-stream fn str)))

(defun map-stream (fn in)
  (let ((obj (lambda (x) x)))
    (do ((expr (read in nil obj) (read in nil obj)))
      ((eql expr obj))
      (funcall fn expr))))