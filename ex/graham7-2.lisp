(defun output-expr (string)
  (with-input-from-string (in string)
	(format t "~A~C" (read in nil :eof) #\linefeed)))

(defun map-stream (fn in)
  (do ((expr (read in nil :#<>) (read in nil :#<>)))
      ((eql expr :#<>))
  	(funcall fn expr)))