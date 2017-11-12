; some doesn't return the index, so maybe not useful

; (defun make-change (val &optional (coins '(25 10 5 1)))
;   (let ((remain (some #'(lambda (coin)
;                          (if (> val coin)
;                              (- val coin)
;                             nil))
;                       coins)))
;     (print remain


; i don't like this I am brain dead
(defun make-change (val &optional (coins '(25 10 5 1)))
  (if (zerop val)
      (values 0 0 0 0)
    (do* ((i 0)
          (coin-length (length coins))
          (change val)
          (remain (- change (nth i coins))
                  (- change (nth i coins)))
          (coin-counts (make-list coin-length :initial-element 0)))
         ((= remain 0) 
          (incf (nth i coin-counts))
          (values-list coin-counts))
     ; (format t "~C----~C" #\linefeed #\linefeed)
     ; (print remain)
     ; (print coin-counts)
     ; (print change)
     ; (print i)
      (cond
        ((not (minusp remain))
         (setf change remain)
         (incf (nth i coin-counts)))
        (t
         (unless (= i (1- coin-length))
           (incf i)))))))