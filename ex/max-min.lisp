(defun max-min (vec &key (start 0) (end (length vec)))
  (cond
    ((>= start end) (values nil nil))
    (t (let ((elem (svref vec start)))
         (find-max-min vec start end)))))

(defun find-max-min (vec start end &optional maximum minimum)
  (if (eql start end)
    	(values maximum minimum)
	  (let ((elem (svref vec start)))
	    (find-max-min vec
	                  (1+ start)
	                  end
	               	  (if (null maximum) elem (max maximum elem))
	                  (if (null minimum) elem (min minimum elem))))))

; (defun find-max-min (vec start end)
;   (if (eql start end)
;       nil
; 	  (let ((elem (svref vec start)))
; 	    (multiple-value-bind (maximum minimum) (find-max-min vec (1+ start) end)
; 				(values (max elem (or maximum elem))
; 	           		(min elem (or minimum elem)))))))
