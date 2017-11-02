(defun max-min (vec &key (start 0) (end (length vec)))
  (cond
    ((>= start end) (values nil nil))
    (t (find-max-min vec start end))))

(defun find-max-min (vec start end &key maximum minimum)
  (if (= start end)
      (values maximum minimum)
    (let ((elem (svref vec start)))
      (find-max-min vec
                    (1+ start)
                    end
                    :maximum (max (or maximum elem) elem)
                    :minimum (min (or minimum elem) elem)))))
