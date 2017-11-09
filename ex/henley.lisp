(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

; (defun read-text (pathname)
;   (with-open-file (s pathname :direction :input)
;     (let ((buffer (make-string maxword))
;           (pos 0))
;       (do ((c (read-char s nil :eof) 
;               (read-char s nil :eof)))
;           ((eql c :eof))
;         (if (or (alpha-char-p c) (char= c #\'))
;             (progn
;               (setf (aref buffer pos) c)
;               (incf pos))
;             (progn
;               (unless (zerop pos)
;                 (funcall see (intern (string-downcase 
;                                (subseq buffer 0 pos))))
;                 (setf pos 0))
;               (let ((p (punc c)))
;                 (if p (funcall see p))))))))
;   (print *words*))

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (read-stream s see))
  (print *words*))
  
(defun read-stream (stream function)
  (let ((buffer (make-string maxword))
        (pos 0)
        (eof-obj (lambda (x) x)))
    (do ((c (read-char stream nil eof-obj) 
            (read-char stream nil eof-obj)))
        ((eql c eof-obj))
      (if (or (alpha-char-p c) (char= c #\'))
          (progn
            (setf (aref buffer pos) c)
            (incf pos))
          (progn
            (unless (zerop pos)
              (funcall function (intern (string-downcase 
                             (subseq buffer 0 pos))))
              (setf pos 0))
            (let ((p (punc c)))
              (if p (funcall function p))))))))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) 
    (#\! '|!|) (#\? '|?|) ))

(defvar see nil)
(defun make-see ()
  (let ((prev `|.|))
    (setq see (lambda (symb)
      (let ((pair (assoc symb (gethash prev *words*))))
        (if (null pair)
            (push (cons symb 1) (gethash prev *words*))
            (incf (cdr pair))))
      (setf prev symb)))))
(make-see)

(defun generate-text (n)
  (if (zerop n)
      (terpri)
    (do ((next '|.| (random-next next))
         (iter n (1- iter)))
        ((= iter 0))
      (format t "~A " next))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))