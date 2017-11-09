(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun henley-p (string)
  (with-input-from-string (in string)
    (let* ((word-follows-from-prev nil)
           (prev `|.|))
      (setq word-follows-from-prev (lambda (symb)
        (let ((pair (assoc symb (gethash prev *words*))))
          (when (null pair)
            (return-from henley-p nil)))
        (setf prev symb)))
      (read-stream in word-follows-from-prev)))
  (return-from henley-p t))

; read-text calls new general function read-stream
; why? better layers of abstraction. read-text can be summarized as
; 1) open the text file as a stream
; 2) apply the function see (which is making the *words* datastruct)
;    to each symbol in the stream
; 3) return number of entries in hashtable
(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (read-stream s (make-see)))
  (return-from read-text (hash-table-count *words*)))

; read-stream handles reading characters at a time until a complete
; "word" is formed. then it apply's function to the "word".
; why? useful generalization, i.e. used in read-text and henley-p
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

; refactored (defun see (...)) to make-see that returns a closure of
; what see did.
(defun make-see ()
  (let ((prev `|.|))
    (return-from make-see (lambda (symb)
      (let ((pair (assoc symb (gethash prev *words*))))
        (if (null pair)
            (push (cons symb 1) (gethash prev *words*))
            (incf (cdr pair))))
      (setf prev symb)))))

; made generate-text iterative.
; the recursive version in lisp didn't benefit from tail-recursion
; optimization.  in addition, there's no longer an additional prev
; parameter. it assumes we want to start generating new sentences.
(defun generate-text (n)
  (if (zerop n)
      (terpri)
    (do ((next '|.| (random-next next))
         (iter n (1- iter)))
        ((= iter 0))
      (unless (eql iter n)
        (format t "~A " next)))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))