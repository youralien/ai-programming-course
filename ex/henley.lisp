(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun henley-p (string)
  (with-input-from-string (in string)
    (let ((prev `|.|))
      (flet ((word-follows-from-prev (symb)
                (when (null (assoc symb (gethash prev *words*)))
                  (return-from henley-p nil))
                (setf prev symb)))
        (read-stream in #'word-follows-from-prev))))
  t)

; read-text calls new general function read-stream
; why? better layers of abstraction. read-text can be summarized as
; 1) open the text file as a stream
; 2) apply the function see (which is making the *words* datastruct)
;    to each symbol in the stream
; 3) return number of entries in hashtable
(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (read-stream s (make-see)))
  (hash-table-count *words*))

; read-stream handles reading characters at a time until a complete
; "word" is formed. then it apply's function to the "word".
; why? useful generalization, i.e. used in read-text and henley-p
; refactors:
; 1) removed the top level let, since do can initialize buffer and pos
; 2) made function word-part-p, which returns true if character
;    is an alpha or apostrophe
; 3) pos is accumulated in the do, instead of using incf. The update of pos
;    depended on word-part-p
; The definition is still too long according to lisp critic, but I argue
; that it is more clear about how the mechanism of writing to/from the buffer works,
; and certaintly shorter than the original from graham
(defun word-part-p (c)
  (or (alpha-char-p c) (char= c #\')))

(defun read-stream (stream function)
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof))
       (buffer (make-string maxword) buffer)
       (pos 0 (if (word-part-p c) (1+ pos) 0)))
      ((eql c :eof))
    (cond
      ((word-part-p c) (setf (aref buffer pos) c))
      (t (unless (zerop pos)
           (funcall function (intern (string-downcase
                          (subseq buffer 0 pos)))))
         (let ((p (punc c)))
           (when p (funcall function p)))))))

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