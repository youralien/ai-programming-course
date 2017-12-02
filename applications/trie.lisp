; api design outlined in
; https://www.cs.northwestern.edu/academics/courses/325/exercises/challenges.php#word-trie
; in order to test...
; load trie-tests.lisp
; switch packages (in-package :trie-tests)
; load this file
; try running the tests to see if functions were properly exported

(defpackage #:trie
  (:use #:common-lisp)
  (:export #:make-trie #:add-word #:subtrie #:trie-word #:trie-count
           #:mapc-trie #:read-words))

(in-package #:trie)

(defconstant maxword 100)

(defun make-trie ()
  nil)


; TODO: change (setq trie (acons key val trie)) to apush macro?
(defun add-word (str trie)
  (let* ((lower (string-downcase str))
         (stream (make-string-input-stream lower)))
    (let ((c (read-char stream nil :eow)))
      (if (assoc c trie)
          (setf (cdr (assoc c trie))
                (add-word-from-stream lower stream (cdr (assoc c trie))))
        (setq trie
          (acons c (add-word-from-stream lower stream (make-trie))
                 trie)))))
  trie)

(defun add-word-from-stream (word stream trie)
  (let ((c (read-char stream nil :eow)))
    (cond
      ((eql c :eow)
       (acons :word word trie))
      ((assoc c trie)
       (setf (cdr (assoc c trie))
             (add-word-from-stream word stream (cdr (assoc c trie))))
       trie)
      (t (acons c (add-word-from-stream word stream (make-trie))
                trie)))))

(defun subtrie (trie &rest chars)
  (cond ((null (car chars)) trie) ; you go not chars, so return it right back
        ((null (assoc (car chars) trie)) nil) ; there's no character like that in the trie node
        ((null (cdr chars))
         (cdr (assoc (car chars) trie)))
        (t (apply #'subtrie (cons (cdr (assoc (car chars) trie))
                                  (cdr chars))))))

(defun trie-word (trie)
  (cdr (assoc :word trie)))


; idea is to crawl down every branch of the trie
; if the subtrie you are looking at has :word, then increment the counter

(defun trie-count (trie)
  (let ((out (mapcar #'(lambda (pair)
                        (cond
                          ((eql :word (car pair)) 1)
                          (t (trie-count (cdr pair)))))
                     trie)))
    (cond
      ((null out) 0)
      ((atom out) out)
      ((null (cdr out)) (car out))
      (t (reduce #'+ out)))))
              
(defun mapc-trie (fn trie)
  (dolist (pair trie)
    (unless (eql :word (car pair))
      (funcall fn (car pair) (cdr pair)))))

(defun read-words (file trie)
  (with-open-file (s file :direction :input)
    (read-stream s #'(lambda (word)
                      (setq trie (add-word word trie)))))
  trie)


(defun word-part-p (c)
  (or (alpha-char-p c) (char= c #\')))

; TODO: reads the correct number if there is an extra
; new line at the end of crosswd.txt SO fix it so that
; it can do the same without the newline at end
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


; (defun trie-count (trie)
;   (print trie)
;   ; (print (trie-word trie))
;   (cond
;     ((null (trie-word trie))
;      (mapcar #'(lambda (pair)
;                      ; (format t "pair: ~A~C" pair #\linefeed)
;                      (cond
;                        ((trie-word (cdr pair))
;                         (print "---- GOTEM ----")
;                         (1+ (trie-count (cdr pair))))
;                        ((null (cdr pair))
;                         (format t "pair: ~A~C" pair #\linefeed)
;                         1)
;                        (t (trie-count (cdr pair)))))
;                   trie))
;     (t (print "woohoo") (trie-count (cdr trie)))))
    
    