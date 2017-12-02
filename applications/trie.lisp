; api design outlined in
; https://www.cs.northwestern.edu/academics/courses/325/exercises/challenges.php#word-trie
; in order to test...
; load trie-tests.lisp
; switch packages (in-package :trie-tests)
; load this file
; try running the tests to see if functions were properly exported

(defpackage #:trie
  (:use #:common-lisp)
  (:export #:make-trie #:add-word #:subtrie #:trie-word #:trie-count))

(in-package #:trie)

(defun make-trie ()
  nil)

(defun add-word (str trie)
  (let* ((lower (string-downcase str))
         (stream (make-string-input-stream lower)))
    (let ((c (read-char stream nil :eow)))
      (if (assoc c trie)
          (rplacd (assoc c trie)
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
       (rplacd (assoc c trie)
               (add-word-from-stream word stream (cdr (assoc c trie))))
       trie)
      (t (acons c (add-word-from-stream word stream (make-trie))
                trie)))))

(defun subtrie (trie &rest chars)
  (pprint "trie: ")
  (pprint trie)
  ; (print "char1: ")
  ; (print (car chars))
  ; (print "chars")
  ; (print chars)
  ; (print "does the trie have this?: ")
  ; (print (assoc (car chars) trie))
  (cond ((null (car chars)) trie) ; you go not chars, so return it right back
        ((null (assoc (car chars) trie)) nil) ; there's no character like that in the trie node
        ((null (cdr chars))
         ; (print "chars again")
         ; (print chars)
         ; (print "(assoc (car chars) trie)")
         ; (print (assoc (car chars) trie))
         ; (print "(cdr (assoc (car chars) trie))")
         ; (print (cdr (assoc (car chars) trie)))
         (cdr (assoc (car chars) trie)))
        (t ; (print "cdr chars")
           ; (print (cdr chars))
           (apply #'subtrie (cons (cdr (assoc (car chars) trie))
                                  (cdr chars))))))

(defun trie-word (trie)
  (cdr (assoc :word trie)))


; idea is to crawl down every branch of the trie
; if the subtrie you are looking at has :word, then increment the counter

(defun trie-count (trie)
  ; (print trie)
  ; (print (trie-word trie))
  (cond
    ((null (trie-word trie))
     (reduce #'+ (mapcar #'(lambda (pair)
                     ; (format t "pair: ~A~C" pair #\linefeed)
                     (cond
                       ((trie-word (cdr pair)) 1)
                       (t (trie-count (cdr pair)))))
                  trie)))
    (t (print "woohoo") 1)))
    
    