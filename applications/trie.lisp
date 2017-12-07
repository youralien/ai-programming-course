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
  (cond ((null chars) trie)
        ((null (assoc (car chars) trie)) nil)
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
    (do ((word (read-line s nil :eof)
               (read-line s nil :eof)))
        ((eql word :eof) trie)
      (setq trie (add-word word trie)))))

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
    
    