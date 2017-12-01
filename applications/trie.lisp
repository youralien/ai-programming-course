; api design outlined in
; https://www.cs.northwestern.edu/academics/courses/325/exercises/challenges.php#word-trie
; in order to test...
; load trie-tests.lisp
; switch packages (in-package :trie-tests)
; load this file
; try running the tests to see if functions were properly exported

(defpackage #:trie
  (:use #:common-lisp)
  (:export #:make-trie #:add-word #:subtrie))

(in-package #:trie)

(defun make-trie ()
  nil)

(defun add-word (str trie)
  (let* ((lower (string-downcase str))
         (stream (make-string-input-stream lower)))
    (let ((c (read-char stream nil :eow)))
      (acons c (add-word-from-stream stream (make-trie)) trie))))

(defun add-word-from-stream (stream trie)
  (let ((c (read-char stream nil :eow)))
    (if (eql c :eow)
        nil  ; probably add the word to this node in some way.
      (acons c (add-word-from-stream stream (make-trie)) trie))))

(defun subtrie (trie &rest chars)
  nil)

; (defun add-word (str trie)
;   (let ((s (make-string-input-stream str)))
;     (do* ((c (read-char s nil :eow)
;              (read-char s nil :eow))
;           (prevnode trie node)
;           (node (make-trie) (make-trie))
;           (prevnode (acons c node trie) (acons c node prevnode)))
;          ((eql c :eow) trie))))
;       ; todo: not changing trie; it's not aliasing and binding like I want.
;       ; (print node))))

; (defvar *trie* (main))

; (defun main()
;   (let* ((trie (make-trie))
;          (out (add-word "hello" trie)))
;     (print (assoc #\h out))
;     out))
    
    