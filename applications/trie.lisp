; api design outlined in
; https://www.cs.northwestern.edu/academics/courses/325/exercises/challenges.php#word-trie

; (defstruct trie-node
;   word
;   a b c d e f g h i j k l m n o p q r s t u v w x y z
;   )

; (defun make-trie ()
;   (make-trie-node))

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
        nil
      (acons c (add-word-from-stream stream (make-trie)) trie))))

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

(defun main()
  (let* ((*trie* (make-trie))
         (*out* (add-word "hello" *trie*)))
    (print (assoc #\h *trie*))))
    
    