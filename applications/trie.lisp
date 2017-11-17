; api design outlined in
; https://www.cs.northwestern.edu/academics/courses/325/exercises/challenges.php#word-trie

(defstruct trie-node
  word
  a b c d e f g h i j k l m n o p q r s t u v w x y z
  )

(defun make-trie ()
  (make-trie-node))

; trying to do trie-node-a by passing the letter a to a func/macro
(deffunc trie-letter (letter trie)
  `(trie-node-,letter ,trie))

(defun add-word (str trie)
  (let ((s (make-string-input-string str)))
    (do ((c (read s nil :eow)
            (read s nil :eow))
         (node trie (trie-node-))))))
