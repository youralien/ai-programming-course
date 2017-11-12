; some doesn't return the index, so maybe not useful
; I could imagine making a hash table to store the indexes
; of each of the coins.  Then using some, I could find the remainder,
; keep a tally of the index, and recurse solving make-change for the
; the remaining val
(defun make-change (val &optional (coins '(25 10 5 1)))
  (let ((coin-usage-ht (make-hash-table :size (length coins))))
    (dolist (coin coins)
      (setf (gethash coin coin-usage-ht) 0))
    (subtract-and-record val coins coin-usage-ht)))
    

(defun subtract-and-record (val coins coin-usage-ht)
  (let ((use-coin (some #'(lambda (coin)
                         (if (>= val coin)
                             coin
                            nil))
                      coins)))
    (cond
      ((null use-coin)
       (values-list
         (mapcar #'(lambda (coin)
                     (gethash coin coin-usage-ht))
                 coins)))
      (t (incf (gethash use-coin coin-usage-ht))
         (subtract-and-record (- val use-coin)
                              coins
                              coin-usage-ht)))))
    

; i don't like this I am brain dead
; (defun make-change (val &optional (coins '(25 10 5 1)))
;   (if (zerop val)
;       (values-list (make-list (length coins) :initial-element 0))
;     (do* ((i 0)
;           (coin-length (length coins))
;           (change val)
;           (remain (- change (nth i coins))
;                   (- change (nth i coins)))
;           (coin-counts (make-list coin-length :initial-element 0)))
;          ((= remain 0) 
;           (incf (nth i coin-counts))
;           (values-list coin-counts))
;       (cond
;         ((not (minusp remain))
;          (setf change remain)
;          (incf (nth i coin-counts)))
;         (t
;          (unless (= i (1- coin-length))
;            (incf i)))))))