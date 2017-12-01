(defun make-change (val &optional (coins '(25 10 5 1)))
  (values-list (make-change-as-list val coins)))

(defun make-change-as-list (val coins)
  (cond
    ((null coins) nil)
    (t (multiple-value-bind (quotient remainder)
            (floor val (car coins))
          (cons quotient (make-change-as-list remainder (cdr coins)))))))

; some doesn't return the index, so maybe not useful
; I could imagine making a hash table to store the indexes
; of each of the coins.  Then using some, I could find the remainder,
; keep a tally of the index, and recurse solving make-change for the
; the remaining val
(defun make-change-complex (val &optional (coins '(25 10 5 1)))
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
      ; no more coins to use, return the counts we found
      ((null use-coin)
       (values-list
         (mapcar #'(lambda (coin)
                     (gethash coin coin-usage-ht))
                 coins)))
      ; 1) increment our count of the coin
      ; 2) subtract the coin, update the value, and do it again.
      (t (incf (gethash use-coin coin-usage-ht))
         (subtract-and-record (- val use-coin)
                              coins
                              coin-usage-ht)))))

(defun make-best-change (val &optional (coins '(25 10 5 1)))
  (multiple-value-bind (min-coins-table last-coin-used-table)
                       (dp-make-change val coins)
      (print min-coins-table)
      (print last-coin-used-table)
      (let ((coin-usage-ht (make-hash-table :size (length coins))))
        (dolist (coin coins)
          (setf (gethash coin coin-usage-ht) 0))
        (do* ((change val (- change (svref last-coin-used-table change))))
             ((or (<= change 0)
                  (null (gethash (svref last-coin-used-table change) coin-usage-ht)))
              (print "IVE ARRIVED")
              (values-list
                (mapcar #'(lambda (coin)
                            (gethash coin coin-usage-ht))
                        coins)))
          (print "before gethash")
          (print "(svref last-coin-used-table change)")
          (print (svref last-coin-used-table change))
          (incf (gethash (svref last-coin-used-table change) coin-usage-ht))
          (print change)
          (print "used")
          (print (svref last-coin-used-table change))
          (print "how many times? ")
          (print "once")
          (print "nextchange")
          (print (- change (svref last-coin-used-table change)))))))

; (defun sub-and-rec (val min-coins-table last-coins-used-table coin-usage-hts)
;   (cond
;     (null val)))

; val is the remaining value to make change from
; min-coins is an alist of the number of coins needed to make each value
(defun dp-make-change (val &optional (coins '(25 10 5 1)))
  (do* ((cents 0 (1+ cents))
       (valid-coins nil (mapcan #'(lambda (c)
                                   (if (>= cents c)
                                       (list c)
                                      nil))
                                coins))
       (last-coin-used (car (reverse coins)) (car (reverse coins)))
       ; (last-coin-used -1 -1)
       (coin-count cents cents)
       (min-coins-table (make-array (1+ val) :initial-element 0)) ; nth element -> n cents
       (last-coin-used-table (make-array (1+ val) :initial-element 0))) ; nth-element -> n cents
      ((> cents val) (values min-coins-table last-coin-used-table))
    (print cents)
    ; (print valid-coins)
    ; (print last-coin-used)
    ; (print min-coins-table)
    ; (print last-coin-used-table)
    ; (print "----------")
    (dolist (coin (reverse valid-coins))
      ; (let ((foo (or (and (plusp (- cents coin))
      ;                     (1+ (svref min-coins-table (- cents coin))))
      ;                1)))
        (cond
          ((> coin cents) nil)
          ((< (1+ (svref min-coins-table (- cents coin))) coin-count) ; weird...
           ; 1 + (number of coins from remaining cents, had you used this coin)
           ; or just 1 if the coin can't be used.
           (when (= cents 14)
             (format t "tmp-coin-count: ~A coin-count: ~A last-coin-used: ~A ~C"
                     (1+ (svref min-coins-table (- cents coin))) coin-count last-coin-used #\linefeed))
           (setf coin-count (1+ (svref min-coins-table (- cents coin))))
           (setf last-coin-used coin)
           (when (= cents 14)
             (format t "tmp-coin-count: ~A coin-count: ~A last-coin-used: ~A ~C"
                     (1+ (svref min-coins-table (- cents coin))) coin-count last-coin-used #\linefeed)))
          ))
    ; observations
    ; (make-best-change 32 '(11 7)) succeeds when this line is commented out
    ; (MAKE-BEST-CHANGE 11 '(7 3)) fails when this line is commented out
    ; (MAKE-BEST-CHANGE 88 '(23 13 5)) fails no matter
    ; the dp tables turn out differently with this line commented
    ; compare when we try to control for the case where there is no valid coin
    ; #(0 0 0 0 0 0 0 1 1 1 1 1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  2  3  3  3  3) 
    ; #(0 0 0 0 0 0 0 7 7 7 7 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11)
    ; it makes more sense that not until 7 cents can we use one coin to represent.
    ; or that 8 cents is really represented by a 7 cent coin only too.
    ; but it really misses out on factors of 7, like 14, which is represented by one 11?! not two 7's? That's hellish...
    
    ; and then when we dont
    ; #(0 1 2 3 4 5 6 1 2 3 4 1  2  3  2 3 4 5 2  3  4  3 2  3  4  3  4  5  4 3  4  5  4) 
    ; #(7 7 7 7 7 7 7 7 7 7 7 11 11 11 7 7 7 7 11 11 11 7 11 11 11 11 11 11 7 11 11 11 11) 
    ; you can see that for 0 - 6, it doesn't make sense that a 6 can be made up of 6 coins.
    
    ; only if there are valid-coins
    ; (i.e. coins who's value is lessor or equal to the current cents) do set do the setf's
    (unless (null (car valid-coins))
      (setf (svref min-coins-table cents) coin-count)
      (setf (svref last-coin-used-table cents) last-coin-used))))

; attempt at just doing a recursive make-best change one, leading towards memoization
(defun main (val &optional (coins '(25 10 5 1)))
  (let ((perfect-change (mapcar #'(lambda (coin)
                                 (if (= val coin)
                                     1
                                    0))
                              coins)))
    (format t "perfect-change: ~A~C" perfect-change #\linefeed)
    (cond
      ((some #'plusp perfect-change) perfect-change)
      (t 
        (let ((valid-coin (mapcan #'(lambda (coin)
                                (if (> val coin)
                                    (list coin)
                                  nil))
                            coins))
              (minCoins val)
              (minCoinSet nil))
          (format t "valid-coin: ~A~C" valid-coin #\linefeed)
          ; for the valid coins, use one of the coins, increment 
          (dolist (coin valid-coin)
            (let* ((coin-set (mapcar #'(lambda (c)
                                       (if (= c coin)
                                           1
                                          0))
                                    coins))
                   (res (main (- val coin) coins)))
              (format t "res: ~A~C" res #\linefeed)
              (format t "coin: ~A~C" coin #\linefeed)
              (format t "coin-set: ~A~C" coin-set #\linefeed)
              (let (numCoins (reduce #'+ (add-lists coin-set res)))
                (format t "numCoins: ~A~C" numCoins #\linefeed)
                (cond ((< numCoins minCoins)
                       (setq minCoins numCoins)
                       (setq minCoinSet (add-lists coin-set res)))))))
          minCoinSet)))))
          
(defun coin-from-coinset (coins coin-set)
  (let (out (mapcan #'(lambda (coin coin-bool)
              (progn
                (if (= coin-set 1)
                    (list coin)
                  nil)
                (print coin-bool)))
            coins coin-set))
    (if (eql 1 (length out))
        nil
      (car out))))

(define-test coin-from-coinset
  (assert-equal 25 (coin-from-coinset '(25 10 5 1) '(1 0 0 0)))
)
          
(defun add-lists (lst1 lst2)
  (mapcar #'+ lst1 lst2))

(defun recursive-function (arg)
  (cond ((atom arg) arg)
        (t (cons arg (recursive-function (cdr arg))))))
    

(defun recursive-function (arg)
  (cond ((atom arg) arg)
        (t (values-list
             (cons arg (recursive-function (cdr arg)))))))

(defun recursive-function (arg accum)
  (cond ((atom arg) (values-list accum))
        (t (recursive-function (cdr arg)
                               (cons arg accum)))))

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