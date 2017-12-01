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
  (multiple-value-bind (min-coins-table last-coin-used-table exact-match-table)
                       (dp-make-change val coins)
      (let ((coin-usage-ht (make-coin-usage-ht coins)))
        (do ((change val (- change (svref last-coin-used-table change))))
            ((or (< change (smallest-coin coins))
                 (null (gethash (svref last-coin-used-table change) coin-usage-ht)))
             (values-list
               (mapcar #'(lambda (coin)
                           (gethash coin coin-usage-ht))
                       coins)))
          (incf (gethash (svref last-coin-used-table change) coin-usage-ht))))))

(defun make-coin-usage-ht (coins)
  (let ((coin-usage-ht (make-hash-table :size (length coins))))
    (dolist (coin coins)
      (setf (gethash coin coin-usage-ht) 0))
    coin-usage-ht))

(defun smallest-coin (coins)
  (car (reverse coins)))

(defun make-exact-match-table (val)
  ; boolean array where
  ; nth index correponds to the val in cents we are trying to represent with coins
  ; the value of elements say whether the val in cents can be represented by exact change 
  (let ((table (make-array (1+ val) :initial-element nil)))
    (setf (svref table 0) t)  ; base case for representing exact change.  0 cents can be represented exactly.
    table))

(defun make-valid-coins (cents coins)
  (mapcan #'(lambda (c) (if (>= cents c) (list c) nil)) coins))

(defun dp-make-change (val &optional (coins '(25 10 5 1)))
  ; val is the remaining value to make change from
  ; min-coins is an alist of the number of coins needed to make each value
  (do* ((cents 1 (1+ cents))
        (min-coins-table (make-array (1+ val) :initial-element 0)) ; nth element -> n cents
        (exact-match-table (make-exact-match-table val)) ; nth element -> n cents
        (last-coin-used-table (make-array (1+ val) :initial-element 0))) ; nth-element -> n cents
       ((> cents val) (values min-coins-table last-coin-used-table exact-match-table))
    (do ((valid-coins (make-valid-coins cents coins)
                      (cdr valid-coins))
         (coin-count cents)
         (exact-match nil)
         (last-coin-used (smallest-coin coins)))
        ((null valid-coins)
         (setf (svref exact-match-table cents) exact-match)
         (setf (svref min-coins-table cents) coin-count)
         (setf (svref last-coin-used-table cents) last-coin-used))
      (let ((this-coin-exact-match
              (svref exact-match-table (- cents (car valid-coins))))
            (this-coin-least-coin-count
              (< (1+ (svref min-coins-table (- cents (car valid-coins)))) coin-count)))
        (cond
          ((or (and this-coin-exact-match (null exact-match))
               (and this-coin-exact-match exact-match this-coin-least-coin-count))
           (setf exact-match t)
           (setf coin-count (1+ (svref min-coins-table (- cents (car valid-coins)))))
           (setf last-coin-used (car valid-coins)))
          (this-coin-least-coin-count
           (setf coin-count (1+ (svref min-coins-table (- cents (car valid-coins)))))
           (setf last-coin-used (car valid-coins))))))))

(run-tests make-best-change)

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

; (defun recursive-function (arg)
;   (cond ((atom arg) arg)
;         (t (cons arg (recursive-function (cdr arg))))))
    

; (defun recursive-function (arg)
;   (cond ((atom arg) arg)
;         (t (values-list
;              (cons arg (recursive-function (cdr arg)))))))

; (defun recursive-function (arg accum)
;   (cond ((atom arg) (values-list accum))
;         (t (recursive-function (cdr arg)
;                                (cons arg accum)))))

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