(in-package #:glist-tests)

;;; Test cases for the GLIST exercises.
;;;
;;; Updates:
;;; 11/20/2014 more tests for GMAP
;;; 11/19/2015 added GREDUCE tests, GRANGE utility
;;; 01/16/2006 File created [CKR]

;;; (GRANGE start end) is a generated list
;;; that signals an error if run to end. 
;;; Used in tests to catch code that extracts
;;; more values than necessary.

(defun grange (start end)
  (if (< start end)
      (gcons start (grange (1+ start) end))
    (error "end ~S of GRANGE reached" end)))

(defun gnth (n glist)
  (cond ((= n 0) (gcar glist))
        (t (gnth (1- n) (gcdr glist)))))

(define-test gnth
  (assert-equal 1 (gnth 0 (grange 1 2)))
  (assert-equal 5 (gnth 4 (grange 1 6)))
  (assert-equal nil (gnth 10 (delay '(a b c))))
  )

(defun gmap (fn glist)
  (cond ((gnull glist) glist)
        (t (cons (funcall fn (gcar glist))
                 (delay (gmap fn (gcdr glist)))))))

(define-test gmap
  (assert-equal nil (gextract (gmap #'1+ nil)))
  (assert-equal '(2 3 4 5) (gextract (gmap #'1+ (gintegers 1)) 4))
  (assert-equal '(2 3 4 5) (gextract (gmap #'1+ (grange 1 5)) 4))
  (assert-true (gnull (gmap #'1+ (delay nil))))
  )

(define-test gfilter
  (assert-equal nil (gextract (gfilter #'1+ nil)))
  (assert-equal '(1 3 5) (gextract (gfilter #'oddp (grange 1 6)) 3))
  (assert-equal '(1 3 5 7) (gextract (gfilter #'oddp (gintegers 1)) 4))
  (assert-equal nil
                (gextract (gfilter #'oddp (delay '(2 4 6 8 10 12 14))) 4))
  )

(define-test gscan
  (assert-equal '(1 4 9)
                (gextract
                 (gscan '+ 
                        (gfilter #'oddp (grange 1 6))
                        0)
                 3))
  (assert-equal nil 
                (gextract 
                 (gscan '+ 
                        (gfilter #'oddp (delay '(2 4 6 8 10))))))
  )

(define-test greduce
  (assert-equal 9
                (greduce '+ 
                         (gfilter #'oddp (grange 1 6))
                         :initial-value 0
                         :end 3))
  (assert-equal 0 
                (greduce '+ 
                         (gfilter #'oddp (delay '(2 4 6 8 10)))
                         :initial-value 0
                         :end 3))
  (assert-equal 12
                (greduce '+ 
                         (gfilter #'oddp (grange 1 10)) 
                         :initial-value 0
                         :start 2 :end 4))
  )