(in-package :metriks-test)

(define-test meter
  (let ((meter (make-instance 'meter)))
    (mark meter)
    (assert-equal 1 (count-of meter))))

(define-test meter-threaded
  (let ((m (make-instance 'meter)))
    (with-threads 10
      (dotimes (i 100)
        (mark m)))
    (assert-equal 1000 (count-of m))))

(define-test test-one-minute-rate
  (let ((m (make-instance 'meter)))
    (mark m 1000)
    (tick m)
    (assert-equal 200 (one-minute-rate m))))

