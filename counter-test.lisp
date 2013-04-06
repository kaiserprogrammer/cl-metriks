(in-package :metriks-test)

(defmacro with-setup (&body body)
  `(let ((counter (make-instance 'counter)))
     ,@body))

(define-test counter-increment
  (with-setup
    (increment counter)
    (assert-equal 1 (value counter))))

(define-test counter-increment-threaded
  (with-setup
    (with-threads 10
      (dotimes (j 100)
        (increment counter)))
    (assert-equal 1000 (value counter))))

(define-test counter-increment-by-more
  (with-setup
    (increment counter 10)
    (assert-equal 10 (value counter))))

(define-test counter-increment-by-more-threaded
  (with-setup
    (with-threads 10
      (dotimes (j 100)
        (increment counter 10)))
    (assert-equal 10000 (value counter))))
