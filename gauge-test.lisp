(in-package :metriks-test)

(define-test gauge
  (let ((gauge (make-instance 'gauge)))
    (dotimes (i 3)
      (setf (value gauge) i)
      (assert-equal i (value gauge)))
    (setf (value gauge) 1)
    (assert-equal 1 (value gauge))))

(define-test gauge-init-value
  (assert-equal nil (value (make-instance 'gauge)))
  (assert-equal 1 (value (make-instance 'gauge :value 1))))

(define-test gauge-callback
  (let ((i 1))
    (let ((gauge (make-instance 'gauge :value (lambda () (incf i)))))
      (assert-equal 2 (value gauge))
      (assert-equal 3 (value gauge)))))
