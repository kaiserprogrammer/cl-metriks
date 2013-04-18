(in-package :metriks-test)

(define-test timer-without-block
  (let ((tm (make-instance 'timer)))
    (let ((s (time tm)))
      (sleep 0.1)
      (stop s))
    (assert-equal 0.1 (mean tm))))

(define-test timer
  (let ((tm (make-instance 'timer)))
    (dotimes (i 3)
      (with-timer tm
        (sleep 0.1)))
    (assert-equal 0.1 (mean tm))
    (assert-equal 0.1 (median (snapshot tm)))))
