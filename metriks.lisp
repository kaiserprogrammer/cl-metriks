(defpackage :metriks
  (:use :cl :lisp-unit))

(in-package :metriks)

(defclass memory-db () ())
(defclass meter ()
  ((marks :initarg :marks
          :initform 0
          :reader count-of)))

(defmethod mark ((m meter) &optional (by 1))
  (incf (slot-value m 'marks) by))


(remove-tests :all)

(defmacro with-threads (n &body body)
  `(let ((threads (list)))
     (dotimes (i ,n)
       (push (sb-thread:make-thread
              (lambda ()
                ,@body))
             threads))
      (dolist (thr threads)
        (sb-thread:join-thread thr))))

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

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
