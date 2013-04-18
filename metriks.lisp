(defpackage :metriks
  (:use :cl :lisp-unit))

(in-package :metriks)


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

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
