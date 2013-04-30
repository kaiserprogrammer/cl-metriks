(defpackage :metriks
  (:use :cl :lisp-unit))

(in-package :metriks)

(defvar *default-sample-size* 1028)
(defvar *default-alpha* 0.015)

(defclass snapshot ()
  ((values :initarg :values)))

(defmethod initialize-instance :after ((s snapshot) &key)
  (with-slots (values) s
    (setf values (sort values #'<))))

(defmethod value ((s snapshot) quantile)
  (with-slots (values) s
    (if (zerop (length values))
        0
        (let ((pos (* quantile (1+ (length values)))))
          (cond ((< pos 1) (aref values 0))
                ((>= pos (length values)) (aref values (1- (length values))))
                (t (let ((lower (aref values (1- (truncate pos))))
                         (upper (aref values (truncate pos))))
                     (+ lower (* (- pos (truncate pos)) (- upper lower))))))))))

(defmethod median ((s snapshot))
  (value s 0.5))

(defmethod 75th-percentile ((s snapshot))
  (value s 0.75))
(defmethod 95th-percentile ((s snapshot))
  (value s 0.95))
(defmethod 98th-percentile ((s snapshot))
  (value s 0.98))
(defmethod 99th-percentile ((s snapshot))
  (value s 0.99))
(defmethod 999th-percentile ((s snapshot))
  (value s 0.999))

(defclass uniform-sample ()
  ((values)
   (count :initform 0)))

(defclass exponentially-decaying-sample ()
  ())

(defmethod initialize-instance :after ((u uniform-sample) &key (size *default-sample-size*))
  (setf (slot-value u 'values) (make-array (list size))))

(defmethod clear ((u uniform-sample))
  (setf (slot-value u 'count) 0))

(defmethod snapshot ((u uniform-sample))
  (with-slots (count values) u
    (make-instance 'snapshot :values (subseq values 0 count))))

(defmethod update ((u uniform-sample) value)
  (with-slots (values count) u
    (let ((new-count (incf count)))
      (if (<= new-count (length values))
          (setf (aref values (1- new-count)) value)
          (let ((idx (random new-count)))
            (when (< idx (length values))
              (setf (aref values idx) value)))))))

(defclass histogram ()
  ((sample :initarg :sample)
   (count :initform 0)
   (min :initform nil
        :reader minimum)
   (max :initform nil
        :reader maximum)
   (sum :initform 0)
   (variance :initform (list -1 0))))

(defmethod mean ((h histogram))
  (with-slots (count sum) h
    (if (> count 0)
        (/ sum count)
        0)))

(defmethod snapshot ((h histogram))
  (snapshot (slot-value h 'sample)))

(defmethod clear ((h histogram))
  (with-slots (sample count min max sum variance) h
    (clear sample)
    (setf count 0)
    (setf min nil)
    (setf max nil)
    (setf sum 0)
    (setf variance (list -1 0))))

(defmethod update ((h histogram) value)
  (with-slots (sample count min max sum variance) h
    (incf count)
    (update sample value)
    (setf max (if max
                  (max max value)
                  value))
    (setf min (if min
                  (min min value)
                  value))
    (incf sum value)
    (update-variance h value)))

(defmethod update-variance ((h histogram) value)
  (let* ((old (slot-value h 'variance))
         (oldm (first old))
         (olds (second old)))
    (setf (slot-value h 'variance)
          (if (= oldm -1)
              (list value 0)
              (let ((newm (+ oldm (/ (- value oldm) (slot-value h 'count)))))
                (list newm
                      (+ olds (* (- value oldm) (- value newm)))))))))

(remove-tests :all)

(defmacro uni-setup (&body body)
  `(let ((h (make-instance 'histogram :sample (make-instance 'uniform-sample))))
     ,@body))

(defmacro exp-setup (&body body)
  `(let ((h (make-instance 'histogram :sample (make-instance 'exponentially-decaying-sample))))
     ,@body))

(define-test uniform-sample-min-max-mean
  (uni-setup
   (update h 5)
   (update h 10)
   (assert-equal 5 (or (format t "wtf: ~a" (minimum h))
                       (minimum h)))
   (assert-equal 10 (maximum h))
   (assert-equal 7 (truncate (mean h)))))

(define-test uniform-sample-mean-threaded
  (uni-setup
   (with-threads 10
     (dotimes (i 100)
       (update h 5)
       (update h 10)))
   (assert-equal 7 (truncate (mean h)))))

(define-test uniform-sample-2000
  (uni-setup
   (dotimes (i 2000)
     (update h i))
   (assert-equal 1999 (maximum h))))

(define-test uniform-sample-2000-threaded
  (uni-setup
   (with-threads 10
     (dotimes (i 2000)
       (when (= (mod i 10) threadnr)
         (update h i))))))

(define-test uniform-sample-snapshot
  (uni-setup
   (dotimes (i 100)
     (update h i))
   (let ((snapshot (snapshot h)))
     (assert-equal 49.5 (median snapshot)))))

(define-test uniform-sample-snapshot-threaded
  (uni-setup
   (with-threads 10
     (dotimes (i 100)
       (update h i)))
   (let ((snapshot (snapshot h)))
     (assert-equal 49.5 (median snapshot)))))

(define-test exponential-sample-min
  (exp-setup
   (update h 5)
   (update h 10)
   (assert-equalp 5 (minimum h))))

(define-test exponential-sample-max
  (exp-setup
   (update h 5)
   (update h 10)
   (assert-equalp 10 (maximum h))))

(define-test exponential-sample-mean
  (exp-setup
   (update h 5)
   (update h 10)
   (assert-equalp 7 (mean h))))

(define-test exponential-sample-mean-threaded
  (exp-setup
    (with-threads 10
      (dotimes (i 100)
        (update h 5)
        (update h 10)))
    (assert-equalp 7 (mean h))))

(define-test exponential-sample-2000
  (exp-setup
    (dotimes (i 2000)
      (update h i))
    (assert-equalp 1999 (maximum h))))

(define-test exponential-sample-2000-threaded
  (exp-setup
    (with-threads 10
      (dotimes (idx 2000)
        (when (= threadnr (mod idx 10))
          (update h idx))))
    (assert-equalp 1999 (maximum h))))

(define-test exponential-sample-snapshot
  (exp-setup
    (dotimes (i 100)
      (update h i))
    (assert-equalp 49.5 (median (snapshot h)))))

(define-test exponential-sample-snapshot-threaded
  (exp-setup
    (with-threads 10
      (dotimes (i 100)
        (update h i)))
    (assert-equalp 49.5 (median (snapshot h)))))


(defmacro with-threads (n &body body)
  `(let ((threads (list)))
     (dotimes (threadnr ,n)
       (push (sb-thread:make-thread
              (lambda ()
                ,@body))
             threads))
     (dolist (thr threads)
       (sb-thread:join-thread thr))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
