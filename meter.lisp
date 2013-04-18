(in-package :metriks)

(defclass ewma-creator ()
  ((interval :initform 5
             :initarg :interval)
   (m1-alpha)
   (m5-alpha)
   (m15-alpha)))

(defmethod initialize-instance :after ((e ewma-creator) &key)
  (with-slots (interval m1-alpha m5-alpha m15-alpha) e
    (setf m1-alpha (exp (/ (/ (- interval) 60) 1)))
    (setf m5-alpha (exp (/ (/ (- interval) 60) 5)))
    (setf m15-alpha (exp (/ (/ (- interval) 60) 15)))))

(defmethod new-m1 ((e ewma-creator))
  (make-instance 'ewma
                 :alpha (slot-value e 'm1-alpha)
                 :interval (slot-value e 'interval)))
(defmethod new-m5 ((e ewma-creator))
  (make-instance 'ewma
                 :alpha (slot-value e 'm5-alpha)
                 :interval (slot-value e 'interval)))
(defmethod new-m15 ((e ewma-creator))
  (make-instance 'ewma
                 :alpha (slot-value e 'm15-alpha)
                 :interval (slot-value e 'interval)))

(defclass ewma ()
  ((alpha :initarg :alpha)
   (interval :initarg :interval)
   (initialized :initform nil)
   (rate :initform 0
         :reader rate)
   (uncounted :initform 0)))

(defmethod clear ((e ewma))
  (with-slots (initialized rate uncounted) e
    (setf initialized nil)
    (setf rate 0)
    (setf uncounted 0)))

(defmethod update ((e ewma) value)
  (incf (slot-value e 'uncounted) value))

(defmethod tick ((e ewma))
  (with-slots (uncounted interval initialized rate alpha) e
    (let (count)
      (setf count uncounted
            uncounted 0)
      (let ((instant-rate (/ count interval)))
        (if initialized
            (incf rate (* alpha (- instant-rate rate)))
            (progn
              (setf rate instant-rate)
              (setf initialized t)))))))

(defvar *tick-interval* 5)
(defvar *averager-creator* (make-instance 'ewma-creator))

(defclass meter ()
  ((count :initarg :count
          :initform 0
          :reader count-of)
   (start-time :initform (get-universal-time))
   (last-tick)
   (m1-rate)
   (m5-rate)
   (m15-rate)))

(defmethod initialize-instance :after ((m meter) &key)
  (with-slots (start-time last-tick m1-rate m5-rate m15-rate) m
   (setf last-tick start-time)
   (setf m1-rate (new-m1 *averager-creator*))
   (setf m5-rate (new-m5 *averager-creator*))
   (setf m15-rate (new-m15 *averager-creator*))))

(defmethod clear ((m meter))
  (with-slots (count start-time last-tick m1-rate m5-rate m15-rate) m
    (setf count 0)
    (setf start-time (get-universal-time))
    (setf last-tick start-time)
    (clear m1-rate)
    (clear m5-rate)
    (clear m15-rate)))

(defmethod tick ((m meter))
  (with-slots (m1-rate m5-rate m15-rate) m
    (tick m1-rate)
    (tick m5-rate)
    (tick m15-rate)))

(defmethod tick-if-necessary ((m meter))
  (with-slots (last-tick) m
    (let* ((new-tick (get-universal-time))
           (age (- new-tick last-tick)))
      (when (> age *tick-interval*)
        (let ((required-ticks (truncate (/ age *tick-interval*))))
          (dotimes (i required-ticks)
            (tick m)))))))

(defmethod mark ((m meter) &optional (by 1))
  (tick-if-necessary m)
  (with-slots (count m1-rate m5-rate m15-rate) m
    (incf count by)
    (update m1-rate by)
    (update m5-rate by)
    (update m15-rate by)))

(defmethod one-minute-rate ((m meter))
  (tick-if-necessary m)
  (rate (slot-value m 'm1-rate)))
(defmethod five-minute-rate ((m meter))
  (tick-if-necessary m)
  (rate (slot-value m 'm5-rate)))
(defmethod fifteen-minute-rate ((m meter))
  (tick-if-necessary m)
  (rate (slot-value m 'm15-rate)))
(defmethod mean-rate ((m meter))
  (with-slots (count start-time) m
    (if (zerop count)
        0
        (let ((elapsed (- (get-universal-time) start-time)))
          (/ count elapsed)))))
