(in-package :metriks)

(defclass gauge ()
  ((value :initarg :value
          :initform nil)
   (f)))

(defmethod initialize-instance :after ((g gauge) &key)
  (when (functionp (slot-value g 'value))
    (setf (slot-value g 'f) (slot-value g 'value))
    (setf (slot-value g 'value) nil)))

(defmethod value ((g gauge))
  (if (slot-boundp g 'f)
      (funcall (slot-value g 'f))
      (slot-value g 'value)))

(defmethod (setf value) (val (g gauge))
  (unless (slot-boundp g 'f)
    (setf (slot-value g 'value) val)))
