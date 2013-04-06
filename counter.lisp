(in-package :metriks)

(defclass counter ()
  ((value :initarg :value
          :initform 0
          :accessor value)))

(defmethod increment ((c counter) &optional (by 1))
  (incf (value c) by))
