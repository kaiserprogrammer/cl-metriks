(defpackage :metriks-test
  (:use :cl :metriks :lisp-unit))

(in-package :metriks-test)



(remove-tests :all)

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
