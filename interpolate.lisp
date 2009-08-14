(in-package #:l-math)

(defgeneric linear-interpolation (start end t-val)
  (:documentation "Interpolates between two points, using the
  parametric line equation. START and END are the two endpoints of a
  line. While T-VAL takes values between 0 and 1, inclusive, the
  returned points will lie between these two points. No matter the
  value of T-VAL, the returned point will always be on the line, but
  will be outside of the [start, end] line segment if T-VAL is not on
  the domain [0, 1]")
  (:method ((start vector) (end vector) (t-val real))
    (let ((direction (- end start)))
      (+ start (* direction t-val)))))
