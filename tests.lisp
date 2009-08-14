(in-package #:l-math)

(declaim (inline test-dimensions))

(defmacro etest-dimensions (lhs rhs)
  (let ((lhs-val-sym (gensym))
	(rhs-val-sym (gensym)))
    `(let ((,lhs-val-sym ,lhs)
	   (,rhs-val-sym ,rhs))
       (unless (= ,lhs-val-sym ,rhs-val-sym)
	 (error 'dimension-error :dim1 ,lhs-val-sym :dim2 ,rhs-val-sym)))))

(defgeneric test-dimensions (lhs rhs &key)
  (:documentation "Throws an error if the two objects do not agree in
  dimensions. If the second object is a matrix, it is possible to
  transpose the dimensions that are tested using the :transpose-rhs
  argument."))

(defmethod test-dimensions ((lhs vector) (rhs vector) &key)
  (etest-dimensions (length lhs) (length rhs)))

(defmethod test-dimensions ((lhs matrix) (rhs vector) &key)
  (etest-dimensions (matrix-cols lhs) (length rhs)))

(defmethod test-dimensions ((lhs vector) (rhs matrix) &key)
  (etest-dimensions (length lhs) (matrix-rows rhs)))

(defmethod test-dimensions((lhs matrix) (rhs matrix)
			   &key (transpose-rhs nil) (both t))
  (with-accessors ((lhs-rows matrix-rows)
		   (lhs-cols matrix-cols)) lhs
    (let ((rhs-rows (matrix-rows rhs))
	  (rhs-cols (matrix-cols rhs)))
      (when transpose-rhs
	(rotatef rhs-rows rhs-cols))
      (and
       (or (not both)
	   (= rhs-rows lhs-rows)
	   (error 'dimension-error :dim2 rhs-rows :dim1 lhs-rows))
       (or (= rhs-cols lhs-cols)
	   (error 'dimension-error :dim2 rhs-cols :dim1 lhs-cols))))))

