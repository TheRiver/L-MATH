(in-package #:l-math)

(defclass vector ()
  ((data :documentation "An array holding the vector's data")))

(declaim (inline length))
(defgeneric length (vector)
  (:documentation "Returns the vector's dimension.")
  (:method ((vector vector))
    (cl:length (slot-value vector 'data)))
  (:method ((vector list))
    (cl:length vector)))

(declaim (inline norm))
(defgeneric norm (vector)
  (:documentation "The distance of the vector from zero.")
  (:method ((vector vector))
    (sqrt (reduce #'(lambda (x y)
		      (cl:+ (expt x 2) y))
		  (slot-value vector 'data) :from-end t :initial-value 0))))


(declaim (inline elt))
(defgeneric elt (vector index)
  (:documentation "Returns the element of a VECTOR at the given index.")
  (:method ((vector vector) (index integer))
    (cl:elt (slot-value vector 'data) index)))

(defsetf elt (vector index) (new-value)
  `(setf (cl:elt (slot-value ,vector 'data) ,index) ,new-value))

(declaim (inline x))
(defgeneric x (vector)
  (:documentation "Returns the x component from an appropriately sized vector.")
  (:method ((vector vector))
    (elt vector 0))
  (:method ((vector list))
    "Allows a list to be treated accessed in a limited way as a vector."
    (first vector)))

(defgeneric (setf x) (value vector)
  (:documentation "Sets the x component from an appropriately sized vector.")
  (:method ((value number) (vector vector))
    (setf (cl:elt (slot-value vector 'data) 0) value))
  (:method ((value number) (vector list))
    (setf (first vector) value)))

(declaim (inline y))
(defgeneric y (vector)
  (:documentation "Returns the y component from an appropriately sized vector.")
  (:method ((vector vector))
    (elt vector 1))
  (:method ((vector list))
    "Allows a list to be accessed, in a limited way, as a vector."
    (second vector)))

(defgeneric (setf y) (value vector)
  (:documentation "Sets the y component from an appropriately sized vector.")
  (:method ((value number) (vector vector))
    (setf (cl:elt (slot-value vector 'data) 1) value))
  (:method ((value number) (vector list))
    (setf (second vector) value)))

(declaim (inline z))
(defgeneric z (vector)
  (:documentation "Returns the z component from an appropriately sized vector.")
  (:method ((vector vector))
    (elt vector 2))
  (:method ((vector list))
    "Allows a list to be accessed, in a limited way, as a vector."
    (third vector)))

(defgeneric (setf z) (value vector)
  (:documentation "Sets the z component from an appropriately sized vector.")
  (:method ((value number) (vector vector))
    (setf (cl:elt (slot-value vector 'data) 2) value))
  (:method ((value number) (vector list))
    (setf (third vector) value)))

(declaim (inline w))
(defgeneric w (vector)
  (:documentation "Returns the w component from an appropriately sized vector.")
  (:method ((vector vector))
    (elt vector 3)))

(defgeneric (setf w) (value vector)
  (:documentation "Sets the w component from an appropriately sized vector.")
  (:method ((value number) (vector vector))
    (setf (cl:elt (slot-value vector 'data) 3) value)))

(defmethod initialise-data ((vector vector) (size integer))
  (with-slots (data) vector
    (setf data (make-array size)))
  vector)

(defmethod initialize-instance :after ((vector vector) &key size)
  (when size
    (initialise-data vector size)))

(defmethod print-object ((vector vector) stream)
  (print-unreadable-object (vector stream :type t)
    (with-slots (data) vector
      (loop for i across data
	 do
	   (format stream "~,3F " i))))
  vector)

(defmacro do-each-vector-element ((element vector &key (index-symbol nil))
				  &body body)
  "Iterates over elements in a vector."
  (let ((data-sym (gensym))
	(index-sym (if index-symbol
		       index-symbol
		       (gensym))))
    `(with-slots ((,data-sym data)) ,vector
       (dotimes (,index-sym (cl:length ,data-sym))
	 (symbol-macrolet ((,element
			    (aref ,data-sym ,index-sym)))
	   ,@body)))))
    

(defun make-vector (dim &key initial-elements)
  "Create a vector of the given dimensions."
  (let ((vec (make-instance 'vector :size dim)))
    (when initial-elements
      (with-slots (data) vec
	(loop
	   for el in initial-elements
	   for i from 0 below dim
	   do
	     (setf (aref data i) el))))
    vec))

(defun vector (&rest elements)
  "Create a vector."
  (make-vector (cl:length elements) :initial-elements elements))

(defgeneric copy-vector (vector)
  (:documentation "Returns a copy of the original vector")
  (:method ((vector vector))
    (let ((result (make-vector (length vector))))
      (with-slots ((original-data data)) vector
	(with-slots ((new-data data)) result
	  (map-into new-data #'identity original-data)))
      result)))

(defmethod copy ((vector vector))
  "Returns a copy of a VECTOR, just as COPY-VECTOR does."
  (copy-vector vector))

(defgeneric vector= (lhs rhs &key tolerance)
  (:documentation "Returns t iff the two vectors are equal to within a
  given tolerance.")
  (:method ((lhs vector) (rhs vector) &key (tolerance 0.0))
    (with-slots ((lhs-data data)) lhs
      (with-slots ((rhs-data data)) rhs
	(cond
	  ((/= (cl:length lhs-data) (cl:length rhs-data))
	   nil)
	  (t
	   (if (/= tolerance 0)
	       (every #'(lambda (x y)
			  (<= (abs (- x y)) tolerance))
		      lhs-data rhs-data)
	       (every #'= lhs-data rhs-data))))))))

;;;---------------------------------------------------------------------

(defgeneric to-vector (item &key dimension)
  (:documentation "Returns a new vector object based created from the
  function's arguments. If dimension is supplied, it should be a
  positive integer defining the dimension of the returned vector. If
  the specified dimension is shorter than the given item, extra
  elements are truncated. If it is longer, the new vector will be
  padded with zero elements."))

(defmethod to-vector ((item vector) &key dimension)
  "Returns a copy of the given vector."
  (check-type dimension (or null (and integer (satisfies plusp)))
	      "NIL, or a positive integer")
  (cond
    ((not dimension)
     (copy-vector item))
    (t
     (let ((result (make-vector dimension)))
       (loop
	  for i from 0 below (min (length item)
				  (length result))
	  do (setf (elt result i)
		   (elt item i)))
       result))))
       
     

(defmethod to-vector ((item list) &key (dimension (length item)))
  "Converts a list to a vector."
  (check-type dimension (or null (and integer (satisfies plusp)))
	      "NIL, or a positive integer")
  (make-vector dimension :initial-elements item))

;;;---------------------------------------------------------------------    


(defgeneric normalise! (vector)
  (:documentation "Destructively normalises the vector.")
  (:method ((vector vector))
    (with-slots (data) vector
      (let ((length (norm vector)))
	(unless (= length 0)
	  (map-into data #'(lambda (x)
			     (/ x length)) data))))
    vector))

(defgeneric normalise (vector)
  (:documentation "Returns a normalised version of the vector.")
  (:method ((vector vector))
    (let ((length (norm vector))
	  (result (make-vector (length vector))))
      (unless (= length 0)
	(do-each-vector-element (el result :index-symbol i)
	  (setf el (/ (elt vector i) length))))
      result)))

(defmethod negate! ((vector vector))
  "Destructively returns the additive inverse of a vector."
  (do-each-vector-element (element vector)
    (setf element (cl:- element)))
  vector)

(defmethod negate ((vector vector))
  "Non-destructively returns the additive inverse of a vector."
  (let ((ret (copy-vector vector)))
    (negate! ret)))

(defmethod negate ((vector list))
  "Non-destructively returns the additive inverse of the list, as if
it were a vector."
  (loop for i in vector collect (* -1 i)))



