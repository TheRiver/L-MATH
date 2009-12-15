(in-package #:l-math)

;;;-----------------------------------------------------------------------------
;;; Matrix class data and basic operations
;;;-----------------------------------------------------------------------------


(defclass matrix ()
  ((data :initarg :data
	 :type (array float)
	 :documentation "A 2D array holding the matrix data."))
  (:documentation "A mathematical matrix class."))

(defgeneric matrix-rows (matrix)
  (:documentation "Return the number of rows in a matrix.")
  (:method ((matrix matrix))
    (first (array-dimensions (slot-value matrix 'data)))))

(defgeneric matrix-cols (matrix)
  (:documentation "Returns the number of columns in a matrix.")
  (:method ((matrix matrix))
    (second (array-dimensions (slot-value matrix 'data)))))

(defmethod make-load-form ((matrix matrix) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of matrix)
		  :data ,(slot-value matrix 'data)))

;;;-----------------------------------------------------------------------------


(defmethod initialise-data ((matrix matrix) (size-list list))
  (with-slots (data) matrix
    (setf data (make-array size-list))))

(defmethod initialize-instance :after ((matrix matrix) &key (size (list 3 3)))
  (initialise-data matrix size))

(defmethod print-object ((matrix matrix) stream)
  (print-unreadable-object (matrix stream :type t)
    (format stream "~a x ~a" (matrix-rows matrix) (matrix-cols matrix))
    (loop for row from 0 below (matrix-rows matrix)
       do
	 (progn
	   (terpri stream)
	   (loop for col from 0 below (matrix-cols matrix)
	      do (format stream "~,3F " (aref (slot-value matrix 'data)
					    row col))))))
  matrix)

(defgeneric matrix-elt (matrix row col)
  (:documentation "Return the matrix componenent at the given row and
  column")
  (:method ((matrix matrix) (row integer) (col integer))
    (with-slots (data) matrix
      (aref data row col))))

(defsetf matrix-elt (matrix row col) (new-value)
  (let ((data-sym (gensym)))
    `(with-slots ((,data-sym data)) ,matrix
       (setf (aref ,data-sym ,row ,col) ,new-value))))

(defmacro do-each-matrix-element ((symbol matrix
					  &optional row-index-symbol
					  col-index-symbol)
				    &body body)
  "Iterates over each of the matrix elements, row by row."
  (let ((rows (gensym))
	(cols (gensym))
	(col-symbol (if col-index-symbol
			col-index-symbol
			(gensym)))
	(row-symbol (if row-index-symbol
			row-index-symbol
			(gensym)))
	(data (gensym)))
    `(with-accessors ((,rows matrix-rows)
		      (,cols matrix-cols)) ,matrix
       (with-slots ((,data data)) ,matrix
	 (do ((,row-symbol 0)
	      (,col-symbol 0))
	     ((or (>= ,row-symbol ,rows) (>= ,col-symbol ,cols)))
	   (symbol-macrolet ((,symbol
			      (aref ,data ,row-symbol ,col-symbol)))
	     ,@body)
	   (incf ,col-symbol)
	   (when (= ,col-symbol ,cols)
	     (setf ,col-symbol 0)
	     (incf ,row-symbol)))))))

(defmacro do-each-matrix-element-2
    ((lhs-symbol rhs-symbol lhs-matrix rhs-matrix &key (transpose-rhs t))
     &body body)
  (let ((lhs-rows (gensym))
	(lhs-cols (gensym))
	(lhs-j (gensym))
	(lhs-i (gensym))
	(lhs-data (gensym))
	(rhs-rows (gensym))
	(rhs-cols (gensym))
	(rhs-j (gensym))
	(rhs-i (gensym))
	(rhs-data (gensym)))
    `(with-accessors ((,lhs-rows matrix-rows)
		      (,lhs-cols matrix-cols)) ,lhs-matrix
       (with-slots ((,lhs-data data)) ,lhs-matrix
	 (with-accessors ((,rhs-rows matrix-rows)
			  (,rhs-cols matrix-cols)) ,rhs-matrix
	   (with-slots ((,rhs-data data)) ,rhs-matrix
	     (do ((,lhs-i 0)
		  (,lhs-j 0)
		  (,rhs-i 0)
		  (,rhs-j 0))
		 ((or (>= ,lhs-i ,lhs-rows) (>= ,lhs-j ,lhs-cols)))
	       (symbol-macrolet ((,lhs-symbol
				  (aref ,lhs-data ,lhs-i ,lhs-j))
				 (,rhs-symbol
				  (aref ,rhs-data ,rhs-i ,rhs-j)))
		 ,@body)
	       (incf ,lhs-j)
	       ,(if transpose-rhs
		    `(incf ,rhs-i)
		    `(incf ,rhs-j))
	       (when (= ,lhs-j ,lhs-cols)
		 ,(if transpose-rhs
		      `(progn
			 (setf ,rhs-i 0)
			 (incf ,rhs-j))
		      `(progn
			 (setf ,rhs-j 0)
			 (incf ,rhs-i)))
		 (setf ,lhs-j 0)
		 (incf ,lhs-i)))))))))
  
  
(defun make-matrix (rows cols &key initial-elements)
  "Creates a matrix of the given dimensions."
  (let ((matrix (make-instance 'matrix :size (list rows cols))))
    (when initial-elements
      (when (/= (cl:length initial-elements) (* rows cols))
	(error
	 'dimension-error
	 :dim1 (* rows cols)
	 :dim2 (cl:length initial-elements)))
      (with-slots (data) matrix
	(do ((i 0)
	     (j 0)
	     (initials initial-elements (rest initials)))
	    ((or (>= i rows) (>= j cols)))
	  (setf (aref data i j) (first initials))
	  (incf j)
	  (when (= j cols)
	    (setf j 0)
	    (incf i)))))
    matrix))

(defun make-identity (size)
  "Creates an size x size identity matrix."
  (let ((matrix (make-matrix size size)))
    (dotimes (i size matrix)
      (setf (matrix-elt matrix i i) 1))))

(defmethod copy ((matrix matrix))
  (let ((new-matrix (make-matrix (matrix-rows matrix)
			     (matrix-cols matrix))))
    (do-each-matrix-element-2 (lhs rhs new-matrix matrix
				   :transpose-rhs nil)
      (setf lhs rhs))
    new-matrix))

(defmethod negate! ((matrix matrix))
  "Sets the matrix to its additive inverse and returns it."
  (do-each-matrix-element (element matrix)
    (setf element (cl:- element)))
  matrix)

(defmethod negate ((matrix matrix))
  "Returns the additive inverse of a matrix."
  (negate! (copy matrix)))

(defgeneric transpose (matrix)
  (:documentation "Returns the non-destructive transpose of a matrix")
  (:method ((matrix matrix))
    (let ((transpose (make-matrix (matrix-cols matrix) (matrix-rows matrix))))
      (do-each-matrix-element-2 (lhs rhs matrix transpose)
	(setf rhs lhs))
      transpose)))

;;;-------------------------------------------------------------

  