(in-package #:l-math)

(declaim (inline to-radians))
(defun to-radians (degrees)
  (cl:* degrees (cl:/ pi 180)))

(declaim (inline to-degrees))
(defun to-degrees (radians)
  (cl:* radians (cl:/ 180 pi)))

(defun roll-matrix (size angle)
  "Creates a matrix that rotates around the z-axis by the given angle,
in radians. This is a left-handed rotation."
  (check-type size (integer 3 4) "an integer of value 3 or 4")
  (let ((matrix (make-identity size)))
    (setf (matrix-elt matrix 0 0) (cos angle)
	  (matrix-elt matrix 0 1) (cl:- (sin angle))
	  (matrix-elt matrix 1 0) (sin angle)
	  (matrix-elt matrix 1 1) (cos angle))
    matrix))

(defun yaw-matrix (size angle)
  "Creates a matrix that rotates around the y-axis by the given angle,
given in radians. This is a left-handed rotation"
  (check-type size (integer 3 4) "an integer of value 3 or 4")
  (let ((matrix (make-identity size)))
    (setf (matrix-elt matrix 0 0) (cos angle)
	  (matrix-elt matrix 0 2) (sin angle)
	  (matrix-elt matrix 2 0) (cl:- (sin angle))
	  (matrix-elt matrix 2 2) (cos angle))
    matrix))

(defun pitch-matrix (size angle)
  "Creates a matrix that rotates around the x-axis by the given angle,
given in radians. This is a left-handed rotation."
  (check-type size (integer 3 4) "an integer of value 3 or 4")
  (let ((matrix (make-identity size)))
    (setf (matrix-elt matrix 1 1) (cos angle)
	  (matrix-elt matrix 1 2) (cl:- (sin angle))
	  (matrix-elt matrix 2 1) (sin angle)
	  (matrix-elt matrix 2 2) (cos angle))
    matrix))


(defmacro fill-row (matrix item-name row-num)
  "A macro useful for filling matrices in functions such as
  CREATE-ROTATION-MATRIX."
  `(do-each-vector-element (value (normalise ,item-name) :index-symbol i)
     (setf (matrix-elt ,matrix ,row-num i) value)))


(defun create-rotation-matrix (view right up &optional (size 3))
  "Creates a rotation matrix from three vectors. VIEW is the direction
  that the object should be pointing along, UP is the direction
  upwards. RIGHT is the vector orthogonal to this."
  (check-type view vector)
  (check-type right vector)
  (check-type up vector)
  (check-type size (integer 3 4))
  (let ((matrix (make-identity size)))
    (fill-row matrix right 0)
    (fill-row matrix up 1)
    (fill-row matrix view 2)
    matrix))

(defun create-rotation-from-view (view world-up &optional (size (length view)))
  "Given a direction to look in (VIEW), and the direction that is
  'upwards' in a given coordinate system, this function creates a
  rotation matrix to translate into that coordinate system. This
  matrix should be post multiplied by any vectors. The matrix is
  defined using a left-handed coordinate system."
  (check-type view vector)
  (check-type world-up vector)
  (check-type size (integer 3 4))
  (let* ((matrix (make-identity size))
	 (view (normalise view))
	 (world-up (normalise world-up))
	 (up (normalise (- world-up (* (dot-product world-up view) view)))))
    (fill-row matrix view 2)
    (fill-row matrix up 1)
    (fill-row matrix (cross-product up view) 0)
    (transpose matrix)))

(defun create-rotation-from-view-to-view (from-view to-view world-up)
  "Creates a rotation matrix that will rotate the vector FROM-VIEW on
to the vector TO-VIEW, using WORLD-UP as the coordinate system's
'upward' direction. This matrix should be post-multiplied by any
vectors."
  (* (create-rotation-from-view to-view world-up)
     (transpose (create-rotation-from-view from-view world-up))))
    


