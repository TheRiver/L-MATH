(in-package #:l-math)

;;; L-MATH: a library for simple linear algebra.
;;; Copyright (C) 2009-2011 Rudolph Neeser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Additional permission under GNU GPL version 3 section 7
;;; 
;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library. Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.
;;; 
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module. An independent module is a module which is not derived from or
;;; based on this library. If you modify this library, you may extend this
;;; exception to your version of the library, but you are not obligated to
;;; do so. If you do not wish to do so, delete this exception statement
;;; from your version.

;;;-----------------------------------------------------------------------
;;; Standard basis matrices
;;;-----------------------------------------------------------------------

(defun make-hermite-basis-matrix ()
  "Returns the 4×4 hermite basis matrix."
  (make-matrix 4 4 :initial-elements (list  2  1  1 -2
					   -3 -2 -1  3
					    0  1  0  0
					    1  0  0  0)))

(defun make-bezier-basis-matrix ()
  "Returns the 4×4 Bézier basis matrix."
  (make-matrix 4 4 :initial-elements (list -1  3 -3  1
					    3 -6  3  0
					   -3  3  0  0
					    1  0  0  0)))

(defun make-uniform-nonrational-bspline-basis-matrix ()
  "Returns a 4×4 matrix for the uniform, nonrational b-splines."
  (make-matrix 4 4 :initial-elements (list -1/6  3/6 -3/6 1/6
					    3/6 -6/6  3/6 0
					   -3/6  0    3/6 0
					    1/6  4/6  1/6 0)))

(defun make-catmull-rom-basis-matrix ()
  "Returns the 4×4 matrix for catmull-rom interpolating splines."
  (make-matrix 4 4 :initial-elements (list -1/2  3/2 -3/2  1/2
					    2/2 -5/2  4/2 -1/2
					   -1/2  0    1/2  0
					    0    2/2  0    0)))

;;;-----------------------------------------------------------------------
;;; Classes for b-spline basis functions

(defclass b-spline-knots ()
  ((knots :initform nil
	  :initarg :knots
	  :type (or null (simple-array double-float))
	  :accessor knots
	  :documentation "An ascending collection of knots, stored with
	  repetition.")
   (multiplicity :initform nil
		 :initarg :multiplicity
		 :type (or null (simple-array fixnum))
		 :accessor multiplicity
		 :documentation "The multiplicity of the above knots.")
   (knot-count :initform 0
	       :type fixnum
	       :reader knot-count
	       :documentation "The number of knots, taking in to account multiplicity.")
   (no-multiplicity :initform nil
		    :reader no-multiplicity-p
		    :documentation "Indicates whether there is a
		    multiplicity or not in the knots.")
   (uniform :initform nil
	    :reader uniform-p
	    :documentation "A boolean indicating whether this is a
	    uniform sequence of knots or not."))
  (:documentation "Some docs. You will likely find it more convenient
  to construct instance of this class using MAKE-KNOTS. Notice that
  B-SPLINE-BASIS requires a phantom knot on either end of the knot
  sequence. MAKE-KNOTS will automatically add those knots for you."))

(defgeneric calculate-knot-count (knot-data)
  (:documentation "Returns the number of knots in the data, taking in
  to account multiplicity. Sets KNOT-COUNT accordingly.")
  (:method ((knot-data b-spline-knots))
    (setf (slot-value knot-data 'knot-count)
	  (with-accessors ((multiplicity multiplicity)) knot-data
	    (loop
	       for m across multiplicity
	       sum m)))))

(defgeneric calculate-uniformity (knot-data)
  (:documentation "Calculates whether the knot-data is uniform or
  not. Sets UNIFORM-P accordingly.")
  (:method ((knot-data b-spline-knots))
    (with-accessors ((knots knots)
		     (multiplicity multiplicity)) knot-data
      (setf (slot-value knot-data 'no-multiplicity)
	    (every #'(lambda (item)
		       (= item 1))
		   multiplicity))
      (setf (slot-value knot-data 'uniform) 
	    (and (no-multiplicity-p knot-data)
		 (let ((distance (- (aref knots 1) (aref knots 0))))
		   (loop
		      for i from 0 below (length knots)
		      for j from 1 below (length knots)
		      always (equivalent distance (- (aref knots j)
						     (aref knots i))))))))))

(defmethod initialize-instance :after ((knot-data b-spline-knots) &key)
  (with-accessors ((knots knots)
		   (multiplicity multiplicity)) knot-data
    (unless (= (length knots)
	       (length multiplicity))
      (error 'l-math-error
	     :format-control "There are a different number of knots (~A) to specified multiplicity (~A)."
	     :format-arguments (list (length knots) (length multiplicity))))
    (calculate-knot-count knot-data)
    (calculate-uniformity knot-data)))

(defmethod (setf knots) :after (item (knot-data b-spline-knots))
  (calculate-uniformity knot-data)
  (calculate-knot-count knot-data))

(defmethod (setf multiplicity) :after (item (knot-data b-spline-knots))
  (calculate-uniformity knot-data)
  (calculate-knot-count knot-data))

(defun make-knots (knots &key
		   (multiplicity (loop
				    repeat (length knots)
				    collect 1))
		   (add-phantoms t))
  "Creates a knot sequence. Notice that this will add some phantom
knots to the beginning and end of the sequence, unless ADD-PHANTOMS is
false. If MULTIPLICITY, a list of multiplicity values, is not given,
each knot is assumed to be a simple (non-repeated) knot."
  (declare (type list knots multiplicity))
  (let* ((distance (- (second knots) (first knots)))
	 (knots (if add-phantoms
		    (append (cons (- (first knots) distance) knots)
			    (list (+ (first (last knots)) distance)))
		    knots))
	 (multiplicity (if add-phantoms
			   (append (cons 1 multiplicity)
				   (list 1))
			   multiplicity)))
    (make-instance 'b-spline-knots
		   :knots  (make-array (length knots) :element-type 'double-float
				       :initial-contents (mapcar #'(lambda (knot)
								     (coerce knot 'double-float))
								 knots))
		   :multiplicity (make-array (length multiplicity) :element-type 'fixnum
					     :initial-contents (mapcar #'(lambda (multi)
									   (coerce multi 'fixnum))
								       multiplicity)))))

(defgeneric all-knots (knots)
  (:documentation "Given a knots data structure, this will return a
  list of all knots, including multiplicities.")
  (:method ((knots-data b-spline-knots))
    (with-accessors ((knots knots)
		     (multiplicity multiplicity)) knots-data
      (let ((count (length knots)))
	(labels ((handle-index (index count accum)
		   (cond
		     ((< count (aref multiplicity index))
		      (handle-index index (1+ count) (cons (aref knots index) accum)))
		     (t
		      accum)))
		 (determine-knots (index accum)
		   (cond
		     ((< index count)
		      (determine-knots (1+ index) (append (handle-index index 0 nil)
							  accum)))
		     (t
		      accum))))
	  (nreverse (determine-knots 0 nil)))))))

(defmethod print-object ((knots b-spline-knots) stream)
  (print-unreadable-object (knots stream :identity t :type t)
    (format stream "~{~A~^, ~}" (all-knots knots))))

(defgeneric low-parameter (knot-data degree)
  (:documentation "Given a knot sequence and the degree of the spline,
  this returns the lowest possible parameter value the spline will
  accept.")
  (:method ((knots b-spline-knots) (degree integer))
    (get-ith-knot knots degree)))

(defgeneric high-parameter (knot-data degree)
  (:documentation "Given a knot sequence and the degree of the spline,
  this returns the highest possible parameter value the spline will
  aceept.")
  (:method ((knots b-spline-knots) (degree integer))
    (get-ith-knot knots (- (knot-count knots) (1+ degree)))))

(defun number-needed-knots (num-points degree)
  "Given the number of points making up a spline, and the degree of
  the spline, this will tell us the number of required knots."
  (+ 1 degree num-points))
  

(defgeneric get-ith-knot (knot-data i &optional offset)
  (:documentation "Returns the ith knot, taking in to account
  multiplicity. OFFSET should be positive number that array indices
  are offset by.")
  (:method ((knot-data b-spline-knots) (i integer) &optional (offset 0))
    (with-accessors ((knots knots)
		     (multiplicity multiplicity)) knot-data
      (when (>= i (knot-count knot-data))
	(error 'l-math-error :format-control "The index is larger than the number of knots."))
      (cond
	((no-multiplicity-p knot-data)
	 ;; Can speed this up in the uniform case.
	 (aref knots (+ i offset)))
	(t
	 (aref knots (loop
			for index from 0 below (length multiplicity)
			for m across multiplicity
			sum m into count
			while (<= count (+ offset i))
			finally (return index))))))))
	   
(defun find-starting-knot-index (knot-data degree parameter)
  "Given knot data, the degree of a spline, and a parameter, this
  locates the first index of the knots which will be used to define
  the point on the given spline. Returns first the b-spline-basis
  family to be used, and then the offset index of the knot."
  (check-type knot-data b-spline-knots)
  (with-accessors ((knots knots)
		   (multiplicity multiplicity)) knot-data
    (let ((index (loop
		    for knot across knots
		    for mult across multiplicity
		    for index = 0 then (+ index mult)
		    while (<= knot parameter)
		    finally (return (- index degree)))))
      (values (- index degree)
	      index))))

(defun b-spline-basis (knot-data degree family parameter &key (offset degree) fast-spline-index)
  "Ask for the value of a given basis function. This is based on
Farin's 'Curves and Surfaces for Computer Aided Geometric Design. DEGREE is the degree of the curve.

This has some pre-calculated basis functions for uniform knot
secquences. You can access these by specifying an index using
FAST-SPLINE-INDEX. This is an integer between 0 and DEGREE, and
specifies which of the basis functions should be called."
  (declare (type b-spline-knots knot-data)
	   (type fixnum degree)
	   (type number parameter)
	   (type (or null fixnum) fast-spline-index))
  (when (minusp degree)
    (error 'l-math-error :format-control "The degree of a b-spline may not be negative."))
  (let ((current (get-ith-knot knot-data family offset))
	(before (get-ith-knot knot-data (1- family) offset))
	(nth-after (get-ith-knot knot-data (+ family degree) offset))
	(nth-after-1 (get-ith-knot knot-data (+ family (1- degree)) offset)))
    (cond
      ((zerop degree)
       (if (and (<= before parameter)
		(< parameter current))
	   1
	   0))
      ((and fast-spline-index
	    (uniform-p knot-data)
	    (= degree 2)
	    (equivalent (abs (- current before)) 1))
       ;; Some code to speed up the quadratic case. First, we need
       ;; to shift the parameter to begin at 0.
       (let ((u (abs (second (multiple-value-list (truncate parameter))))))
      	 (ecase fast-spline-index
      	   (2 (* 1/2 (expt u 2)))
      	   (1 (+ 1/2 u (- (expt u 2))))
      	   (0 (+ 1/2 (- u) (* 1/2 (expt u 2)))))))
      ((and fast-spline-index
	    (uniform-p knot-data)
      	    (= degree 3)
	    (equivalent (abs (- current before)) 1))
       ;; Some code to speed up the quadratic case. First, we need
       ;; to shift the parameter to begin at 0.
       (let ((u (abs (second (multiple-value-list (truncate parameter))))))
      	 (ecase fast-spline-index
      	   (3 (* 1/6 (expt u 3)))
      	   (2 (+ 1/6 (* 1/2 u) (* 1/2 (expt u 2)) (* -1/2 (expt u 3))))
      	   (1 (+ 4/6 (- (expt u 2)) (* 1/2 (expt u 3))))
      	   (0 (+ 1/6 (* -1/2 u) (* 1/2 (expt u 2)) (* -1/6 (expt u 3)))))))
      (t
       (+ (if (equivalent (- nth-after-1 before) 0)
	      0
	      (* (/ (- parameter before)
		    (- nth-after-1 before))
		 (b-spline-basis knot-data (1- degree) family parameter :offset offset)))
	  (if (equivalent (- nth-after current) 0)
	      0
	      (* (/ (- nth-after parameter)
		    (- nth-after current))
		 (b-spline-basis knot-data (1- degree) (1+ family) parameter :offset offset))))))))

(defun chord-length-parameterisation (all-points degree)
  "Produces a non-parameteric parameterisation for b-splines."
  (cond
    ((= (length all-points) (1+ degree))
     (list -3 -1 0 1 2 3))
    (t
     (let* ((domain-points (rest (butlast all-points)))
	    (result (loop
		       for count from 0 below (- (length domain-points) 2)
		       for i in domain-points
		       for j in (rest domain-points)
		       sum (lm:euclidean-distance i j) into sum
		       collecting sum into collection
		       finally (return collection)))
	    (last (+ (first (last result))
		     (euclidean-distance (first (last domain-points))
					 (nth (- (length domain-points) 2) domain-points))))
	    (result (append result (list last))))
       (mapcar #'(lambda (item)
		   (/ item last))
	       (append (loop
			  for i from (1- degree) above 0
			  with distance = (- (second result)
					     (first result))
			  collecting (- (* distance i)))
		       (cons 0 result)
		       (loop
			  for i from 1 below degree
			  with distance = (- (first (last result))
					     (first (last (butlast result))))
			  collecting (+ last (* distance i)))))))))

;;;-----------------------------------------------------------------------

