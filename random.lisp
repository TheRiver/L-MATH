(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package #:l-math)

;;; L-MATH: a library for simple linear algebra.
;;; Copyright (C) 2009-2010 Rudolph Neeser
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

;;;-----------------------------------------------------------------------------
;;; Functions for creating random data.
;;;-----------------------------------------------------------------------------

(defun normal (&key (mean 0) (sd 1) (min nil) (max nil) (state *random-state*))
  "Returns a normally distributed random variable with the given mean
and standard-deviation. If min or max is given, the random number is
no smaller than min and no larger than max."
  (flet ((min-fun (val)
	   (if (and min (< val min))
	       min
	       val))
	 (max-fun (val)
	   (if (and max (> val max))
	       max
	       val)))
    (min-fun (max-fun (+ mean
			 (* sd
			    (sqrt (* -2 (log (random 1.0 state))))
			    (cos (* 2 pi (random 1.0 state)))))))))

(defun uniform (&key (min 0.0) (max 1.0) (state *random-state*))
  "Returns a random number uniformly distributed between
minimum (inclusive) and maximum (exclusive). Like CL:RANDOM, the type
of the returned value depends on the type of min and max."
  (when (<= max min)
    (error 'l-math-error :format-control "The maximum value (~A) is less than or equal to the minimum value (~A)."
	   :format-arguments (list max min)))
  (+ (random (- max min) state) min))

(defun make-random-vector (dimension &key (noise #'normal))
  "Creates a VECTOR of the given dimension filled with random values
  obtained by calling NOISE once for each element in the VECTOR. By
  default, this uses normally distributed random numbers with a mean
  of 0 and standard deviation of 1."
  (declare (type (integer 1) dimension)
	   (type function noise))
  (let ((result (make-vector dimension)))
    (do-each-vector-element (el result)
      (setf el (coerce (funcall noise) 'double-float)))
    result))

(defun make-random-matrix (rows cols &key (noise #'normal))
  "Creates a MATRIX of the ROWS × COLS dimension filled with random
  values obtained by calling NOISE once for each element in the
  MATRIX. By default, this uses normally distributed random numbers
  with a mean of 0 and standard deviation of 1."
  (declare (type (integer 1) rows cols)
	   (type function noise))
  (let ((result (make-matrix rows cols)))
    (do-each-matrix-element (el result)
      (setf el (funcall noise)))
    result))

;;;---------------------------------------------------------------------------------------
;;; An implementation of Perlin noise
;;;---------------------------------------------------------------------------------------

(defvar *perlin-gradients* nil
  "Stores the wavelet coefficients / gradients.")
(defvar *perlin-permutations* nil
  "A list of random permutations.")

(defun initialize-noise3 (&key (lookup-size 256))
  "Initialises the perlin noise function, NOISE3. This will be called
  automatically by NOISE3 if it has not already been done so. You
  usually will not have to call this function. If you wish to change
  the random data which NOISE3 uses to calculate the perlin noise,
  then call this function. LOOKUP-SIZE determines the amount of data
  stored to calculate the perlin noise. You will probably not have to
  change this from the default value."
  (declare (type fixnum lookup-size))
  (flet ((noise ()
	   (/ (- (random 100) 50) 50)))
    (let ((gradients (make-array lookup-size
				 :element-type 'vector
				 :initial-element (vector 0 0 0)))
	  (permutations (make-array lookup-size
				    :element-type 'fixnum)))
      ;; Here we find random vectors on the unit sphere.
      (loop
	 with i = 0
	 and vector = (make-random-vector 3 :noise #'noise)
	 when (<= (norm vector) 1.0)
	 do (progn
	      (setf (cl:elt gradients i) (normalise! vector)
		    vector  (make-random-vector 3 :noise #'noise))
	      (incf i))
	 else do (setf vector (make-random-vector 3 :noise #'noise))
	 while (< i lookup-size))
      ;; Here we create the permutation
      ;; table, mapping indices to
      ;; indices.
      (loop
	 for i from 0 below lookup-size
	 do (setf (cl:elt permutations i) i))
      (loop
	 for i from 0 below lookup-size
	 do (rotatef (cl:elt permutations i) (cl:elt permutations (random lookup-size))))
      (setf *perlin-gradients* gradients
	    *perlin-permutations* permutations))
    (values)))


(defun noise3 (point)
  "Calculates Perlin noise. As input, it takes a 3-vector. It returns
  a random value between -1 and 1."
  (declare (type (or vector list) point))
  (test-dimensions point 3)
  (unless *perlin-gradients*
    (initialize-noise3))
  (multiple-value-bind (i u) (floor (x point))
    (multiple-value-bind (j v) (floor (y point))
      (multiple-value-bind (k w) (floor (z point))
	(labels ((interpolation (val)
		   ;; A polynomial used to smooth between values.
		   (declare (type real val))
		   (let ((val (abs val)))
		     (+ (- (* 6d0 (expt val 5))
			   (* 15d0 (expt val 4)))
			(* 10d0 (expt val 3)))))
		 (fold (point)
		   (declare (type list point))
		   (let ((divisor (cl:length *perlin-permutations*)))
		     (cl:elt *perlin-permutations*
			     (mod (+ (cl:elt *perlin-permutations*
					     (mod (+ (cl:elt *perlin-permutations* (mod (first point) divisor))
						     (second point))
						  divisor))
				     (third point))
				  divisor))))
		 (wavelet-coefficient (point)
		   ;; Return the wavelet coefficents, which are merely
		   ;; random choices from the points we selected on
		   ;; the unit sphere.
		   (declare (type list point))
		   (cl:elt *perlin-gradients* (fold point)))
		 (wavelet-value (offset &key (neg-x nil) (neg-y nil) (neg-z nil))
		   ;; Calculates the value of the wavelet used to
		   ;; calculate the noise.
		   (declare (type list offset))
		   (dot-product (wavelet-coefficient (list (+ i (first offset))
							   (+ j (second offset))
							   (+ k (third offset))))
				(list (if neg-x
					  (1- u)
					  u)
				      (if neg-y
					  (1- v)
					  v)
				      (if neg-z
					  (1- w)
					  w)))))
	  (let ((x-interp-param (interpolation u))
		(y-interp-param (interpolation v))
		(z-interp-param (interpolation w)))
	    ;; Finds the wavelet values at points surrounding the
	    ;; point we are interested in, and interpolates between
	    ;; them to find the value we want.
	    (linear-interpolation (linear-interpolation (linear-interpolation (wavelet-value (list 0 0 0))
									      (wavelet-value (list 1 0 0) :neg-x t)
									      x-interp-param)
							(linear-interpolation (wavelet-value (list 0 1 0) :neg-y t)
									      (wavelet-value (list 1 1 0) :neg-x t :neg-y t)
									      x-interp-param)
							y-interp-param)
				  (linear-interpolation (linear-interpolation (wavelet-value (list 0 0 1) :neg-z t)
									      (wavelet-value (list 1 0 1) :neg-x t :neg-z t)
									      x-interp-param)
							(linear-interpolation (wavelet-value (list 0 1 1) :neg-y t :neg-z t)
									      (wavelet-value (list 1 1 1) :neg-x t :neg-y t :neg-z t)
									      x-interp-param)
							y-interp-param)
				  z-interp-param)))))))



(defun draw-noise3-output (width height file-path &optional (plane-width 5))
  "Outputs perlin noise in the PPM format. WIDTH and HEIGHT are the
resolution of the image. FILE-PATH defines where the image should be
saved, and PLANE-WIDTH defines the width of the plane on which the
noise is added. The wider this width, the smaller the noise detail."
  (declare (type (integer 1) height plane-width))
  (let ((top-left (vector (- plane-width) plane-width 0))
	(top-right (vector plane-width plane-width 0))
	(bottom-left (vector (- plane-width) (- plane-width) 0))
	(bottom-right (vector plane-width (- plane-width) 0)))
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (format stream "P3~%~A ~A~%255~%"
	      width height)
      (labels ((print-point (noise)
		 (let ((noise (floor (* (/ (+ 1 noise) 2.0) 255))))
		   (format stream "~a ~a ~a "
			   noise noise noise))))
	(loop
	   for i from 0 below width
	   do (loop
		 for j from 0 below height
		 do (let ((i-val (/ i (1- width)))
			  (j-val (/ j (1- height))))
		      (print-point (noise3 (linear-interpolation (linear-interpolation top-left bottom-left j-val)
								 (linear-interpolation top-right bottom-right j-val)
								 i-val))))
		 finally (format stream "~%")))))))

