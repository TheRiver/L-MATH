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

;;;---------------------------------------------------------------------
;;; Vector operations
;;;---------------------------------------------------------------------

(defgeneric dot-product (lhs rhs)
  (:documentation "Calculates the dot product between two vectors.")
  (:method ((lhs vector) (rhs vector))
    (test-dimensions lhs rhs)
    (with-slots ((lhs-data data)) lhs
      (with-slots ((rhs-data data)) rhs
	(loop for l across lhs-data
	   for r across rhs-data
	   sum (* l r)))))
  (:method ((lhs list) (rhs vector))
    (dot-product (to-vector lhs) rhs))
  (:method ((lhs vector) (rhs list))
    (dot-product lhs (to-vector rhs)))
  (:method ((lhs list) (rhs list))
    (test-dimensions lhs rhs)
    (loop
       for l in lhs
       for r in rhs
       sum (* l r))))

(defgeneric angle-between (from to)
  (:documentation "Returns the angle between two vectors. The angle is
  in radians, and is signed. The angle represents the the angle needed
  to transform the FROM vector to the TO vector.")
  (:method ((from vector) (to vector))
    (test-dimensions from to)
    (test-nonzero from)
    (test-nonzero to)
    (let ((from (normalise from))
	  (to (normalise to)))
      (- (atan (coerce (y to) 'double-float)
	       (coerce (x to) 'double-float))
	 (atan (coerce (y from) 'double-float)
	       (coerce (x from) 'double-float))))))

(declaim (inline cross-product))
(defgeneric cross-product (lhs rhs)
  (:documentation "Calculates the cross product between 3-vectors")
  (:method ((lhs vector) (rhs vector))
    (test-dimensions lhs rhs)
    (unless (= (length lhs) 3)
      (error 'l-math-error :format-control "Can only perform cross products on 3-vectors."))
    (vector (- (* (y lhs) (z rhs))
	       (* (z lhs) (y rhs)))
	    (- (* (z lhs) (x rhs))
	       (* (x lhs) (z rhs)))
	    (- (* (x lhs) (y rhs))
	       (* (y lhs) (x rhs))))))

(defgeneric euclidean-distance (lhs rhs)
  (:documentation "Calculates the Euclidean distance between two objects."))

(defmethod euclidean-distance ((lhs vector) (rhs vector))
  "Calculates the Euclidean distance between two vectors."
  (test-dimensions lhs rhs)
  (with-slots ((lhs-data data)) lhs
    (with-slots ((rhs-data data)) rhs
      (sqrt 
       (loop
	  for i across lhs-data
	  for j across rhs-data
	  sum (expt (- i j) 2))))))

(defmethod euclidean-distance ((lhs list) (rhs list))
  "Calculates the Euclidean distance between two vectors, represented
as lists."
  (loop
     for l in lhs
     for r in rhs
     sum (expt (- l r) 2)))

(defgeneric centre-of-mass (vector &rest vectors)
  (:documentation "Calculates the centre point of a series of
  objects.")
  (:method ((vector vector) &rest vectors)
    (/ (reduce #'+ vectors :initial-value vector)
       (1+ (length vectors)))))

;;;-------------------------------------------------------------------
;;; Operations on numbers
;;;-------------------------------------------------------------------

(defmethod euclidean-distance ((lhs number) (rhs number))
  "Calculates the Euclidean distance between two numbers."
  (abs (- lhs rhs)))

;;;-------------------------------------------------------------------