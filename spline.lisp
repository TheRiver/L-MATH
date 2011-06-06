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

;;;--------------------------------------------------------------------
;;; A base class for all spline implementations
;;;--------------------------------------------------------------------

(defclass spline ()
  ()
  (:documentation "A base class for all spline and parametric curve
  implementations."))

;;;--------------------------------------------------------------------

(defgeneric evaluate (spline parameter)
  (:documentation "Evaluate the given spline at the given parameter
  value. Returns a point on that spline."))

(defgeneric spline-geometry (spline)
  (:documentation "Returns the geometry of the associated
  spline. These are the end points, tangents, and so on, of the
  various splines."))

(defgeneric set-spline-geometry (spline geometry)
  (:documentation "Set the control geometry (end points, tangents,
  etc.) of the given spline."))

(defsetf spline-geometry set-spline-geometry)

;;;--------------------------------------------------------------------

(defclass matrix-spline (spline)
  ((basis-matrix :initform (make-hermite-basis-matrix)
		 :initarg :basis-matrix
		 :accessor basis-matrix
		 :documentation "Defines the behaviour of the
		 spline. For possible bases to use, see
		 MAKE-HERMITE-BASIS-MATRIX, MAKE-BEZIER-BASIS-MATRIX,
		 and MAKE-UNIFORM-NONRATIONAL-BSPLINE-BASIS-MATRIX"))
  (:documentation "Used to represent splines that are defined by basis
  matrices."))

;;;--------------------------------------------------------------------