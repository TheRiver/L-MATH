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

(defgeneric linear-interpolation (start end t-val)
  (:documentation "Interpolates between two points, using the
  parametric line equation. START and END are the two endpoints of a
  line. While T-VAL takes values between 0 and 1, inclusive, the
  returned points will lie between these two points. No matter the
  value of T-VAL, the returned point will always be on the line, but
  will be outside of the [start, end] line segment if T-VAL is not on
  the domain [0, 1]")
  (:method (start end (t-val real))
    (declare (type (or vector list) start end))
    (let ((direction (- end start)))
      (+ start (* direction t-val))))
  (:method ((start number) (end number) (t-val real))
    (+ start (* (- end start) t-val))))

(defun private-bilinear-interpolation (top-left bottom-left top-right bottom-right parameter)
  (linear-interpolation (linear-interpolation top-left bottom-left (y parameter))
			(linear-interpolation top-right bottom-right (y parameter))
			(x parameter)))

(defgeneric bilinear-interpolation (top-left bottom-left top-right bottom-right parameter)
  (:documentation "Performs bilinear interpolation: first this
  performs linear interpolation between the TOP-LEFT and BOTTOM-LEFT,
  as well as TOP-RIGHT and BOTTOM-RIGHT, using the y coordinate of the
  parameter. It then performs linear interpolation between these
  obtained values using the parameter's x coordinate. The four
  position vectors can be of any dimension, but must all be of the
  same dimension. The paramater must be a 2-vector. Parameter
   (0, 0) maps to the upper left corner, and (1, 1) maps to the lower
  right corner.")
  (:method ((top-left vector) (bottom-left vector) (top-right vector) (bottom-right vector) parameter)
    (test-dimensions parameter 2)
    (test-dimensions bottom-left (length top-left))
    (test-dimensions top-right (length top-left))
    (test-dimensions bottom-right (length top-left))
    (private-bilinear-interpolation top-left bottom-left top-right bottom-right parameter))
  (:method ((top-left number) (bottom-left number) (top-right number) (bottom-right number) parameter)
    (test-dimensions parameter 2)
    (private-bilinear-interpolation top-left bottom-left top-right bottom-right parameter)))
    
    
					     

(defgeneric between (start end)
  (:documentation "Calculates the vector half way between two other
  vectors. This is equivalent to (LINEAR-INTERPOLATION START END
  0.5).")
  (:method (start end)
    (declare (type (or vector list number) start end))
    (lm:/ (lm:+ start end) 2)))