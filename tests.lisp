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

(declaim (inline test-dimensions))

(defmacro etest-dimensions (lhs rhs)
  (let ((lhs-val-sym (gensym))
	(rhs-val-sym (gensym)))
    `(let ((,lhs-val-sym ,lhs)
	   (,rhs-val-sym ,rhs))
       (unless (= ,lhs-val-sym ,rhs-val-sym)
	 (error 'dimension-error :dim1 ,lhs-val-sym :dim2 ,rhs-val-sym)))))

(defmacro etest-needed-vector-dimension (vector dimension)
  (let ((vector-val-sym (gensym))
	(dimension-val-sym (gensym)))
    `(let ((,vector-val-sym (length ,vector))
	   (,dimension-val-sym ,dimension))
       (unless (= ,vector-val-sym ,dimension-val-sym)
	 (error 'required-dimension-error :dim1 ,vector-val-sym :dim2 ,dimension-val-sym)))))

(defgeneric test-dimensions (lhs rhs &key)
  (:documentation "Throws an error if the two objects do not agree in
  dimensions. If the second object is a matrix, it is possible to
  transpose the dimensions that are tested using the :transpose-rhs
  argument."))

(defmethod test-dimensions ((lhs vector) (rhs vector) &key)
  (etest-dimensions (length lhs) (length rhs)))

(defmethod test-dimensions ((vector vector) (dimension number) &key)
  "Ensure that the given vector has the specified dimension."
  (etest-needed-vector-dimension vector dimension))

(defmethod test-dimensions ((lhs list) (rhs list) &key)
  (etest-dimensions (cl:length lhs) (cl:length rhs)))

(defmethod test-dimensions ((vector list) (dimension number) &key)
  "Ensure that the given vector has the specified dimension."
  (etest-needed-vector-dimension vector dimension))

(defmethod test-dimensions ((lhs matrix) (rhs vector) &key)
  (etest-dimensions (matrix-cols lhs) (length rhs)))

(defmethod test-dimensions ((lhs vector) (rhs matrix) &key)
  (etest-dimensions (length lhs) (matrix-rows rhs)))

(defmethod test-dimensions ((lhs matrix) (rhs list) &key)
  (etest-dimensions (matrix-cols lhs) (cl:length rhs)))

(defmethod test-dimensions ((lhs list) (rhs matrix) &key)
  (etest-dimensions (cl:length lhs) (matrix-rows rhs)))

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


(defgeneric test-nonzero (vector &optional threshold)
  (:documentation "Throws a ZERO-LENGTH-ERROR if the given vector has
  a length close to zero.")
  (:method (vector &optional (threshold *equivalence-tolerance*))
    (declare (type (or list vector) vector)
	     (type (and number (satisfies plusp)) threshold))
    (when (< (abs (norm vector)) threshold)
      (error 'zero-norm-error))
    (values)))
