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

(defun make-random-vector (dimension &key (noise #'normal))
  "Creates a VECTOR of the given dimension filled with random values
  obtained by calling NOISE once for each element in the VECTOR. By
  default, this uses normally distributed random numbers with a mean
  of 0 and standard deviation of 1."
  (declare (type (integer 1) dimension)
	   (type function noise))
  (let ((result (make-vector dimension)))
    (do-each-vector-element (el result)
      (setf el (funcall noise)))
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
  