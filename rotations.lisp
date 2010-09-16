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

(defun roll-by (vector angle)
  "Returns the given vector after it has undergone a roll by the given
  angle, specified in radians. This is a left-handed rotation."
  (check-type vector (or list vector))
  (check-type angle real)
  (* (roll-matrix (length vector) angle) vector))


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

(defun yaw-by (vector angle)
  "Returns the given vector after it has undergone a yaw by the given
  angle, specified in radians. This is a left-handed rotation."
  (check-type vector (or list vector))
  (check-type angle real)
  (* (yaw-matrix (length vector) angle) vector))

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

(defun pitch-by (vector angle)
  "Returns the given vector after it has undergone a pitch by the given
  angle, specified in radians. This is a left-handed rotation."
  (check-type vector (or list vector))
  (check-type angle real)
  (* (pitch-matrix (length vector) angle) vector))


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
    


