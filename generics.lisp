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

;;;-----------------------------------------------------------------------

(defvar *equivalence-tolerance* 0.00010d0
  "When objects are closer than this distance, they are considered equivalent.")

;;;-----------------------------------------------------------------------

(defgeneric initialise-data (item size)
  (:documentation "Initialises the DATA slot to the given
  size. Returns the vector object."))

(defgeneric negate (item)
  (:documentation "Returns an item with all of its elements
  negated. This is non-destructive.")
  (:method ((item number))
    "Returns the additive inverse of all basic Common Lisp
numbers. This is just a shortcut for using the cl:- function."
    (cl:- item)))

(defgeneric negate! (item)
  (:documentation "Returns the additive inverse of an item, much like
  NEGATE, only this version is destructive."))

(defgeneric copy (item)
  (:documentation "Returns a copy of the given item.")
  (:method ((item list))
    "Returns a copy of a list."
    (copy-seq item)))

(declaim (inline dimension))
(defgeneric dimension (object)
  (:documentation "Returns the dimensions of a given object."))

(defgeneric equivalent (lhs rhs)
  (:documentation "Returns t iff the two objects are numerically the
  same. Real valued objects are always compared using
  *equivalence-tolerance*.")
  (:method (lhs rhs)
    "A default implementation that ensures that unrelated objects are
    not considered equivalent."
    (declare (ignore lhs rhs))
    nil)
  (:method ((lhs number) (rhs number))
    "Returns t iff two numbers are equivalent (using =)."
    (= lhs rhs))
  (:method ((lhs real) (rhs real))
    "Two real values are compared for equivalence to a pre-defined
tolerance."
    (< (abs (- lhs rhs)) *equivalence-tolerance*)))

(defgeneric zerop (item)
  (:documentation "Returns T iff the given item is zero, to the
  tolerance used by EQUIVALENT"))

(defgeneric to-list (item)
  (:documentation "Returns a representation of the given object as a list.")
  (:method ((item list))
    "The list representation of a list is just the list itself."
    item))

(defgeneric to-homogenous (item)
  (:documentation "Takes an item and lifts it into homogenous space by
  adding one dimension on to it."))