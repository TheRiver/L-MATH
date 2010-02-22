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

(defpackage #:l-math
  (:use :common-lisp)
  (:nicknames :lm)
  (:export #:*equivalence-tolerance*
	   #:copy
	   #:equivalent
	   #:vector
	   #:make-vector
	   #:to-vector
	   #:copy-vector
	   #:vector=
	   #:matrix=
	   #:do-each-vector-element
	   #:length
	   #:dimension
	   #:norm
	   #:normalise
	   #:normalise!
	   #:dot-product
	   #:angle-between
	   #:euclidean-distance
	   #:cross-product
	   #:elt
	   #:x
	   #:y
	   #:z
	   #:w
	   #:matrix
	   #:matrix-rows
	   #:matrix-cols
	   #:make-matrix
	   #:matrix-elt
	   #:make-identity
	   #:do-each-matrix-element
	   #:do-each-matrix-element-2
	   #:test-dimensions
	   #:c+
	   #:c-
	   #:negate
	   #:transpose
	   #:negate!
	   #:c*
	   #:c/
	   #:*
	   #:/
	   #:-
	   #:+
	   #:yaw-matrix
	   #:pitch-matrix
	   #:roll-matrix
	   #:create-rotation-matrix
	   #:create-rotation-from-view
	   #:create-rotation-from-view-to-view
	   #:to-degrees
	   #:to-radians
	   #:l-math-error
	   #:dimension-error
	   #:zero-length-error
	   #:linear-interpolation
	   #:between
	   #:normal
	   #:make-random-vector)
  (:shadow #:vector
	   #:make-vector
	   #:length
	   #:elt
	   #:*
	   #:/
	   #:-
	   #:+))
