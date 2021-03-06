;;; L-MATH: a library for simple linear algebra.
;;; Copyright (C) 2009-2011 Rudolph Neeser
;;; Copyright (C) 2012 L-MATH (See AUTHORS file)
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
	   #:zerop
	   #:to-list
	   #:vector
	   #:make-vector
	   #:to-vector
	   #:to-homogenous
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
	   #:centre-of-mass
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
	   #:make-diagonal
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
	   #:rotation-x
	   #:rotation-y
	   #:rotation-z
	   #:pitch-matrix
	   #:roll-matrix
	   #:yaw-by
	   #:pitch-by
	   #:roll-by
	   #:*rotation-naming-conventions*
	   #:set-rotation-naming-convention
	   #:create-rotation-matrix
	   #:create-rotation-from-view
	   #:create-rotation-from-view-to-view
	   #:create-uniform-scale-matrix
	   #:create-scale-matrix
	   #:create-translation-matrix
	   #:to-degrees
	   #:to-radians
	   #:l-math-error
	   #:dimension-error
	   #:zero-length-error
	   #:linear-interpolation
	   #:bilinear-interpolation
	   #:spline-interpolation
	   #:between
	   #:normal
	   #:uniform
	   #:noise3
	   #:draw-noise3-output
	   #:initialize-noise3
	   #:noise3
	   #:make-random-vector
	   ; Splines
	   #:spline
	   #:basis-matrix
	   #:coefficient-matrix
	   #:evaluate
	   #:maximum-parameter
	   #:minimum-parameter
	   #:spline-geometry
	   #:set-spline-geometry
	   #:hermite-curve
	   #:cubic-bezier-curve
	   #:bezier-curve
	   #:unrbs-spline
	   #:catmull-rom-spline
	   #:b-spline
	   #:b-spline-knots
	   #:b-spline-points
	   #:all-knots
	   #:domain-knots
	   #:b-spline-degree
	   ; Transformation
	   #:transform
	   #:summation-transformer
	   ; Utilities
	   #:factorial
	   #:binomial-coefficient
	   #:create-bernstein-polynomial
	   #:evaluate-bernstein-polynomial
	   #:b-spline-basis
	   #:b-spline-knots
	   #:knots
	   #:make-knots
	   #:multiplicity
	   )
  (:shadow #:vector
	   #:zerop
	   #:make-vector
	   #:length
	   #:elt
	   #:*
	   #:/
	   #:-
	   #:+))
