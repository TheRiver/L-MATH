(in-package #:l-math)

;;; L-MATH: a library for simple linear algebra.
;;; Copyright (C) 2009-2012 Rudolph Neeser
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

(defgeneric spline-interpolation (points &key degree parametrisation close)
  (:documentation "Returns a b-spline that interpolates the given
  points. This is based on the discussion in sections 9.1 and 7.8 of
  Farin's Curves and Surfaces for Computer Aided Geometric
  Design. PARAMETRISATION should be one of :uniform, :centripetal,
  or :chord-length. If CLOSE is true, then this will produce a closed
  b-spline.")
  (:method ((points list) &key
	    (degree (dimension (first points)))
	    (parametrisation :centripetal)
	    close)
    (when (or (null points)
	      (null (second points)))
      (error 'l-math-error :format-control "There must be two or more points in order to interpolate."))
    (let* ((points (if (and close	; Here we want to make sure that the points form a closed shape.
			   (not (equivalent (first (last points))
					    (first points))))
		      (append points (list (first points)))
		      points)))
      (labels ((make-first-point (points)
		 (cond
		   (close
		    (nth (- (length points) 2) points))
		   (t
		    (- (* 2 (first points))
		       (second points)))))
	       (make-last-point (points)
		 (cond
		   (close
		    (second points))
		   (t
		    (+ (- (nth (- (length points) 2) points))
		       (* 2 (first (last points)))))))
	       (converge (spline)
		 (let ((differences (mapcar #'(lambda (par point)
						;; DIFFERENCES is the difference between the wanted
						;; points and those the spline passes through.
						(- point (evaluate spline par)))
					    (domain-knots spline)
					    points)))
		   (cond
		     ((some #'(lambda (value)
				(not (equivalent 0 (norm value))))
			    differences)
		      ;; So we know we still need to move things slightly.
		      (let ((points (loop
				       for point in (rest (butlast (spline-geometry spline)))
				       for difference in differences
				       for i from 0
				       collect (+ point difference))))
			(converge (make-instance 'b-spline
						 :degree degree
						 :knots (b-spline-knots spline)
						 :points (append (cons (make-first-point points)
								       points)
								 (list (make-last-point points)))))))
		     (t
		      spline)))))
	(converge (make-instance 'b-spline
				 :uniform (eq parametrisation :uniform)
				 :centripetal (eq parametrisation :centripetal)
				 :chord-length (eq parametrisation :chord-length)
				 :degree degree
				 :points (append (cons (make-first-point points) points)
						 (list (make-last-point points)))))))))
