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

;;;--------------------------------------------------------------------------
;;; Various utility functions, such as factorials and so on.
;;;--------------------------------------------------------------------------

(defun factorial (n)
  "Calculates the factorial of the integer, n. The factorial is the
  product of the sequence of integers 1, 2, 3, ..., n. n must not be a
  negative number. The factorial of n = 0 is defined to be 1."
  (declare (type integer n))
  (when (minusp n)
    (error 'l-math-error :format-control "Cannot calculate the factorial of a negative number."))
  (labels ((fact-accum (n accum)
	     (declare (type integer n accum))
	     (cond
	       ((<= n 1)
		accum)
	       (t
		(fact-accum (1- n) (* n accum))))))
    (fact-accum n 1)))

;;;--------------------------------------------------------------------------

(defun binomial-coefficient (n i)
  "Calculates the binomial coefficients. n and i must be non-negative."
  (declare (type integer n i))
  (when (or (minusp n)
	    (minusp i))
    (error 'l-math-error :format-control "The binomial coefficients only exist for non-negative n and i."))
  (cond
    ((cl:zerop i)
     1)
    ((cl:zerop n)
     0)
    ((> i n)
     0)
    (t 
     (/ (factorial n)
	(* (factorial i) (factorial (- n i)))))))

;;;--------------------------------------------------------------------------

(defun create-bernstein-polynomial (n i)
  "Returns a bernstein polynomial for n and i. This returned function
  takes one parameter."
  (declare (type integer n i))
  (when (or (minusp n)
	    (minusp i))
    (error 'l-math-error :format-control "n and i cannot be negative."))
  (cond
    ((and (zerop n)
	  (zerop i))
     (format t "Both zero~%")
     #'(lambda (t-val)
	 (declare (ignore t-val))
	 1))
    ((or (minusp i)
	 (> i n))
     (format t "out of range.~%")
     #'(lambda (t-val)
	 (declare (ignore t-val))
	 0))
    (t
     (let ((coefficient (binomial-coefficient n i)))
       #'(lambda (t-val)
	   (* coefficient
	      (expt t-val i)
	      (expt (- 1 t-val) (- n i))))))))

(defun evaluate-bernstein-polynomial (n i t-val)
  "Evaluates the bernstein polygnomial for n and i at t-val. If this
polynomial will be used multiple times, it might be more efficient to
create it using CREATE-BERNSTEIN-POLYNOMIAL."
  (funcall (create-bernstein-polynomial n i) t-val))


;;;--------------------------------------------------------------------------


