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

(define-condition l-math-error (error)
  ((format-control :initarg :format-control
		   :initform "A general mathematical error has occured."
		   :reader l-math-error-format-control)
   (format-arguments :initarg :format-arguments
		     :initform nil
		     :reader l-math-error-format-arguments))
  (:report (lambda (condition stream)
	     (apply #'format
		    stream
		    (l-math-error-format-control condition)
		    (l-math-error-format-arguments condition)))))

(define-condition dimension-error (l-math-error)
  ((dim1 :initarg :dim1
	 :initform nil
	 :reader dimension-error-dim1)
   (dim2 :initarg :dim2
	 :initform nil
	 :reader dimension-error-dim2))
  (:report (lambda (condition stream)
	     (apply #'format stream
		    "The dimensions of the mathematical objects do not agree: ~a vs ~a."
		    (list
		     (dimension-error-dim1 condition)
		     (dimension-error-dim2 condition))))))

(define-condition required-dimension-error (dimension-error)
  ((dim1 :initarg :dim1
	 :initform nil
	 :reader dimension-error-dim1)
   (dim2 :initarg :dim2
	 :initform nil
	 :reader dimension-error-dim2))
  (:report (lambda (condition stream)
	     (apply #'format stream
		    "The mathematical object has dimension ~a; an object with dimension ~a is required."
		    (list
		     (dimension-error-dim1 condition)
		     (dimension-error-dim2 condition))))))

(define-condition zero-norm-error (l-math-error)
  ((format-control :initform "The given vector is of zero length.")))

(define-condition operation-not-supported (l-math-error)
  ((operation-name :initarg :operation-name
		   :initform "<generic operation>"
		   :reader operation-not-suported-operation-name)
   (extra-information :initarg :extra-information
		      :initform nil
		      :reader operation-not-supported-extra-information))
  (:report (lambda (condition stream)
	     (apply #'format stream
		    "Attempted to perform an unsupported operation using operator ~s. ~@[~&~A~]"
		    (list (operation-not-suported-operation-name condition)
			  (operation-not-supported-extra-information condition))))))

(define-condition spline-geometry-error (l-math-error)
  ((format-control :initform "The geometry data for the spline is malformed.")))
   