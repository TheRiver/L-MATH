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

(defmacro create-vector-operation-method (name function)
  "Expands into code that can be used to fill out one a method for one
of the generic methods defined below, such as c+"
  `(defmethod ,name ((lhs vector) (rhs vector))
     (test-dimensions lhs rhs)
     (with-slots ((lhs-data data)) lhs
       (with-slots ((rhs-data data)) rhs
	 (let ((result (make-vector (length lhs))))
	   (with-slots (data) result
	     (loop
		for x across lhs-data
		for y across rhs-data
		for i from 0 below (cl:length data)
		do (setf (aref data i) (,function x y))))
	   result)))))

(defmacro create-list-operation-method (name function)
  "Generates a method, such as c+, which operates on common lisp
lists, treating them as if they were vectors."
  `(defmethod ,name ((lhs list) (rhs list))
     (test-dimensions lhs rhs)
     (mapcar #',function lhs rhs)))
  
(defgeneric c+ (lhs rhs)
  (:documentation "Adds two objects together.")
  (:method (lhs rhs)
    (error 'operation-not-supported
	   :operation-name '+
	   :extra-information (format nil "The arguments are of type ~S and ~S."
				      (type-of lhs)
				      (type-of rhs))))
  (:method ((lhs number) (rhs number))
    (cl:+ lhs rhs))
  (:method ((lhs number) (rhs vector))
    (let ((result (make-vector (length rhs))))
      (do-each-vector-element (el rhs :index-symbol i)
	(setf (elt result i) (cl:+ el lhs)))
      result))
  (:method ((lhs vector) (rhs number))
    (let ((result (make-vector (length lhs))))
      (do-each-vector-element (el lhs :index-symbol i)
	(setf (elt result i) (cl:+ el rhs)))
      result))
  (:method ((lhs number) (rhs list))
    (loop for i in rhs collect (cl:+ lhs i)))
  (:method ((lhs list) (rhs number))
    (loop for i in lhs collect (cl:+ rhs i))))

(defgeneric c- (lhs rhs)
  (:documentation "Subtracts objects.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '-
	   :extra-information (format nil "The arguments are of type ~S and ~S."
				      (type-of lhs)
				      (type-of rhs))))
  (:method ((lhs number) (rhs number))
    (cl:- lhs rhs))
  (:method ((lhs number) (rhs vector))
    (let ((result (make-vector (length rhs))))
      (do-each-vector-element (el rhs :index-symbol i)
	(setf (elt result i) (cl:- lhs el)))
      result))
  (:method ((lhs vector) (rhs number))
    (let ((result (make-vector (length lhs))))
      (do-each-vector-element (el lhs :index-symbol i)
	(setf (elt result i) (cl:- el rhs)))
      result))
  (:method ((lhs number) (rhs list))
    (loop for i in rhs collect (cl:- lhs i)))
  (:method ((lhs list) (rhs number))
    (loop for i in lhs collect (cl:- i rhs))))

(defgeneric c* (lhs rhs)
  (:documentation "multiplies two objects together.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '*
	   :extra-information (format nil "The arguments are of type ~S and ~S."
				      (type-of lhs)
				      (type-of rhs))))
  (:method ((lhs number) (rhs number))
    (cl:* lhs rhs))
  (:method ((lhs number) (rhs vector))
    (let ((result (make-vector (length rhs))))
      (do-each-vector-element (el rhs :index-symbol i)
	(setf (elt result i) (cl:* el lhs)))
      result))
  (:method ((lhs number) (rhs list))
    (loop for i in rhs collect (cl:* lhs i)))
  (:method ((lhs vector) (rhs number))
    (let ((result (make-vector (length lhs))))
      (do-each-vector-element (el lhs :index-symbol i)
	(setf (elt result i) (cl:* el rhs)))
      result))
  (:method ((lhs list) (rhs number))
    (loop for i in lhs collect (cl:* i rhs))))

(defgeneric c/ (lhs rhs)
  (:documentation "Divides two objects.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '/
	   :extra-information (format nil "The arguments are of type ~S and ~S."
				      (type-of lhs)
				      (type-of rhs))))
  (:method ((lhs number) (rhs number))
    (cl:/ lhs rhs))
  (:method ((lhs number) (rhs vector))
    (let ((result (make-vector (length rhs))))
      (do-each-vector-element (el rhs :index-symbol i)
	(setf (elt result i) (cl:/ el lhs)))
      result))
  (:method ((lhs number) (rhs list))
    (loop for i in rhs collect (cl:/ i lhs)))
  (:method ((lhs vector) (rhs number))
    (let ((result (make-vector (length lhs))))
      (do-each-vector-element (el lhs :index-symbol i)
	(setf (elt result i) (cl:/ el rhs)))
      result))
  (:method ((lhs list) (rhs number))
    (loop for i in lhs collect (cl:/ i rhs))))

(create-vector-operation-method c+ cl:+)	; Add two vector.
(create-vector-operation-method c- cl:-)	; Subtract two vectors.
(create-vector-operation-method c* cl:*)	; multiplies two vectors.
(create-vector-operation-method c/ cl:/)	; divides two vectors.

(create-list-operation-method c+ cl:+)	; Add two lists.
(create-list-operation-method c- cl:-)	; Subtract two lists.
(create-list-operation-method c* cl:*)	; multiplies two lists.
(create-list-operation-method c/ cl:/)	; divides two lists.

(defmethod c+ (lhs (rhs list))
  "Converts lists to vectors when performing mixed type operations."
  (c+ lhs (make-vector (length rhs) :initial-elements rhs)))

(defmethod c+ ((lhs list) rhs)
  "Converts lists to vectors when performing mixed type operations."
  (c+ rhs lhs))

(defmethod c* (lhs (rhs list))
  "Converts lists to vectors when performing mixed type operations."
  (c* lhs (make-vector (length rhs) :initial-elements rhs)))

(defmethod c* ((lhs list) rhs)
  "Converts lists to vectors when performing mixed type operations."
  (c* rhs lhs))

(defmethod c- (lhs (rhs list))
  "Converts lists to vectors when performing mixed type operations."
  (c- lhs (make-vector (length rhs) :initial-elements rhs)))

(defmethod c- ((lhs list) rhs)
  "Converts lists to vectors when performing mixed type operations."
  (c- (make-vector (length lhs) :initial-elements lhs) rhs))

(defmethod c/ (lhs (rhs list))
  "Converts lists to vectors when performing mixed type operations."
  (c/ lhs (make-vector (length rhs) :initial-elements rhs)))

(defmethod c/ ((lhs list) rhs)
  "Converts lists to vectors when performing mixed type operations."
  (c/ (make-vector (length lhs) :initial-elements lhs) rhs))

(defmethod c* ((lhs matrix) (rhs vector))
  (test-dimensions lhs rhs)
  (let ((result (make-vector (matrix-rows lhs))))
    (do-each-matrix-element (el lhs i j)
      (setf (elt result i)
	    (cl:+ (elt result i)
		  (cl:* el (elt rhs j)))))
    result))

(defmethod c* ((lhs vector) (rhs matrix))
  (test-dimensions lhs rhs)
  (let ((result (make-vector (matrix-cols rhs))))
    (do-each-matrix-element (el rhs i j)
      (setf (elt result j)
	    (cl:+ (elt result j)
		  (cl:* el (elt lhs i)))))
    result))

(defmethod c* ((lhs matrix) (rhs matrix))
  (test-dimensions lhs rhs :transpose-rhs t)
  (let ((result (make-matrix (matrix-rows lhs) (matrix-cols rhs))))
    (do-each-matrix-element (el result row col)
      (dotimes (i (matrix-cols lhs))
	(setf el (cl:+ el (cl:* (matrix-elt lhs row i) (matrix-elt rhs i col))))))
    result))


  

;;;---------------------------------------------------
;;; Producing some general operations that can operate on lists,
;;; and items of different types.
;;;---------------------------------------------------

(defun + (&rest items)
  (reduce #'c+ items))
(defun - (&rest items)
  (if (= 1 (cl:length items))
      (negate (first items))
      (reduce #'c- items)))
(defun * (&rest items)
  (reduce #'c* items))
(defun / (&rest items)
  (reduce #'c/ items))
