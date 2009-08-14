(in-package #:l-math)

(defmacro create-vector-operation-method (name function)
  "Expands into code that can be used to fill out one a method for one
of the generic methods defined below, such as c+"
  `(defmethod ,name ((lhs vector) (rhs vector))
     (test-dimensions lhs rhs)
     (with-slots ((lhs-data data)) lhs
       (with-slots ((rhs-data data)) rhs
	 (let ((result (make-vector (length lhs))))
	   (with-slots (data) result
	     (map-into data #',function lhs-data rhs-data))
	   result)))))
  
(defgeneric c+ (lhs rhs)
  (:documentation "Adds two objects together.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '+))
  (:method ((lhs number) (rhs number))
    (cl:+ lhs rhs)))

(defgeneric c- (lhs rhs)
  (:documentation "Subtracts objects.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '-))
  (:method ((lhs number) (rhs number))
    (cl:- lhs rhs)))

(defgeneric c* (lhs rhs)
  (:documentation "multiplies two objects together.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '*))
  (:method ((lhs number) (rhs number))
    (cl:* lhs rhs))
  (:method ((lhs number) (rhs vector))
    (let ((result (make-vector (length rhs))))
      (do-each-vector-element (el rhs :index-symbol i)
	(setf (elt result i) (cl:* el lhs)))
      result))
  (:method ((lhs vector) (rhs number))
    (let ((result (make-vector (length lhs))))
      (do-each-vector-element (el lhs :index-symbol i)
	(setf (elt result i) (cl:* el rhs)))
      result)))

(defgeneric c/ (lhs rhs)
  (:documentation "Divides two objects.")
  (:method (lhs rhs)
    (error 'operation-not-supported :operation-name '/))
  (:method ((lhs number) (rhs number))
    (cl:/ lhs rhs))
  (:method ((lhs number) (rhs vector))
    (let ((result (make-vector (length rhs))))
      (do-each-vector-element (el rhs :index-symbol i)
	(setf (elt result i) (cl:/ el lhs)))
      result))
  (:method ((lhs vector) (rhs number))
    (let ((result (make-vector (length lhs))))
      (do-each-vector-element (el lhs :index-symbol i)
	(setf (elt result i) (cl:/ el rhs)))
      result)))

(create-vector-operation-method c+ cl:+)	; Add two vector.
(create-vector-operation-method c- cl:-)	; Subtract two vectors.
(create-vector-operation-method c* cl:*)	; multiplies two vectors.
(create-vector-operation-method c/ cl:/)	; divides two vectors.


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
  (test-dimensions lhs rhs)
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
