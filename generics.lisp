(in-package #:l-math)

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
  (:documentation "Returns a copy of the given item."))