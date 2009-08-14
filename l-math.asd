;;; -*- Mode: Lisp; -*-

(defpackage #:l-math-asdf
  (:use :common-lisp :asdf))

(in-package #:l-math-asdf)

(defsystem :l-math
    :description "A simple math library focused on linear algebra."
    :version "0.1.1"
    :author "Rudy Neeser <rudy.neeser@gmail.com>"
    :license "GPLv3"
    :components ((:file "package")
		 (:file "conditions")
		 (:file "generics")
		 (:file "vector")
		 (:file "matrix")
		 (:file "tests")
		 (:file "vector-operations")
		 (:file "operations")
		 (:file "rotations")
		 (:file "interpolate"))
    :serial t)