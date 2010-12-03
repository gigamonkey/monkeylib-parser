(in-package :cl-user)

(defpackage :com.gigamonkeys.parser
  (:use :cl)
  (:shadow :type)
  (:export
   :defprod
   :defchartype
   :deflexer
   :defparser
   :last-match
   :parselet
   :value
   :kind
   :?
   :*
   :+
   :~
   :!
   :/
   :&
   :@
   :^
   :%))

(defpackage :com.gigamonkeys.new-parser
  (:use :cl)
  (:shadow :type)
  (:export
   :defprod
   :defchartype
   :deflexer
   :defparser
   :last-match
   :parselet
   :?
   :*
   :+
   :~
   :!
   :/
   :&
   :@
   :^
   :%))

(defpackage :com.gigamonkeys.math-parser
  (:use :common-lisp :com.gigamonkeys.parser)
  (:export :arithmetic :calculator))

(defpackage :com.gigamonkeys.java-lexer
  (:use :cl :com.gigamonkeys.parser))


(defpackage :com.gigamonkeys.css
  (:use :cl :com.gigamonkeys.parser))

(defpackage :com.gigamonkeys.parser.dot-parser
  (:use :cl :com.gigamonkeys.parser
	:com.gigamonkeys.utilities)
  (:shadowing-import-from :com.gigamonkeys.parser :!)
  (:shadow :string))

(defpackage :com.gigamonkeys.parser.time-period-parser
  (:use :cl :com.gigamonkeys.parser
	:com.gigamonkeys.utilities)
  (:shadowing-import-from :com.gigamonkeys.parser :!)
  (:shadow :step :time))
