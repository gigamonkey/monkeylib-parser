;;
;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.parser
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components
  ((:file "packages")
   (:file "parser" :depends-on ("packages")))
  :depends-on (:com.gigamonkeys.macro-utilities 
	       :com.gigamonkeys.utilities))

