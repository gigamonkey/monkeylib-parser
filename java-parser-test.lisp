(defpackage :java-parser-test
  (:use :common-lisp :test :parser))

(in-package :java-parser-test)

(defvar *cr* (string #\return))
(defvar *lf* (string #\newline))
(defvar *crlf* (concatenate 'string *cr* *lf*))
(defvar *space* (string #\space))
(defvar *tab* (string #\tab))
(defvar *form-feed* (string #\page))

(deftest compilation-unit ()
  (defparser compilation-unit-parser java-parser::compilation-unit)
  (check
   (compilation-unit-parser "")
   (compilation-unit-parser ";;;;;")
   (compilation-unit-parser "package com.javamonkey.foo;;;;;")
   (compilation-unit-parser "import com.javamonkey.Foo;")
   (compilation-unit-parser "import com.javamonkey.*;")
   ))

(deftest 3.4-line-terminators ()
  (defparser line-terminator-parser  java-parser::line-terminator)
  (check
   (line-terminator-parser *cr*)
   (line-terminator-parser *lf*)
   (line-terminator-parser *crlf*)
   (not (line-terminator-parser (concatenate 'string *lf* *cr*)))))


(deftest 3.6-white-space ()
  (defparser white-space-parser java-parser::white-space)
  (check
   (white-space-parser *space*)
   (white-space-parser *tab*)
   (white-space-parser *form-feed*)
   (white-space-parser *cr*)
   (white-space-parser *lf*)
   (white-space-parser *crlf*)))

(deftest 3.7-comments ()) ;; nyi

(deftest 3.8-identifiers ()
  (defparser identifier-parser java-parser::identifier)
  (check
   (identifier-parser "a")
   (identifier-parser "ab")
   (identifier-parser "a1")
   (identifier-parser "foo")
   (not (identifier-parser "1a"))
   (not (identifier-parser "abstract"))
   (not (identifier-parser "boolean"))
   (not (identifier-parser "break"))
   (not (identifier-parser "byte"))
   (not (identifier-parser "case"))
   (not (identifier-parser "catch"))
   (not (identifier-parser "char"))
   (not (identifier-parser "class"))
   (not (identifier-parser "const"))
   (not (identifier-parser "continue"))
   (not (identifier-parser "default"))
   (not (identifier-parser "do"))
   (not (identifier-parser "double"))
   (not (identifier-parser "else"))
   (not (identifier-parser "extends"))
   (not (identifier-parser "final"))
   (not (identifier-parser "finally"))
   (not (identifier-parser "float"))
   (not (identifier-parser "for"))
   (not (identifier-parser "goto"))
   (not (identifier-parser "if"))
   (not (identifier-parser "implements"))
   (not (identifier-parser "import"))
   (not (identifier-parser "instanceof"))
   (not (identifier-parser "int"))
   (not (identifier-parser "interface"))
   (not (identifier-parser "long"))
   (not (identifier-parser "native"))
   (not (identifier-parser "new"))
   (not (identifier-parser "package"))
   (not (identifier-parser "private"))
   (not (identifier-parser "protected"))
   (not (identifier-parser "public"))
   (not (identifier-parser "return"))
   (not (identifier-parser "short"))
   (not (identifier-parser "static"))
   (not (identifier-parser "strictfp"))
   (not (identifier-parser "super"))
   (not (identifier-parser "switch"))
   (not (identifier-parser "synchronized"))
   (not (identifier-parser "this"))
   (not (identifier-parser "throw"))
   (not (identifier-parser "throws"))
   (not (identifier-parser "transient"))
   (not (identifier-parser "try"))
   (not (identifier-parser "void"))
   (not (identifier-parser "volatile"))
   (not (identifier-parser "while"))
   (not (identifier-parser "true"))
   (not (identifier-parser "false"))
   (not (identifier-parser "null"))))

(deftest 3.9-keywords ()
  (defparser java-keyword-parser java-parser::java-keyword)
  (check
   (java-keyword-parser "abstract")
   (java-keyword-parser "boolean")
   (java-keyword-parser "break")
   (java-keyword-parser "byte")
   (java-keyword-parser "case")
   (java-keyword-parser "catch")
   (java-keyword-parser "char")
   (java-keyword-parser "class")
   (java-keyword-parser "const")
   (java-keyword-parser "continue")
   (java-keyword-parser "default")
   (java-keyword-parser "do")
   (java-keyword-parser "double")
   (java-keyword-parser "else")
   (java-keyword-parser "extends")
   (java-keyword-parser "final")
   (java-keyword-parser "finally")
   (java-keyword-parser "float")
   (java-keyword-parser "for")
   (java-keyword-parser "goto")
   (java-keyword-parser "if")
   (java-keyword-parser "implements")
   (java-keyword-parser "import")
   (java-keyword-parser "instanceof")
   (java-keyword-parser "int")
   (java-keyword-parser "interface")
   (java-keyword-parser "long")
   (java-keyword-parser "native")
   (java-keyword-parser "new")
   (java-keyword-parser "package")
   (java-keyword-parser "private")
   (java-keyword-parser "protected")
   (java-keyword-parser "public")
   (java-keyword-parser "return")
   (java-keyword-parser "short")
   (java-keyword-parser "static")
   (java-keyword-parser "strictfp")
   (java-keyword-parser "super")
   (java-keyword-parser "switch")
   (java-keyword-parser "synchronized")
   (java-keyword-parser "this")
   (java-keyword-parser "throw")
   (java-keyword-parser "throws")
   (java-keyword-parser "transient")
   (java-keyword-parser "try")
   (java-keyword-parser "void")
   (java-keyword-parser "volatile")
   (java-keyword-parser "while")
   (not (java-keyword-parser "foo"))))



(deftest 3.10.1-integer-literals ()
  (defparser integer-literal-parser java-parser::integer-literal)
  (defparser decimal-integer-literal-parser java-parser::decimal-integer-literal)
  (defparser hex-integer-literal-parser java-parser::hex-integer-literal)
  (defparser octal-integer-literal-parser java-parser::octal-integer-literal)
  (check
   (decimal-integer-literal-parser "0")
   (integer-literal-parser "0")
   (decimal-integer-literal-parser "2")
   (integer-literal-parser "2")
   (octal-integer-literal-parser "0372")
   (integer-literal-parser "0372")
   (hex-integer-literal-parser "0xDadaCafe")
   (integer-literal-parser "0xDadaCafe")
   (decimal-integer-literal-parser "1996")
   (integer-literal-parser "1996")
   (hex-integer-literal-parser "0x00FF00FF")
   (integer-literal-parser "0x00FF00FF")
   ))

(deftest 3.10.2-floating-point-literals ()) ;; nyi

(deftest 3.10.3-boolean-literals ()
  (defparser boolean-literal-parser java-parser::boolean-literal)
  (check
   (boolean-literal-parser "true")
   (boolean-literal-parser "false")))

(deftest 3.10.4-character-literals ()
  (defparser character-literal-parser java-parser::character-literal)
  (check
   (character-literal-parser "'a'")
   (character-literal-parser "'%'")
   (character-literal-parser "'\\t'")
   (character-literal-parser "'\\\\'")
   (character-literal-parser "'\\''")
   (character-literal-parser "'\\1'")
   (character-literal-parser "'\\12'")
   (character-literal-parser "'\\123'")
   (character-literal-parser "'\\177'")))

(deftest 3.10.5-string-literals ()
  (defparser string-literal-parser java-parser::string-literal)
  (check
   (string-literal-parser "\"\"")
   (string-literal-parser "\"\\\"\"")
   (string-literal-parser "\"This is a string\"")))

(deftest 3.10.6-escape-sequences ()
  (defparser escape-sequence-parser java-parser::escape-sequence)
  (check
   (escape-sequence-parser "\\b")
   (escape-sequence-parser "\\t")
   (escape-sequence-parser "\\n")
   (escape-sequence-parser "\\f")
   (escape-sequence-parser "\\r")
   (escape-sequence-parser "\\\"")
   (escape-sequence-parser "\\'")
   (escape-sequence-parser "\\\\")
   (escape-sequence-parser "\\1")
   (escape-sequence-parser "\\4")
   (escape-sequence-parser "\\12")
   (escape-sequence-parser "\\42")
   (escape-sequence-parser "\\123")
   (not (escape-sequence-parser "\\423"))))

(deftest 3.10.7-the-null-literal ()
  (defparser null-literal-parser java-parser::null-literal)
  (check
   (null-literal-parser "null")
   (not (null-literal-parser "NULL"))))

(deftest 4.2-primitive-types ()
  (defparser primitive-type-parser java-parser::primitive-type)
  (check
   (primitive-type-parser "boolean")
   (primitive-type-parser "byte")
   (primitive-type-parser "short")
   (primitive-type-parser "int")
   (primitive-type-parser "long")
   (primitive-type-parser "char")
   (primitive-type-parser "float")
   (primitive-type-parser "double")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defun tests (&optional reload)
  (if reload
      (utils::load-and-test "java-parser")
      (test::test-package "JAVA-PARSER-TEST")))
