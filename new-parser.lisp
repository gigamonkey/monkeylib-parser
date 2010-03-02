;;;
;;; Copyright (c) 2003-2005, Gigamonkeys Consulting All rights reserved.
;;;
;;; Parser generator, loosely based on Henry Baker's META paper. The
;;; most obvious change is that we don't user reader macros because
;;; they're too much of a hassle.
;;;

;;;; Building results:

;;; Each grammar matches over a certain range of the input text (or tokens).
;;; Can save the raw text matched (or the start and end indices).

(in-package :com.gigamonkeys.new-parser)

;;; Expand bits of grammar into code to do the matching.

(defgeneric code (grammar parser-type &key save-p)
  (:documentation "Emit the code to match the grammar. When save-p is
  true we also generate code to save whatever was matched in last-match."))

(defmethod code ((value character) (parser-type (eql 'character)) &key save-p)
  `(and
    (< index end)
    (char= (char input index) ,value)
    ,@(if save-p `((progn (setf last-match (string ,value)) t)))
    (progn (incf index) t)))

(defmethod code ((value character) (parser-type (eql 'token)) &key save-p)
  `(and
    (< index end)
    (let ((token (aref input index)))
      (and
       (string= (value token) (string ,value))
       ,@(if save-p `((progn (setf last-match token) t)))))
    (progn (incf index) t)))

(defmethod code ((value string) (parser-type (eql 'character)) &key save-p)
  `(and
    (<= index (- end ,(length value)))
    (string= ,value input :start2 index :end2 (+ index ,(length value)))
    ,@(if save-p `((progn (setf last-match ,value) t)))
    (progn (incf index ,(length value)) t)))

(defmethod code ((value string) (parser-type (eql 'token)) &key save-p)
  `(and
    (< index end)
    (let ((token (aref input index)))
      (and
       (string= (value token) ,value)
       ,@(if save-p `((progn (setf last-match token) t)))))
    (progn (incf index) t)))

(defmethod code ((name symbol) parser-type &key save-p)
  (declare (ignore parser-type))
  (if (and save-p (not (eql save-p 'not)))
      `(and (,name :save-p t) ,@(when save-p `((progn (setf ,name last-match) t))))
      `(,name)))

(defmethod code ((grammar cons) parser-type &key save-p)
  (code-for-special (first grammar) grammar parser-type :save-p save-p))

(defmethod code-for-special ((op t) grammar parser-type &key save-p)
  "A list with no special op in the CAR is a sequence."
  `(let ((start index))
     (or
      (and
       ,@(loop for g in grammar collect (code g parser-type :save-p save-p)))
      (progn (setf index start) nil))))

(defmethod code-for-special ((op (eql '/)) grammar parser-type &key save-p)
  `(or 
    ,@(loop for g in (rest grammar)
	 collect (code g parser-type :save-p save-p))))

(defmethod code-for-special ((op (eql '&)) grammar parser-type &key save-p)
  (destructuring-bind (first &rest rest) (rest grammar)
    `(let ((start index) (end-first-match nil))
       (or (and
	    (and
	     ,(code first parser-type :save-p save-p)
	     (progn (setf end-first-match index) t))
	    ,@(mapcar
	       #'(lambda (g) 
		   `(progn
		      (let ((end-match index))
			(declare (ignorable end-match))
			(setf index start)
			,(code g parser-type :save-p save-p))))
	       rest)
	    (progn (setf index end-first-match) t))
	   (progn (setf index start) nil)))))

(defmethod code-for-special ((op (eql '?)) grammar parser-type &key save-p)
  `(or ,(code (second grammar) parser-type :save-p save-p) t))

(defmethod code-for-special ((op (eql '*)) grammar parser-type &key save-p)
  `(not (do () ((not ,(code (second grammar) parser-type :save-p save-p))))))

(defmethod code-for-special ((op (eql '+)) grammar parser-type &key save-p)
  (let ((sub-code (code (second grammar) parser-type :save-p save-p)))
    `(and ,sub-code (not (do () ((not ,sub-code)))))))

(defmethod code-for-special ((op (eql '~)) grammar parser-type &key save-p)
  (declare (ignore save-p))
  `(not ,(code (second grammar) parser-type :save-p 'not)))

(defmethod code-for-special ((op (eql '!)) grammar parser-type &key save-p)
  "Matches only if the sub-grammar fails to match or, if it does
match, if index hasn't moved up to end-match. (I.e. this can only be
used inside a conjuctive grammar."
  (declare (ignore save-p))
  `(not (and
	 ,(code (second grammar) parser-type :save-p 'not)
         (= index end-match))))

(defmethod code-for-special ((op (eql '@)) grammar parser-type &key save-p)
  (declare (ignore save-p))
  (destructuring-bind (grammar form) (rest grammar)
    (let ((vars (productions-called grammar)))
      `(let (,@vars)
	 (declare (ignorable ,@vars))
	 (and
	  ,(code grammar parser-type :save-p t)
	  (progn ,form t))))))

(defmethod code-for-special ((op (eql '^)) grammar parser-type &key save-p)
  (declare (ignore save-p))
  (destructuring-bind (grammar &optional form) (rest grammar)
    (let ((vars (productions-called grammar)))
      `(let (,@vars)
	 (declare (ignorable ,@vars))
	 (and
	  ,(code grammar parser-type :save-p t)
	  (progn `(setf result ,(or ,form 'last-match)) t))))))

(defmethod code-for-special ((op (eql '%)) grammar parser-type &key save-p)
  (declare (ignore parser-type))
  (let ((name (second grammar)))
    `(and
      (< index end)
      (let ((token (aref input index)))
	(and
	 (eql (kind token) ',name)
	 ,@(if save-p `((progn (setf last-match token ,name token) t)))))
      (progn (incf index) t))))

(defun productions-used (grammar)
  "Productions used incudes all the productions called by the grammar
  plus any productions used in the definitions of those productions."
  (let ((called (productions-called grammar)))
    (delete-duplicates
     (nconc called
	    (mapcan #'productions-used (mapcar #'production-grammar called))))))

(defun productions-called (grammar)
  "Productions called are all the productions used directly in the
grammar given."
  (when grammar
    (labels ((walk (g)
	       (if (symbolp g) (list g) (mapcan #'walk (subgrammars g)))))
      (delete-duplicates (walk grammar)))))

(defun subgrammars (grammar)
  (when (consp grammar)
    (destructuring-bind (first &rest rest) grammar
      (case first
	((* + ? ~ ! % @ ^) (list (first rest)))
	((& /) rest)
	(t grammar)))))

;; Productions are named grammars. We hang a few bits of information
;; off the symbol that names the production.

(defun production-type (name)
  (get name 'production-type))

(defun (setf production-type) (type name)
  (setf (get name 'production-type) type))
  
(defun production-grammar (name)
  (get name 'production-grammar))

(defun (setf production-grammar) (grammar name)
  (setf (get name 'production-grammar) grammar))

(defun production-variables (name)
  (get name 'production-variables))

(defun (setf production-variables) (grammar name)
  (setf (get name 'production-variables) grammar))

;; Code generation to stitch it all together.

(defun production-function-body (name parser-type)
  (case (production-type name)
    (chartype (chartype-production-function-body name))
    (grammar (grammar-production-function-body name parser-type))))

(defun chartype-production-function-body (name)
  `(,name (&key save-p)
	  (and
	   (< index end)
	   (let ((current-char (char input index)))
	     (and
	      (typep current-char ',name)
	      (progn (when save-p (setf last-match current-char)) t)))
	   (progn (incf index) t))))

(defun grammar-production-function-body (name parser-type)
  (let ((grammar (production-grammar name))
	(variables (production-variables name)))
    `(,name (&key save-p)
	    (declare (ignorable ,@variables))
	    (let (,name ,@variables (start index))
	      (symbol-macrolet ((result ,name))
		(and ,(code grammar parser-type)
		     (progn
		       (when save-p
			 (setf last-match (or ,name (subseq input start index))))
		       t)))))))

(defun parser-body (name grammar parser-type)
  "Emit the body of a parser. Used by both defparser and parselet."
  `(,name (input)
	  (let ((output (make-array 0 :adjustable t :fill-pointer t)))
	    (let ((index 0) (end (length input)) last-match)
	      (declare (ignorable last-match))
	      (labels
		  (,@(mapcar
		      #'(lambda (p) (production-function-body p parser-type))
		      (productions-used grammar)))
		(let (result)
		  (values
		   (and ,(code grammar parser-type) (= index end))
		   (if (zerop (length output)) result output))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(defmacro defchartype (name &body typespec)
  "Define a character-type production. The body should be a typespec
that defines a type whose extension is a subset of characters, e.g.
'(member #\a #\b #\c)"
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf (production-type ',name) 'chartype))
    (deftype ,name () ,@typespec)))

(defmacro defprod (name (&rest vars) &body spec)
  "Define an arbitrary production in the PARSER grammar language."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (production-type ',name) 'grammar
	   (production-grammar ',name) ',spec
	   (production-variables ',name) ',vars)))

(defmacro defparser (name grammar &key (type 'character))
  "Define a parser function in the PARSER grammar language."
  `(defun ,@(parser-body name grammar type)))

(defmacro parselet ((&rest bindings) &body forms)
  "Define local parsers. Parslet is to defparser as flet is to defun."
  `(flet (,@(mapcar
             #'(lambda (binding) 
                 (destructuring-bind (name spec &key (type 'character)) binding
                   (parser-body name spec type)))
	     bindings))
     ,@forms))

#+(or)(defmacro deflexer (name spec (&rest meta))
  "Define a parser that collects a vector of tokens."
  ((let ((parser
         (make-instance 'character-parser
                        :name name
                        :grammar (make-grammar spec))))
    (dolist (m meta)
      (when (eql (car m) :tokens) (setf (tokens parser) (cdr m))))
    `(defun ,@(parser-body parser)))))





