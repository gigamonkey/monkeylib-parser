(in-package :com.gigamonkeys.parser.dot-parser)

#|

graph      :       [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'

stmt_list  :       [ stmt [ ';' ] [ stmt_list ] ]

stmt       :       node_stmt
           |       edge_stmt
           |       attr_stmt
           |       ID '=' ID
           |       subgraph

attr_stmt  :       (graph | node | edge) attr_list

attr_list  :       '[' [ a_list ] ']' [ attr_list ]

a_list     :       ID [ '=' ID ] [ ',' ] [ a_list ]

edge_stmt  :       (node_id | subgraph) edgeRHS [ attr_list ]

edgeRHS    :       edgeop (node_id | subgraph) [ edgeRHS ]

node_stmt  :       node_id [ attr_list ]

node_id    :       ID [ port ]

port       :       ':' ID [ ' :' compass_pt ]
           |       ':' compass_pt

subgraph   :       [ subgraph [ ID ] ] '{' stmt_list '}'
           |       subgraph ID

compass_pt :       (n | ne | e | se | s | sw | w | nw)

An ID is one of the following:
# Any string of alphabetic characters, underscores or digits, not beginning with a digit;
# a number [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
# any double-quoted string ("...") possibly containing escaped quotes (\");
# an HTML string (<...>). 


|#


(defchartype digit '(satisfies digit-char-p))

(defchartype alphabetic '(satisfies alpha-char-p))

(defchartype id-start-char
  '(or alphabetic (eql #\_)))

(defchartype id-char
  '(or id-start-char digit))

(defchartype string-char '(not (member #\")))

(defprod number ()
  ((? "-") (/ ("." (+ digit)) ((+ digit) (? ("." (* digit)))))))

(defprod ws ()
  (* (/ #\Space #\Tab #\Newline)))

(defprod mws ()
  (+ (/ #\Space #\Tab #\Newline)))

(defprod string ()
  ("\"" (* (/ (& "\\" (! "\\\"")) "\\\"" string-char)) "\""))

(defprod id ()
  ;; skipped html string
  (& (/ (id-start-char (* id-char)) number string)
     (! (/ "strict" "graph" "digraph" "subgraph" "node" "edge"))))

;; graph : [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
(defprod graph ()
  ((? "strict" mws)  (/ "graph" "digraph") (? mws id) ws "{" ws stmt-list "}"))

;; stmt_list : [ stmt [ ';' ] [ stmt_list ] ]
(defprod stmt-list ()
  (? (stmt (? ";") (? (ws stmt-list)))))

;; stmt       :       node_stmt
;;            |       edge_stmt
;;            |       attr_stmt
;;            |       ID '=' ID
;;            |       subgraph
(defprod stmt ()
  (ws (/ (id ws "=" ws id) edge-stmt attr-stmt node-stmt subgraph)))

;; attr_stmt  :       (graph | node | edge) attr_list
(defprod attr-stmt ()
  ((/ "graph" "node" "edge") attr-list))

;; attr_list  :       '[' [ a_list ] ']' [ attr_list ]
(defprod attr-list ()
  (ws "[" (? a-list) "]" (? attr-list)))

;; a_list : ID [ '=' ID ] [ ',' ] [ a_list ]
(defprod a-list ()
  (ws id (? ws "=" ws id) (? ws "," ws) (? a-list)))

;; edge_stmt  :       (node_id | subgraph) edgeRHS [ attr_list ]
(defprod edge-stmt ()
  ((/ node-id subgraph) edge-rhs (? attr-list)))

;; edgeRHS    :       edgeop (node_id | subgraph) [ edgeRHS ]
(defprod edge-rhs ()
  (edgeop (/ node-id subgraph) (? edge-rhs)))

;; node_stmt  :       node_id [ attr_list ]
(defprod node-stmt ()
  (node-id  (? attr-list)))

;; node_id    :       ID [ port ]
(defprod node-id ()
  (id (? port)))

;; port       :       ':' ID [ ' :' compass_pt ]
;;            |       ':' compass_pt
(defprod port ()
  (":" (/ (id (? (" :" compass-pt))) compass-pt)))

;; subgraph   :       [ subgraph [ ID ] ] '{' stmt_list '}'
;;            |       subgraph ID
(defprod subgraph ()
  (ws (/ ((? "subgraph" (? mws id)) ws "{" ws stmt-list "}")
	 ("subgraph" mws id))))


;; compass_pt :       (n | ne | e | se | s | sw | w | nw)
(defprod compass-pt ()
  (/ "ne" "se" "sw" "nw" "n" "s" "e" "w"))

(defprod edgeop ()
  (ws (/ "--" "->") ws))

(defparser dot (^ graph))

(defmacro tdp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
	(foo x))) ,input))

