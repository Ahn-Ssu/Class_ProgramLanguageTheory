#lang plai

;Define PWAE type
;<PWAE> ::= <num>                            
;	| <op>
;	| <id>
;	| <keyword>
;	| {<PWAE> <PWAE> <op>}
;    	| {{<id> <PWAE>} <PWAE> <keyword>}

(define-type PWAE
  [num (n number?)]
  [op (type (or/c 'add 'sub))]; symbol=? s '+ '-
  [id (name symbol?)]
  [keyword (keyword-name symbol?)]
  [postfix (lhs PWAE?) (rhs PWAE?) (op PWAE?)]
  [substitute (id-name symbol?) (named-expr PWAE?)(body PWAE?)]
 )

(postfix (num 10) (num 1) (op 'sub))
(postfix (num 10) (num 1) (op 'add))
(substitute 'x (num 5) (postfix (id 'x) (id 'x) (op 'add)))
;(postfix (num 10) (num 1) (op 'oppp))
