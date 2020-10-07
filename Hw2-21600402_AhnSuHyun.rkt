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
  [substitute (id-name symbol?) (named-expr PWAE?)(body PWAE?)(sub-keyword PWAE?)]
 )

(postfix (num 10) (num 1) (op 'sub))
(postfix (num 10) (num 1) (op 'add))
(substitute 'x (num 5) (postfix (id 'x) (id 'x) (op 'add))(keyword 'with))
;(postfix (num 10) (num 1) (op 'oppp))

;[contract] parse: sexp -> PWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list l r '+) (postfix (parse l) (parse r) (op 'add))]
    [(list l r '-) (postfix (parse l) (parse r) (op 'sub))]
    [(list (list i v) e 'with) (substitute  i (parse v) (parse e) (keyword 'with))] 
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )


(parse '{3 4 -})
(parse '{{3 4 -} 7 +})
(parse '{{x 5} {x x +} with})
(parse '{{x {5 4 +}} {x x +} with}) 
    