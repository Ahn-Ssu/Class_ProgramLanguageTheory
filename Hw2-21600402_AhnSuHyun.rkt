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
;[purpose] to convert s-expression into PWAE
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



(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b))
  )
(define (checkDuplication target source)
  (cond
    [(empty? target) source]
    [(empty? source) target]
    [(not (index-of source (first target))) (append target source)]
    [else source]
    )
  )
;[contract] parse: PWAE -> list-of-sym
;[purpose] to find free identifiers in a PWAE

;[contract] parse: PWAE -> list-of-sym
;[purpose] to find binding-ids identifiers in a PWAE

(define (binding-ids pwae)
  (type-case PWAE pwae
    [substitute (i v e k) (sort (checkDuplication (binding-ids v) (checkDuplication (list i) (binding-ids e))) symbol<?)]
    [else '()]
    )
  )

;(test (binding-ids (postfix (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add))) '())
;(test (binding-ids (substitute 'y (num 3) (substitute 'x (id 'x) (id 'y) (keyword 'with)) (keyword 'with))) '(x y))
;(test (binding-ids (substitute 'y (num 3) (substitute 'y (id 'x) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with))) '(y))
;(test (binding-ids (substitute 'y (num 3) (substitute 'y (substitute 'x (postfix (num 3) (id 'y) (op 'sub)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with))) '(x y))
;(test (binding-ids  (substitute 'y (substitute 'x (postfix (num 3) (id 'y) (op 'sub)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with))) '(x y))
;(test (binding-ids (substitute 'z (num 3) (substitute 'w (substitute 'z (postfix (num 3) (id 'y) (op 'add)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (substitute 'w (id 'y) (postfix (num 7) (id 'w) (op 'add)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(w z))

;[contract] parse: PWAE -> list-of-sym
;[purpose] to find bound-ids identifiers in a PWAE
(define (bound-ids pwae)
  (type-case PWAE pwae
    [substitute (i v e k) ]
    [id (s) 