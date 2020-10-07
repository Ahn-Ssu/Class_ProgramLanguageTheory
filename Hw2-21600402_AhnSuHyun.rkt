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
    [postfix (l r opType) ((bound-ids l) (bound-ids r))]
    [substitute (i v e k) (boundFinder e i) ] ;  (if(eq? i (bound-ids e)) i (checker e i))
    [else '()]
    )
  )

(define (boundFinder pwae bound-id)
  (type-case PWAE pwae
    [postfix (l r opType) (checkDuplication (boundFinder l bound-id) (boundFinder r bound-id) )]
    [substitute (i v e k) (if(symbol=? i bound-id) (list bound-id) (boundFinder e bound-id))]
    [id (s) (if (symbol=? s bound-id) (list bound-id) '())]
    [else '()]
    )
  )

(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (num 3) (op 'add)) (keyword 'with))) '())
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (substitute 'y (num 7) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x y))
;(test (bound-ids (substitute 'y (num 7) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with))) '(x y))
;(test (bound-ids (substitute 'x (num 3) (substitute 'y (id 'x) (postfix (num 3) (id 'y) (op 'sub)) (keyword 'with)) (keyword 'with))) '(x y))
;(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (id 'x) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x))
;(test (bound-ids (substitute 'x (id 'x) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (substitute 'z (num 7) (postfix (id 'z) (id 'x) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x z))
;(test (bound-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(y))
;(test (bound-ids (substitute 'x (id 'a) (substitute 'y (id 'b) (substitute 'z (id 'c) (postfix (id 'd) (postfix (id 'x) (postfix (id 'y) (id 'z) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(x y z))
;(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(a x))
;(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(x))

