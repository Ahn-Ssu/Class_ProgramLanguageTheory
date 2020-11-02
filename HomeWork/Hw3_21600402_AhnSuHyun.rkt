#lang plai

;[BNF]
; <SDFAE> ::= <num>
; 	   |  <id>
; 	   |  {+ <SDFAE> <SDFAE>}
;          |  {- <SDFAE> <SDFAE>}
;          |  {with {<id> <SDFAE>} <SDFAE>}
;          |  {<SDFAE> <SDFAE>}
;          |  {s fun {<id>} <SDFAE>}
;          |  {d fun {<id>} <SDFAE>}
; <num> ::= 1, 2, 3, ....
; <id>  ::= a, b, c, ....



; the type given
(define-type SDFAE
    [num     (n number?)]
    [add     (lhs SDFAE?) (rhs SDFAE?)]
    [sub     (lhs SDFAE?) (rhs SDFAE?)]
    [id        (name symbol?)]
    [fun     (sd symbol?) (param symbol?) (body SDFAE?)]
    [app     (ftn SDFAE?) (arg SDFAE?)])

; for keeping static scope 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value (or/c SDFAE-Value? SDFAE?))
        (ds DefrdSub?)]
  )

; for binding id
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? name i) v (lookup name saved))]
    )
  )

; for result
(define-type SDFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body SDFAE?) (ds DefrdSub?)]
  )


(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))
  )
(define num+ (num-op +))
(define num- (num-op -))

(define (run sexp)
  (interp (parse sexp) (mtSub)))
;------------------------------------------------------------------------------------
; Problems 1.	Implement a Parser for SDFAE 
; Solved by myself: Y
; Time taken: 6m
; [contract] parse: sexp -> SDFAE
; [purpose] Consume the concrete code to abstract syntax, SDFAE

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun 's i (parse e)) (parse v))]
    [(list 'd 'fun (list p) b) (fun 'd p (parse b))]
    [(list 's 'fun (list p) b) (fun 's p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )

"Test case given_parse start"
(test (parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(app (fun 's 'x (app (fun 's 'f (app (fun 's 'x (app (id 'f) (num 4))) (num 5))) (fun 'd 'y (add (id 'x) (id 'y))))) (num 3)))
(test (parse '{with {x 3} {with {f {s fun {y} {+ x y}}} {with {x 5} {f 4}}}})
      (app (fun 's 'x (app (fun 's 'f (app (fun 's 'x (app (id 'f) (num 4))) (num 5))) (fun 's 'y (add (id 'x) (id 'y))))) (num 3)))
"Test case given_parse end"

;------------------------------------------------------------------------------------
; Problems 2.	Implement a Interpreter for SDFAE  
; Solved by myself: Y
; Time taken: 6m
; [contract] parse: SDFAE -> SDFAE-Value (=result Value)
; [purpose] interprete SDFAE to actual value type(SDFAE-Value)

(define (interp sdfae ds)
  (type-case SDFAE sdfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (sd p b) (if (symbol=? sd 's) (closureV p b ds) sdfae)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))]
                 (if (closureV? f-val)
                 (interp (closureV-body f-val) (aSub (closureV-param f-val)
                                                     a-val
                                                     (closureV-ds f-val)))
                 (interp (fun-body f-val) (aSub (fun-param f-val) (interp a-val ds) ds))
                 )
                 )]
    )
  )

"Test case given_interp start"
(run '{with {x 3}{+ x 4}})
(parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(SDFAE? (fun 'd 'y (add (id 'x) (id 'y))))
(run '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}})
"Test case given_interp end"
