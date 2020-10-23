#lang plai
; FAE
;----------------------------------------
;[BNF]
; <FAE> ::= <num>
;          | {+ <FAE> <FAE>}
;          | {- <FAE> <FAE>}
;          | <id>
;          | {with {<id> <FAE>} <FAE>} (only for concrete syntax)
;          | {<id> <FAE>}
;          | {fun {<id>} <FAE>}
;
;----------------------------------------
;[Require]
; 1) parser for FAE
; 2) interpreter
; 3) lookup for Deferred Substitution (cache!)
; 4) FAE values : value container
; 5) lambda concept
;----------------------------------------

; 0-1) Type_FAE
(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (ftn FAE?)(arg FAE?)]
  )

; 0-2) Type_DefredSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value FAE-Value?)
        (ds DefrdSub?)]
  )

; 1) parser for FAE
(define (parse fae)
  (match fae
    [(? number?) (num fae)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(? symbol?) (id fae)]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" fae)]
    )
  )
(parse '{with {x 5} 4})
(parse '{with {x 3} {+ x x}})
(parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}})
(parse '{f 10})
(parse '{with {f {fun {y} {+ 5 y}}} {f 10}})
(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(test (parse '{+ 1 {fun {x} {+ x y}}}) '())
(test (parse '(1 2)) '())

; 2) interpreter
(define (interp fae ds)
  (type-case FAE fae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))]
                 (interp (closureV-body f-val) (aSub (closureV-param f-val)
                                                     a-val
                                                     (closureV-ds f-val)))
                 )]
    )
  )
(app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3))


; 3) lookup for Deferred Substitution (cache!)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? name i) v (lookup name saved))]
    )
  )

; 4) FAE values : value container
(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)]
  )

; 5) lambda concept
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))
  )

(define num+ (num-op +))
(define num- (num-op -))

(num-n (num 3))
(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))
(parse '{with {x 3} {with {f {fun {y}{+ x y}}} {with {x 5} {f 4}}}})

(parse '{with {x 3}{with {f {fun {y}{+ x y}}} {with {x 5} {f 4}}}})
(app (fun 'x(app (fun 'f(app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3))
(parse '(fun {x}{f 4}))
(parse '({fun {x}{f 4}}5))
(parse '((fun {x} ((fun {f}({fun {x}{f 4}} 5))(fun {y}{+ x y}))) 3))

(parse '{with {y 10}{fun {x}{+y x}}})
(interp (parse '{with {y 10}{fun {x}{+ y x}}}) (mtSub))
(parse '{{fun {x}{x 7}}{fun {x}{+ x 13}}})
(interp (app (fun 'x (app (id 'x) (num 7))) (fun 'x (add (id 'x) (num 13)))) (mtSub))
(parse '{{fun {x}{+ x 13}}{fun {x}{x 7}}})
;(interp (app (fun 'x (add (id 'x) (num 13))) (fun 'x (app (id 'x) (num 7))))(mtSub))

(parse '{with {y 10}{fun {x}{+ y x}}}) ; (app (fun 'y (fun 'x (add (id 'y) (id 'x)))) (num 10))
(interp (app (fun 'y (fun 'x (add (id 'y) (id 'x)))) (num 10)) (mtSub))
(parse '{with {y 10}{{fun {x}{+ y x}}20}})
(interp (app (fun 'y (app (fun 'x (add (id 'y) (id 'x))) (num 20))) (num 10)) (mtSub))
'__otherExample1__
(parse '{with {x 3}{fun {x}{+ x 1}}})
(interp (app (fun 'x (fun 'x (add (id 'x) (id 'y)))) (num 3)) (mtSub))
(parse '{with {x 3}({fun {x}{+ x 1}} x)})
(interp (app (fun 'x (app (fun 'x (add (id 'x) (num 1))) (id 'x))) (num 3)) (mtSub))
'__otherExample2__
(parse '{with {x 3}{{fun {x}{x 2}}{fun {k}{+ k 77}}}})
(interp (app (fun 'x (app (fun 'x (app (id 'x) (num 2))) (fun 'k (add (id 'k) (num 77))))) (num 3)) (mtSub))
'__otherExample3__
(parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}})
(interp (app (fun 'z (app (fun 'y (id 'z)) (num 10))) (fun 'x (add (id 'x) (id 'y)))) (mtSub))
(parse '{{fun {f}{f 1}}{fun {x}{+ x 1}}})

(interp (parse '{{fun {x}{+ {+ x x}{+ x x}}}{- {+ 4 5}{+ 8 9}}})(mtSub))