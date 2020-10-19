#lang plai
; FAE
;----------------------------------------
;[BNF]
; <FAE> ::= <num>
;          | {+ <FAE> <FAE>}
;          | {- <FAE> <FAE>}
;          | <id>
;          | {with {<id> <FAE>} <FAE>}
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
    [(list f a) (app f (parse a))]
    [else (error 'parse "bad syntax: ~a" fae)]
    )
  )
(parse '{with {x 5} 4})
(parse '{with {x 3} {+ x x}})
(parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}})
(parse '{with {f {fun {y} {+ 5 y}}} {+ f 10}})
(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})

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
    (num (op (num-n x) (num-n y))))
  )
(define num+ (num-op +))
(define num- (num-op -))


;(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))