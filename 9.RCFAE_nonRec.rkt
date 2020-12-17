#lang plai
; RCFAE
;----------------------------------------
;[BNF]
; <RCFAE> ::= <num>
;          | {+ <RCFAE> <RCFAE>}
;          | {- <RCFAE> <RCFAE>}
;          | {* <RCFAE> <RCFAE>}
;          | <id>
;          | {with {<id> <RCFAE>} <RCFAE>} (only for concrete syntax)
;          | {<id> <RCFAE>}
;          | {fun {<id>} <RCFAE>}
;          | {if0 <RCFAE><RCFAE><RCFAE>
;          | {rec {<id><RCFAE>} <RCFAE>}
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
(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [mul (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (ftn RCFAE?)(arg RCFAE?)]
  [if0 (test-expr RCFAE?)
      (then-expr RCFAE?) (else-expr RCFAE?)]
 ; [rec (name symbol?) (named-expr RCFAE?) (fst-call RCFAE?)]
  )

; 0-2) Type_DefredSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value FAE-Value?)
        (ds DefrdSub?)]
  )

; 1) parser for FAE
(define (parse rcfae)
  (match rcfae
    [(? number?) (num rcfae)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(? symbol?) (id rcfae)]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'if0 cond-exp then-exp else-exp) (if0 (parse cond-exp) (parse then-exp) (parse else-exp))]
  ;  [(list 'rec f-name f-name-expr f-call) (rec (parse f-name) (parse f-name-expr) (parse f-call))]
    [else (error 'parse "bad syntax: ~a" rcfae)]
    )
  )

; 2) interpreter
(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [mul (l r) (num* (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))]
                 (interp (closureV-body f-val) (aSub (closureV-param f-val)
                                                     a-val
                                                     (closureV-ds f-val)))
                 )]
    [if0 (test-expr then-expr else-expr) (if (numzero? (interp test-expr ds))
                                             (interp then-expr ds)
                                             (interp else-expr ds))]
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
  [closureV (param symbol?) (body RCFAE?) (ds DefrdSub?)]
  )

; 5) lambda concept
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))
  )

(define (numzero? exp)
  (zero? (numV-n exp)))
(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))


"No more with - Syntax Sugar"
(parse '(with (x 10) x))
(parse '((fun (x) x) 10))

(parse
 '{with {fac {fun {n}
                   {if0 n
                        1
                        {* n {fac {- n 1}}}}}}
        {fac 10}})

; free identifier erro 'fac'
;(interp (parse
; '{with {fac {fun {n}
;                   {if0 n
;                        1
;                        {* n {fac {- n 1}}}}}}
;        {fac 10}}) (mtSub))




(require racket/trace)
(parse '(with (fac (facY facY))
                                                         (fun  (n) (if0 n
                                                                        1
                                                                        (* n (fac (- n 1)))))))
'(app (fun 'fac (fun 'n (if0 (id 'n) (num 1) (mul (id 'n) (app (id 'fac) (sub (id 'n) (num 1)))))))
      (app (id 'facY) (id 'facY)))
(parse '(with (fac (fun (x) ((facY facY) x)))
                                                         (fun  (n) (if0 n
                                                                        1
                                                                        (* n (fac (- n 1)))))))
'(app (fun 'fac (fun 'n (if0 (id 'n) (num 1) (mul (id 'n) (app (id 'fac) (sub (id 'n) (num 1)))))))
      (fun 'x (app (app (id 'facY) (id 'facY)) (id 'x))))
(trace interp)
;(interp (parse '{with {fac {with {facX {fun {facY} {fun {n} {if0 n 1 {* n {{facY facY}{- n 1}}}}}}}
 ;                        {facX facX}}}
  ;            {fac 1}}) (mtSub))
(interp (parse '{with (fac (with (facX (fun (facY) (with (fac (fun (x) ((facY facY) x)))
                                                         (fun  (n) (if0 n
                                                                        1
                                                                        (* n (fac (- n 1))))))))
                                 (facX facX)))
                      (fac 0)}) (mtSub))


