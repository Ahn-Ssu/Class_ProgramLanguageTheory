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
; 3) substitution
; 4) lambda concept
;----------------------------------------

; 0) Type_FAE
(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FAE?)]
  [app (ftn symbol?)(arg FAE?)]
  )

; 1) parser for FWAE
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

; 2) interpreter
(define (interp fae)
  (type-case FAE fae
    [num (n) fae]
    [add (l r) (num+ (interp l) (interp r))]
    [sub (l r) (num- (interp l) (interp r))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) (fae)]
    [app (f a) (local [(define ftn (interp f))]
                 (interp (subst (fun-body ftn)
                                (fun-param ftn)
                                (interp a))))]
    )
  )

; 3) substitution
(define (subst fae idtf val)
  (type-case FAE fae
    [num (n) fae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [id (name) (if (symbol=? name idtf) val fae)]
    [app (f arg) (app (subst f idtf val) (subst arg idtf val))]
    [fun (id body) (if (equal? idtf id) exp (fun id (subst body idtf val)))]
    )
  )

; 4) lambda concept
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y))))
  )
(define num+ (num-op +))
(define num- (num-op -))
