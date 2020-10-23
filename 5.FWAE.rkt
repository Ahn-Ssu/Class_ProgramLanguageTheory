#lang plai
; FWAE
;----------------------------------------
;[BNF]
; <FWAE> ::= <num>
;          | {+ <FWAE> <FWAE>}
;          | {- <FWAE> <FWAE>}
;          | <id>
;          | {with {<id> <FWAE>} <FWAE>}
;          | {<id> <FWAE>}
;          | {fun {<id>} <FWAE>}
;
;                                           X) <Fundef> ::= {deffun {<id><id>} <F1WAE>}
;----------------------------------------
;[Require]
; 1) parser for FWAE
;                                           X) parser for FunDef
; 2) interpreter
; 3) substitution
; 4) lambda concept
;----------------------------------------

; 0) Type_FWAE
;(define-type FWAE
;  [num (n number?)]
;  [add (lhs FWAE?)(rhs FWAE?)]
;  [sub (lhs FWAE?)(rhs FWAE?)]
;  [with (name symbol?)(named-expr FWAE?)(body FWAE?)]
;  [id (name symbol?)]
;  [fun (param symbol?)(body FWAE?)]
;  [app (ftn FWAE?)(arg FWAE?)]
;  )
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)(rhs FWAE?)]
  [sub (lhs FWAE?)(rhs FWAE?)]
  [with (name symbol?)(named-expr FWAE?)(body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FWAE?)]
  [app (ftn FWAE?)(arg FWAE?)])



; 1) parser for FWAE
;(define (parse fwae)
;  (match fwae
;    [(? number?) (num fwae)]
;    [(list '+ l r) (add (parse l)(parse r))]
;    [(list '- l r) (sub (parse l)(parse r))]
;    [(list 'with (list i v) e) (with i (parse v)(parse e))]
;    [(? symbol?) (id fwae)]
;    [(list 'fun (list p) b) (fun p (parse b))]
;    [(list f a) (app (parse f) (parse a))]
;    [else (error 'parse "bad syntax: ~a" fwae)]
;    )
;  )
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e) (with i (parse v)(parse e))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]  ; e.g., {fun {x}{+ x 1}}
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax:~a"sexp)]))


; 3) substitution
(define (subst fwae idtf val)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (add (subst l idtf val)(subst r idtf val))]
    [sub (l r) (sub (subst l idtf val)(subst r idtf val))]
    [with (i v e) (with i (subst v idtf val)(if (symbol=? i idtf) e
                                                (subst e idtf val)))]
    [id (name)
        ;(if (symbol=? name idtf) val fwae)
        (cond [(equal? name idtf) val]
              [else fwae])
        ]
    [app (f arg) (app (subst f idtf val)
                      (subst arg idtf val))]
    [fun (id body) (if (equal? idtf id) fwae (fun id (subst body idtf val)))]
    )
  )


; 4) lambda concept
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y))))
  )
(define num+ (num-op +))
(define num- (num-op -))


; 2) interpreter
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (num+ (interp l)(interp r))]
    [sub (l r) (num- (interp l)(interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) fwae]
    [app (f a)  (local [(define ftn (interp f))]
                 (interp (subst (fun-body ftn)
                                (fun-param ftn)
                                (interp a))))]
    )
  )

(parse '((fun (x)(+ x x)) 10))
(interp (app (fun 'x (add (id 'x) (id 'x))) (num 10)))
(parse '((fun (x)(x 1))(fun (y) {+ y y})))
(interp (app (fun 'x (app (id 'x) (num 1))) (fun 'y (add (id 'y) (id 'y)))))
(parse '((+ 3 4) 5))
;(interp (app (add (num 3) (num 4)) (num 5)))


;(parse '(+ 1 2))
;(parse '(+ 1 {fun {x}{+ 1 x}}))
;(parse '({fun {x}{+ 1 x}} {fun {k}{+ 1 k}}))
;(parse '(1 2))

(parse '{with {f {fun {x} {+ x 1}}}{f 10}})
(interp (with 'f (fun 'x (add (id 'x) (num 1))) (app (id 'f) (num 10))))
(parse '{with {z {fun {x}{+ x y}}}{with {y 10} z}})
(interp (with 'z (fun 'x (add (id 'x) (id 'y))) (with 'y (num 10) (id 'z))))
(parse '{with {x 3} {with {f {fun {y}{+ x y}}}{with {x 5} {f 4}}}})
(interp (with 'x (num 3) (with 'f (fun 'y (add (id 'x) (id 'y))) (with 'x (num 5) (app (id 'f) (num 4))))))
(subst (with 'y (num 10) (id 'z)) 'z (fun 'x (add (id 'x)(id 'y))))
(parse '(with (z (fun {x}{+ x y})) {with {y 10} z}))
(interp (with 'z (fun 'x (add (id 'x) (id 'y))) (with 'y (num 10) (id 'z))))
