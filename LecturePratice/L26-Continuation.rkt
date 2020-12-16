#lang plai
; KCFAE
;----------------------------------------
;[BNF]
; <KCFAE> ::= <num>
;          | {+ <KCFAE> <KCFAE>}
;          | {- <KCFAE> <KCFAE>}
;          | <id>
;          | {<id> <KCFAE>}
;          | {fun {<id>} <KCFAE>}
;          | {if0 <KCFAE><KCFAE><KCFAE>}
;          | {withcc <id><KCFAE>}


; 0-1) Type_FAE
(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [sub (lhs KCFAE?) (rhs KCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [app (ftn KCFAE?)(arg KCFAE?)]
  [withcc (id symbol?)(exp KCFAE?)]
  [if0 (test-expr KCFAE?)(then-expr KCFAE?)(else-expr KCFAE?)]
  )

; 0-2) Type_DefredSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value KCFAE-Value?)
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
    [(list 'withcc i exp) (withcc i (parse exp))]
    [(list 'if0 test-expr then-expr else-expr) (if0 (parse test-expr) (parse then-expr) (parse else-expr))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" fae)]
    )
  )
; 4) FAE values : value container
(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (c procedure?)]
  )

; 2) interpreter
(define (interp kcfae ds k)
  (type-case KCFAE kcfae
    [num (n) (k (numV n))]
    [add (l r) (interp l ds (lambda (lv)
                              (interp r ds
                                      (lambda (rv)
                                        (k (num+ lv rv))))))]
    [sub (l r) (interp l ds (lambda (lv)
                              (interp r ds
                                      (lambda (rv)
                                        (k (num- lv rv))))))]
    [id (s) (k(lookup s ds))]
    [fun (p b) (k (closureV (lambda (a-val dyn-k)
                              (interp b (aSub p a-val ds) dyn-k))))]
    [app (f a) (interp f ds (lambda (f-val)
                              (interp a ds (lambda (a-val)
                                             (type-case KCFAE-Value f-val
                                               [closureV (c) (c a-val k)]
                                               [contV (c) (c a-val)]
                                               [else (error "not an applocable value")])))))]
    [withcc (cont-var body) (interp body
                                    (aSub cont-var (contV (lambda (val)
                                                            (k val)))
                                          ds)
                                    k)]
    [if0 (test t f) (interp test ds (lambda (tv)
                                      (if (eq? (interp test ds k) (numV 0))
                                          (interp t ds k)
                                          (interp f ds k))))]
    )
  )



; 3) lookup for Deferred Substitution (cache!)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? name i) v (lookup name saved))]
    )
  )

; 5) lambda concept
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))
  )

(define num+ (num-op +))
(define num- (num-op -))



(define (run sexp ds)
  (interp (parse sexp) ds (lambda (x) x)))



(require racket/trace)
(trace interp)

(run '7 (mtSub))
(run '{withcc k {+ 1 {k 3}}} (mtSub))
