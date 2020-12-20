#lang plai
; L 15-16 , Laziness
; <RCFAE> ::= <num>
;          | {+ <RCFAE> <RCFAE>}
;          | {- <RCFAE> <RCFAE>}
;          | (* <RCFAE> <RCFAE>}
;          | <id>
;          | {fun {<id>} <RCFAE>}   _function definition
;          | {<RCFAE><RCFAE>}        _function call
;          | {if0 <RCFAE><RCFAE><RCFAE>}
;          | {rec {<id><LFAE>}<LFAE>}

; -------------------------------------------------------------
(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?)(rhs RCFAE?)]
  [sub (lhs RCFAE?)(rhs RCFAE?)]
  [mul (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?)(arg-expr RCFAE?)]
  [if0 (test-expr RCFAE?)
       (then-expr RCFAE?)(else-expr RCFAE?)]
  [rec (name symbol?) (named-expr RCFAE?)(fst-call RCFAE?)]
  )
; -------------------------------------------------------------
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
    [(list 'rec f-name f-name-expr f-call) (rec (parse f-name) (parse f-name-expr) (parse f-call))]
    [else (error 'parse "bad syntax: ~a" rcfae)]
    )
  )
; interp : RCFAE DefrdSub -> RCFAE-Value
(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [mul (l r) (num* (interp l ds) (interp r ds))]
    [id (name)(lookup name ds)]
    [fun (param body-expr)(closureV param body-expr ds)]
    [app (f a) (local [(define ftn (interp f ds))]
                 (interp (closureV-body ftn)
                         (aSub (closureV-param ftn)
                               (interp a ds)
                               (closureV-ds ftn))))]
    [if0 (test-expr then-expr else-expr) (if (numzero? (interp test-expr ds))
                                             (interp then-expr ds)
                                             (interp else-expr ds))]
    [rec (bound-id named-expr fst-call) (local [(define value-holder (box (numV 198)))
                                                (define new-ds (aRecSub bound-id value-holder ds))]
                                          (begin
                                            (set-box! value-holder (interp named-expr new-ds))
                                            (interp fst-call new-ds)))]
    )
  )

;numzero? : RCFAE-Value -> boolean
(define (numzero? n)
  (zero? (numV-n n))
  )

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value RCFAE-Value?)
        (ds DefrdSub?)]
  [aRecSub (name symbol?)
           (value-box (box/c RCFAE-Value?))
           (ds DefrdSub?)]
  )

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)(body RCFAE?)(ds DefrdSub?)]
  )
    
;lookup : symbol DefredSub -> RCFAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name val rest-ds) (if (symbol=? sub-name name)
                                     val
                                     (lookup name rest-ds))]
    [aRecSub (sub-name val-box rest-ds) (if (symbol=? sub-name name)
                                            (unbox val-box)
                                            (lookup name rest-ds))]
    )
  )

(define (num-op op)
  (lambda(x y)
    (num (op (num-n x) (num-n y)))
    )
  )
(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))
          


