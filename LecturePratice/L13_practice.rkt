#lang plai
; L 13 , Laziness
; <LFAE> ::= <num>
;          | {+ <LFAE> <LFAE>}
;          | {- <LFAE> <LFAE>}
;          | <id>
;          | {<LFAE><LFAE>}        _function call
;          | {fun {<id>} <LFAE>}   _function definition

; -------------------------------------------------------------
(define-type LFAE
  [num (n number?)]
  [add (lhs LFAE?) (rhs LFAE?)]
  [sub (lhs LFAE?) (rhs LFAE?)]
  [with (name symbol?) (named-expr LFAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body LFAE?)]   ;_FunDef에서 fun-name field만 사라짐 = 익명함수(anonymous)
  [app (ftn LFAE?) (arg LFAE?)]; AST Keyword : about thefunction 
  )
; -------------------------------------------------------------
;(define (interp fae ds)
;  (type-case FAE fae
;    [num (n) (numV n)]
;    [add (l r) (num+ (interp l ds) (interp r ds))]
;    [sub (l r) (num- (interp l ds) (interp r ds))]
;    [id  (s) (lookup s ds)]
;    [fun (p b) (closureV p b ds)]
;    [app (f a) (local [(define f-val (interp f ds))
;                       (define a-val (interp a ds))]
;                 (interp (closureV-body f-val)
;                         a-val
;                         (closureV-ds f-val)))]
;    )
;  )
;
; -------------------------------------------------------------
(define (interp lfae ds)
  (type-case LFAE lfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id  (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local [(define ftn-val (strict (interp f ds)))
                       (define arg-val (exprV a ds))]
                 (interp (closureV-body ftn-val)
                         a-val
                         (closureV-ds ftn-val)))]
    )
  )

(define-type LFAE_value
  [numV (n number?)]
  [closureV (param symbol?)
            (body LFAE?)
            (ds DefredSub?)]
  [expV (expr LFAE?)
        (ds DefrdSub?)]
  )
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(value FAE-Value?)(ds DefredSub?)]
  )
(define (strict v)
  (type-case LFAE-Value v
    [exprV (expr ds) (strict (interp expr ds))]
    [else v])
  )
