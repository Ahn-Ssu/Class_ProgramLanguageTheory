#lang plai
; F1WAE
;----------------------------------------
;[BNF]
; <F1WAE> ::= <num>
;          | {+ <F1WAE> <F1WAE>}
;          | {- <F1WAE> <F1WAE>}
;          | <id>
;          | {with {<id> <F1WAE>} <F1WAE>}
;          | {<id> <F1WAE>}
;
; <Fundef> ::= {deffun {<id><id>} <F1WAE>}
;----------------------------------------
;[Require]
; 1-1) parser for F1WAE
; 1-2) parser for FunDef
; 2) interpreter
; 3) substitution
; 4) lookup for function (function finder)
;----------------------------------------

; 0-1) Type_F1WAE
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [id (name symbol?)]
  [with (name symbol?)(name-val F1WAE?)(body F1WAE?)]
  [app (ftn symbol?)(arg F1WAE?)]
  )

; 0-2) Tpye_FunDef
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)]
  )

; 1-1) parser for F1WAE
(define (parse f1wae)
  (match f1wae
    [(? number?) (num f1wae)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(? symbol?) (id f1wae)]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(list f a) (app f (parse a))]
    [else (error 'parse "bad syntax: ~a" f1wae)]
    )
  )

; 1-2) parser for FunDef
(define (parse-fd fundefs)
  (match fundefs
    [(list 'deffun (list f x) b) (fundef f x (parse b))]
    )
  )

; 2) interpreter
(define (interp f1wae fundefs)
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs) (interp r fundefs))]
    [sub (l r) (- (interp l fundefs) (interp r fundefs))]
    [id (s) (error 'interp "free identifier")]
    [with (i v e) (interp (subst e i (interp v fundefs)) fundefs)]
    [app (f a) (local [(define a-fundef (lookup-fundefs f fundefs))]
                 (interp (subst (fundef-body a-fundef)
                               (fundef-arg-name a-fundef)
                               (interp a fundefs))
                        fundefs))]
    )
  )

; 3) substitution
(define (subst f1wae idtf val)
  (type-case F1WAE f1wae
    [num (n) f1wae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [id (s) (if (symbol=? s idtf) (num val) f1wae)]
    [with (i v e) (with i (subst v idtf val)
                        (if (symbol=? i idtf) e (subst e idtf val)))]
    [app (f a) (app f (subst a idtf val))]
    )
  )

; 4) lookup for function (function finder)
(define (lookup-fundefs name fundefs)
  (cond
    [(empty? fundefs) (error 'lookup-fundef "unknown function")]
    [else (if (symbol=? name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundefs name (rest fundefs)))]
    )
  )
