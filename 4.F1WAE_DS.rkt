#lang plai

; F1WAE + DS
;----------------------------------------
;[BNF]
; <F1WAE> ::= <num>
;          | {+ <F1WAE> <F1WAE>}
;          | {- <F1WAE> <F1WAE>}
;          | <id>
;          | {with {<id> <F1WAE>} <F1WAE>}
;          | {<id> <F1WAE>
;
; <Fundef> ::= {deffun {<id><id>} <F1WAE>}
;----------------------------------------
;[Require]
; 1-1) parser for F1WAE
; 1-2) parser for FunDef
; 2) interpreter
;                                               X) substitution
; 3) lookup for Function (function finder)
; 4) lookup for Deferred Substitution (cache!)
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

; 0-2) Type_FunDef
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)]
  )

; 0-3) Type_DefredSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)]
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
(define (interp f1wae fundefs ds)
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [id (s) (lookup s ds)]
    [with (i v e) (interp e (aSub i (interp v ds) ds))]
    [app (ftn arg) (local [(define a-fundef (lookup-fundefs ftn fundefs))]
                (interp (fundef-body a-fundef) fundefs (aSub (fundef-arg-name a-fundef)
                                                             (interp arg fundefs ds)
                                                             (mtSub))))]
    )
  )

; 3) lookup for function (function finder)
(define (lookup-fundefs name fundefs)
  (cond
    [(empty? fundefs) (error 'lookup-fundef "unknown function")]
    [else (if (symbol=? name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundefs name (rest fundefs)))]
    )
  )

; 4) lookup for Deferred Substitution (cache!)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? name i) v (lookup name saved))]
    )
  )
