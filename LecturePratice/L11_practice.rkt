#lang plai
; First-class function
;
; <FWAE> ::= <num>
;          | {+ <FWAE> <FWAE>}
;          | {- <FWAE> <FWAE>}
;          | {with {<id><FWAE>} <FWAE>}
;          | <id>
;          | {<id><FWAE>}          _function call
;          | {fun {<id>} <FWAE>}   _function definition

; -------------------------------------------------------------
;(define-type FunDef
;  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE?)]
;  )
;(define-type F1WAE
;  [num (n number?)]
;  [add (lhs F1WAE?) (rhs F1WAE?)]
;  [sub (lhs F1WAE?) (rhs F1WAE?)]
;  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
;  [id (name symbol?)]
;  [app (ftn symbol?) (arg F1WAE?)]; AST Keyword : about thefunction 
;  )
; -------------------------------------------------------------

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FWAE?)]   ;_FunDef에서 fun-name field만 사라짐 = 익명함수(anonymous)
  [app (ftn FWAE?) (arg FWAE?)]; AST Keyword : about thefunction 
  )

(fun 'x (add (id 'x)(id 'x)))

; -------------------------------------------------------------
; F1WAE의 parse function (not using cache)
;(define (parse sexp)
;  (match sexp
;    [(? number?) (num sexp)]
;    [(list '+ l r) (add parse l) (parse r)]
;    [(list '- l r) (sub parse l) (parse r)]
;    [(list 'with (list i v) e) (with i (parse v) (parse e))]
;    [(? symbol?) (id sexp)]
;    [(list f a) (app f (parse a))]
;    [else (error 'parse "bad syntax: ~a" sexp)]
;    )
;  )
; -------------------------------------------------------------

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )