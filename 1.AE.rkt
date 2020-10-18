#lang plai
;AE
;----------------------------------------
;[BNF]
; <AE> ::= <num>
;          | {+ <AE> <AE>}
;          | {- <AE> <AE>}
;----------------------------------------
;[Require]
; 1) parser
; 2) interpreter
;----------------------------------------
; 0) Type
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)]
  )

; 1) Parse
(define (parse ae)
  (match ae
    [(? number?) (num ae)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [else (error 'parse "bad syntax: ~a" ae)
    )
  )
(parse '(+ 3 5))
(parse '(+ (+ 3 5) (- 6 4)))

; 2) Interpreter
(define (interp ae)
  (type-case AE ae
    [num (n) n]
    [add (l r) (+ (interp l)(interp r))]
    [sub (l r) (- (interp l)(interp r))]
    )
  )

(interp (add (num 3) (num 5)))
(interp (add (add (num 3) (num 5)) (sub (num 6) (num 4))))
(test (num-n (num 7)) 7)

  