#lang plai
; WAE
;----------------------------------------
;[BNF]
; <WAE> ::= <num>
;          | {+ <WAE> <WAE>}
;          | {- <WAE> <WAE>}
;          | <id>
;          | {with {<id> <WAE>} <WAE>}
;----------------------------------------
;[Require]
; 1) parser
; 2) interpreter
; 3) substitution
;----------------------------------------
; 0) Type
(define-type WAE
  [num (n number?)]
  [add (l WAE?) (r WAE?)]
  [sub (l WAE?) (r WAE?)]
  [id (name symbol?)]
  [with (name symbol?) (name-exp WAE?) (body WAE?)]
  )

; 1) parser
(define (parse wae)
  (match wae
    [(? number?) (num wae)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(? symbol?) (id wae)]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [else (error 'parse "bad syntax: ~a" wae)]
    )
  )

(parse 3)
(parse '{+ 3 4})
(parse '{with (x 5) {+ x x}})
(parse '{with (x (with (k 5) (+ k 5))) {+ x 3}})

; 2) interp
(define (interp wae)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [id (s) (error 'interp "free identifier")]
    [with (i v e) (interp (subst e i (interp v)))]
    )
  )

; 3) substitution
(define (subst wae idtf val)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [id (s) (if (symbol=? s idtf) (num val) wae)]
    [with (i v e) (with i (subst v idtf val)
                        (if (symbol=? i idtf) e (subst e idtf val)))] 
    )
  )
(test (subst (with 'y (num 17) (id 'x)) 'x 10) (with 'y (num 17) (num 10)))
(test (subst (with 'y (id 'x) (id 'y)) 'x 10) (with 'y (num 10) (id 'y)))
(test (subst (with 'x (id 'y) (id 'x)) 'x 10) (with 'x (id 'y) (id 'x)))
;(test (subst (with 'x (with 'y (with 'z ((id 'x) ( (id 'y) (id 'z)))) 'z 3) 'y 4) 'x 3)
 ;     (add (num 3) (add (num 4) (num 5)))
  ;    )
(test (subst  (with 'x (num 3) (with 'y (num 4) (with 'z (num 5) (add (id 'x) (add (id 'y) (id 'z)))))) 'x 3)
      (with 'x (num 3) (with 'y (num 4) (with 'z (num 5) (add (id 'x) (add (id 'y) (id 'z))))))
      )
(subst (with 'z (num 5) (add (id 'x) (add (id 'y) (id 'z)))) 'z 5)
(subst (with 'z (num 5) (add (id 'x) (id 'z))) 'z 5)
(subst (id 'z) 'z 5)

    