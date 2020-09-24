#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?)(name-exp WAE?)(body WAE?)]
  [id (name symbol?)]
  )

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
     
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax:~a" sexp)]
    )
  )
(test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
(test (parse '{with {x 5} {+ 8 2}}) (with 'x (num 5) (add (num 8) (num 2))))
(test (parse '{with {x 5} {+ x x}}) (with 'x (num 5) (add (id 'x) (id 'x))))
; 우리가 identifier를 지정하면 지정된 대로 수행을 함 
; (test (parse '{* {x 5} {+ x x}}) (* 'x (num 5) (add (id 'x) (id 'x))))



; interpreter 를 도와주는 substitution function
; [contract] subst : WAE symbol number -> WAE
(define (subst wae bound-id actual-value)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [sub (l r) (sub (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [with (i v e) (with i (subst v bound-id actual-value)(if (symbol=? i bound-id)
                                                             e (subst e bound-id actual-value)))]
    [id (s) (if (symbol=? s bound-id) (num actual-value) wae)]
    )
  )

(test (subst (with 'y (num 17) (id 'x)) 'x 10) (with 'y (num 17) (num 10)))
(test (subst (with 'y (id 'x) (id 'y)) 'x 10) (with 'y (num 10) (id 'y)))
(test (subst (with 'x (id 'y) (id 'x)) 'x 10) (with 'x (id 'y) (id 'x)))


; [contract] interp : WAE -> number
(define (interp wae)
  (type-case WAE wae 
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s)     (error 'interp "free identifier ~a" s)]
    )
  )

; questionEx

(interp (with 'x (num 5) (add (num 1) (with 'y (id 'x) (id 'y)))))
;(interp (parse 'p))
;(add (num 1) (with 'y (id 'x) (id 'y))))

 