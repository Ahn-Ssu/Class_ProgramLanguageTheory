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
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax:~a" sexp)]
    )
  )
(test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
(test (parse '{with {x 5} {+ 8 2}}) (with 'x (num 5) (add (num 8) (num 2))))
(test (parse '{with {x 5} {+ x x}}) (with 'x (num 5) (add (id 'x) (id 'x))))
(test (parse '{with {x 3} {with {y 4}{with {z 5}{+ x {+ y z}}}}})
      (with 'x (num 3) (with 'y (num 4) (with 'z (num 5) (add (id 'x) (add (id 'y) (id 'z))))))
      )
               
; 우리가 identifier를 지정하면 지정된 대로 수행을 함 
; (test (parse '{* {x 5} {+ x x}}) (* 'x (num 5) (add (id 'x) (id 'x))))
; 결국 인터프리터는 알려준 해독 방법으로
; Concrete Syntaxㅡ를 읽고 난 후 AST를 생성하는 것이다.



; interpreter 를 도와주는 substitution function
; [contract] subst : WAE symbol number -> WAE
(define (subst wae bound-id actual-value)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [sub (l r) (sub (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [with (i v e) (with i (subst v bound-id actual-value) ; with i ( lhs , rhs ) 의 형태를 가지게 됨 
                        (if (symbol=? i bound-id) e (subst e bound-id actual-value)))] ; if 문에서 조건이 참이면 왼항, 거짓이면 오른항 수행 
    [id (s) (if (symbol=? s bound-id) (num actual-value) wae)]
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
(test (subst (with 'z (num 5) (add (id 'x) (add (id 'y) (id 'z)))) 'z 5) (num 5))


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

