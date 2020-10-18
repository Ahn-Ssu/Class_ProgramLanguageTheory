#lang plai
; -------------------------------------------------------------
; L 11



; -------------------------------------------------------------
;(define (subst wae bound-id actual-value)
;  (type-case WAE wae
;    [num (n) wae]
;    [add (l r) (add (subst l bound-id actual-value)(subst r bound-id actual-value))]
;    [sub (l r) (sub (subst l bound-id actual-value)(subst r bound-id actual-value))]
;    [with (i v e) (with i (subst v bound-id actual-value) ; with i ( lhs , rhs ) 의 형태를 가지게 됨 
;                        (if (symbol=? i bound-id)
;                         e 
;                         (subst e bound-id actual-value)))] ; if 문에서 조건이 참이면 왼항, 거짓이면 오른항 수행 
;    [id (s) (if (symbol=? s bound-id) (num actual-value) wae)]
;    [app (f a) (app f (subst a idtf val))]
;    )
;  )
; -------------------------------------------------------------


; -------------------------------------------------------------
; L 12
; -------------------------------------------------------------
;(define (interp fwae)
;  (type-case FWAE fwae
;    [num (n) fwae]
;    [add (l r) (num+ (interp l)(interp r))]
;    [sub (l r) (num- (interp l)(interp r))]
;    [with (i v e) (interp (subst e i (interp v)))]
;    [id (s) (error 'interp "free indentifier")]
;    [fun (p b) fwae] ; 얘는 이제 first-class니까 return value가 될 수 있음 
;    [app (f a) (local [(define ftn (interp f))]
;                 (interp (subst (fun-body ftn)  ;fwae 
;                                (fun-param ftn) ; bound-id
;                                (interp a))))]  ; actual-value 
;    )
;  )
; -------------------------------------------------------------
; 이제 AST 는 더이상 with keyword format을 생성하지 않음
; <FAE> ::= <num>
;          | {+ <FAE> <FAE>}
;          | {- <FAE> <FAE>}
;          | {with {<id><FAE>} <FAE>}
;          | <id>
;          | {<FAW><FWAE>}          _function call
;          | {fun {<id>} <FAE>}   _function definition

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
; [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FAE?)]   ;_FunDef에서 fun-name field만 사라짐 = 익명함수(anonymous)
  [app (ftn FAE?) (arg FAE?)]; AST Keyword : about thefunction 
  )

(define (num-op op)
  (lambda(x y)
    (num (op (num-n x) (num-n y)))
    )
  )
(define num+ (num-op +))
(define num- (num-op -))


(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e))(parse v))] ; with keyword도 app의 형태로 생산 
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )

(parse '{with {x 3} {+ x x}})
(parse '{fun (x) {+ x x}})
(parse '{{fun {x} {+ x x}} 3})


(define (subst exp bound-id actual-value)
  (type-case FAE exp
    [num (n) exp]
    [add (l r) (add (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [sub (l r) (sub (subst l bound-id actual-value)(subst r bound-id actual-value))]
;    [with (i v e) (with i (subst v bound-id actual-value) ; with i ( lhs , rhs ) 의 형태를 가지게 됨 
 ;                       (if (symbol=? i bound-id)
  ;                       e 
   ;                      (subst e bound-id actual-value)))] ; if 문에서 조건이 참이면 왼항, 거짓이면 오른항 수행 
    [id (name) (cond [(equal? name bound-id) actual-value]
                     [else exp])]
    [app (f arg) (app (subst f bound-id actual-value)
                      (subst arg bound-id actual-value))]
    [fun (id body) (if (equal? bound-id id)
                       exp
                       (fun id (subst body bound-id actual-value)))]; 이 코드의 리턴은 함수 자체로 리턴하는 거임 
    )
  )

;(define (interp fae)
;  (type-case FAE fae
;    [num (n) fae]
;    [add (l r) (num+ (interp l) (interp r))]
;    [sub (l r) (num- (interp l) (interp r))]
;    [id (s) (error 'interp "free identifier")]
;    [fun (p b) fae]
;    [app (f a) (local [(define ftn f))]
;                 (interp (subst (fun-body ftn)
;                                (fun-param ftn)
;                                (interp a))))]
;    )
;  )
;(interp (parse '{with {x 3} {+ x x}}))
;(interp (parse '{fun (x) {+ x x}}))
;(interp (parse '{{fun {x} {+ x x}} 3}))
;(parse '{{+ a b} 5})
;(interp (app (add (id 'a) (id 'b)) (num 5)))
;(parse '{{+ 3 4} 5})
;(interp (app (add (num 3) (num 4)) (num 5)))


(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)]
  )
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(value FAE?)(saved DefrdSub?)]
  )
(define (lookup name ds)
  (cond
    [(mtSub? ds)
     (error 'lookup "unknown")]
    [else
     (if (symbol=? name (aSub-name ds))
         (aSub-value ds)
         (lookup name aSub-saved))]
    )
  )
(define (interp fae ds)
  (type-case FAE fae
    [num (n) fae]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) fae]
    [app (f a) (local ([define ftn (interp f ds)])
                 (interp (fun-body ftn) (aSub (fun-param ftn)
                                              (interp a ds)
                                              ds))
                 )]
    )
  )
(num-n (num 3))
(parse '{with {x 3}{with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(parse '(+ 3 3))
(interp (add (num 3) (num 3)) (mtSub))
(interp (app (fun 'x (add (id 'x) (id 'x))) (num 3)) (mtSub))
;(interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))
(interp (app (fun 'x (app (id 'x) (num 4))) (num 5)) (mtSub))
