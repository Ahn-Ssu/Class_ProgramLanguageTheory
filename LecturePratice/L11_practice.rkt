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


; -------------------------------------------------------------
; L 11
; -------------------------------------------------------------
;interp: F1WAE list-of=FuncDef -> number
;(define (interp f1wae fundefs)
;  (type-case F1WAE f1wae
;    [num (n) n]
;    [add (l r) (+ (interp l fundefs) (interp r fundefs))]
;    [sub (l r) (- (interp l fundefs) (interp r fundefs))]
;    [with (x i b) (interp (subst b x (interp i fundefs)) fundefs)]
;    [id (s) (error 'interp "free identifier")]
;    [app (f a)
;         (local
;           [(define a_fundef (lookup-fundef f fundefs))]
;           (interp (subst (fundef-body a_fundef)
;                          (fundef-arg-name a_fundef)
;                          (interp a fundefs))
;                   fundefs)
;           )
;         ]
;    )
;  )
; fundefs라는 인수로 함수페어를 제공함요 
; -------------------------------------------------------------
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (num+ (interp l)(interp r))]
    [sub (l r) (num- (interp l)(interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free indentifier")]
    [fun (p b) fwae] ; 얘는 이제 first-class니까 return value가 될 수 있음 
    [app (f a) (local [(define ftn (interp f))]
                 (interp (subst (fun-body ftn)  ;fwae 
                                (fun-param ftn) ; bound-id
                                (interp a))))]  ; actual-value 
    )
  )

(define (num-op op)
  (lambda(x y)
    (num (op (num-n x) (num-n y)))
    )
  )
(define num+ (num-op +))
(define num- (num-op -))


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
(define (subst exp bound-id actual-value)
  (type-case FWAE exp
    [num (n) exp]
    [add (l r) (add (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [sub (l r) (sub (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [with (i v e) (with i (subst v bound-id actual-value) ; with i ( lhs , rhs ) 의 형태를 가지게 됨 
                        (if (symbol=? i bound-id)
                         e 
                         (subst e bound-id actual-value)))] ; if 문에서 조건이 참이면 왼항, 거짓이면 오른항 수행 
    [id (name) (cond [(equal? name idtf) actual-value]
                     [else exp])]
    [app (f arg) (app (subst f bound-id actual-value)
                      (subst arg bound-id val))]
    [fun (id body) (if (equal? bound-id id)
                       exp
                       (fun id (subst body bound-id actual value)))]; 이 코드의 리턴은 함수 자체로 리턴하는 거임 
    )
  )