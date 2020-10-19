#lang plai

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)]
  )
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?) (arg F1WAE?)]; AST Keyword : about thefunction 
  )

(fundef 'identify 'x
        (id 'x)
        )
(app 'identity (num 8))

(fundef 'twice 'x
        (add (id 'x) (id 'x))
        )
(app 'twice (num 10))
(app 'twice (num 17))
(app 'twice (num 3))


;parse-fd : sexp -> FunDef
(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]
    )
  )

;parse : sexp -> F1WAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list f a) (app f (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )


;interp: F1WAE list-of=FuncDef -> number
(define (interp f1wae fundefs)
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs) (interp r fundefs))]
    [sub (l r) (- (interp l fundefs) (interp r fundefs))]
    [with (i v e) (interp (subst e i (interp v fundefs)) fundefs)]
    [id (s) (error 'interp "free identifier")]
    [app (f a)
         (local
           [(define a_fundef (lookup-fundefs f fundefs))]
           (interp (subst (fundef-body a_fundef)
                          (fundef-arg-name a_fundef)
                          (interp a fundefs))
                   fundefs)
           )
         ]
    )
  )

(define (lookup-fundefs name fundefs)
  (cond
    [(empty? fundefs)
     (error 'lookup-fundef "unknown function")]
    [else
     (if (symbol=? name (fundef-fun-name (first fundefs)))
         (first fundefs)
         (lookup-fundefs name (rest fundefs)))]
    )
  )

; +
(define (subst f1wae bound-id actual-value)
  (type-case F1WAE f1wae
    [num (n) f1wae]
    [add (l r) (add (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [sub (l r) (sub (subst l bound-id actual-value)(subst r bound-id actual-value))]
    [with (i v e) (with i (subst v bound-id actual-value) ; with i ( lhs , rhs ) 의 형태를 가지게 됨 
                        (if (symbol=? i bound-id)
                         e 
                         (subst e bound-id actual-value)))] ; if 문에서 조건이 참이면 왼항, 거짓이면 오른항 수행 
    [id (s) (if (symbol=? s bound-id) (num actual-value) f1wae)]
    [app (f a) (app f (subst a bound-id actual-value))]
    )
  )


(subst (app 'fn (id 'x)) 'x 1)
(subst (add (id 'x)(app 'fn (id 'x))) 'x 2)
(subst (add (id 'x)(app 'fn (id 'y))) 'x 2)
(interp (app 'f (num 1)) (fundef 'f 'x (add (id 'x) (num 3))))