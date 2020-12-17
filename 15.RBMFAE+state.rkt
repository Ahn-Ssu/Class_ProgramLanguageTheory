#lang plai
;전에 했던 구현은 store를 안쓰는 상태로 했었는데, 안쓰니까 못풀겠어요
; L 21-22, Variables
; <RBMFAE> ::= <num>
;          | {+ <RBMFAE> <RBMFAE>}
;          | {- <RBMFAE> <RBMFAE>}
;          | <id>
;          | {fun {<id>} <RBMFAE>}   _function definition
;          | {refun {<id>} <RBMFAE>}   _function definition
;          | {<RBMFAE><RBMFAE>}        _function call
;          | {newbox <RBMFAE>}
;          | {setbox <RBMFAE> <RBMFAE>}
;          | {openbox <RBMFAE>}
;          | {seqn <RBMFAE> <RBMFAE>}
;          | {setvar <id> <RBMFAE>}
; -------------------------------------------------------------
(define-type RBMFAE
  [num (n number?)]
  [add (lhs RBMFAE?)(rhs RBMFAE?)]
  [sub (lhs RBMFAE?)(rhs RBMFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RBMFAE?)]
  [refun (param symbol?) (body RBMFAE?)]
  [newbox (v RBMFAE?)]
  [setbox (bn RBMFAE?)(v RBMFAE?)]
  [openbox (v RBMFAE?)]
  [seqn (ex1 RBMFAE?)(ex2 RBMFAE?)]
  [setvar (i symbol?)(val RBMFAE?)]
  [app (ftn RBMFAE?)(arg RBMFAE?)]
  )

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list 'refun (list p) b) (refun p (parse b))]
    [(list 'newbox exp) (newbox (parse exp))]
    [(list 'setbox exp1 exp2) (setbox (parse exp1)(parse exp2))]
    [(list 'openbox exp) (openbox (parse exp))]
    [(list 'seqn exp1 exp2) (seqn (parse exp1) (parse exp2))]
    [(list 'setvar i val) (setvar i (parse val))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )
(parse '(setvar i 5))
(define-type RBMFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RBMFAE?) (ds DefrdSub?)]
  [refcolsV (param symbol?) (body RBMFAE?) (ds DefrdSub?)]
  [boxV (address integer?)]
  )

(define-type Store
  [mtSto]
  [aSto (address integer?) (value RBMFAE-Value?) (rest Store?)]
  )

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(address integer?)(ds DefrdSub?)]
  )

(define-type Value*Store
  [v*s (value RBMFAE-Value?)(store Store?)]
  )

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "Free identifier")]
    [aSub (i adr saved) (if(symbol=? i name)
                           adr
                           (lookup name saved))]
    )
  )

(define (store-lookup address sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "No value at address")]
    [aSto (location value rest-store) (if(= location address)
                                         value
                                         (store-lookup address rest-store))]
    )
  )

(define malloc
  (local ([define max-address (box -1)])
    (lambda (store)
      (begin
        (set-box! max-address (+ 1 (unbox max-address)))
        (unbox max-address)))
    )
  )


(define (num-op op)
  (lambda(x y)
    (numV (op (numV-n x) (numV-n y)))
    )
  )
;[contract] num+ : numV numV -> numV
;[purpose] to add number value of numV
(define num+ (num-op +))
;[contract] num- : numV numV -> numV
;[purpose] to sub number value of numV
(define num- (num-op -))


;interp : BFAE DefrdSub Store -> Value*Store
(define (interp expr ds st)
  (type-case RBMFAE expr
    [num (n) (v*s (numV n) st)]
    [add (l r) (type-case Value*Store (interp l ds st)
                 [v*s (l-value l-store)
                    (type-case Value*Store (interp r ds l-store)
                      [v*s (r-value r-store)
                           (v*s (num+ l-value r-value) r-store)])])]
    [sub (l r) (type-case Value*Store (interp l ds st)
               [v*s (l-value l-store)
                    (type-case Value*Store (interp r ds l-store)
                      [v*s (r-value r-store)
                           (v*s (num- l-value r-value) r-store)])])]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [refun (p b) (v*s (refcolsV p b ds) st)]
    [app (f a) (type-case Value*Store (interp f ds st)
               [v*s (f-value f-store)
                    (type-case RBMFAE-Value f-value
                      [closureV (c-param c-body c-ds)
                                (type-case Value*Store (interp a ds f-store)
                                  [v*s (a-value a-store)
                                                (local ([define new-address (malloc a-store)])
                                                  (interp c-body
                                                          (aSub c-param new-address c-ds)
                                                          (aSto new-address a-value a-store)))])]
                      [refcolsV (rc-param rc-body rc-ds)
                                (local ([define address (lookup (id-name a) ds)])
                                  (interp rc-body
                                          (aSub rc-param address rc-ds)
                                          f-store))]
                      [else (error interp "trying to apply a number")]
                      )])]
;  [newbox (val) (type-case Value*Store (interp val ds st)
;                  [v*s (vl st1) (local [(define a (malloc st1))]
;                                  (v*s (boxV a)
;                                       (aSto a vl st1)))])]
  [newbox (val-expr) (boxV (box (interp val-expr ds)))]
    [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st)
                       [v*s (bx-val st1) (v*s (store-lookup (boxV-address bx-val) st1)
                                              st1)])]
    [setbox (bx-expr val-expr) (type-case Value*Store (interp bx-expr ds st)
                               [v*s (bx-val st2)
                                    (type-case Value*Store (interp val-expr ds st2)
                                               [v*s (val st3) (v*s val
                                                                   (aSto (boxV-address bx-val) val st3))])])]
    [seqn (a b) (type-case Value*Store (interp a ds st)
                [v*s (a-value a-store) (interp b ds a-store)])]
    [setvar (id val-expr) (local ([define a (lookup id ds)])
                            (type-case Value*Store (interp val-expr ds st)
                              [v*s (val st) (v*s val (aSto a val st))]))]
  )
  )


;----------------------------------------------------------
(parse '{with (swap (refun {x} (refun {y} {with {z x} {seqn {setvar x y}{setvar y z}}})))
              (with (a 10) (with (b 20) (seqn ((swap a) b) b)))})
(require racket/trace)
(trace interp)
(parse '{with (swap (fun {x} (fun {y} {with {z x} {seqn {setvar x y}{setvar y z}}})))
              (with (a 10) (with (b 20) (seqn ((swap a) b) a)))})
'(app (fun 'swap (app (fun 'a (app (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))) (num 20))) (num 10)))
      (fun 'x (fun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))))
(interp (app (fun 'swap (app (fun 'a (app (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))) (num 20))) (num 10)))
      (fun 'x (fun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))))
        (mtSub))

