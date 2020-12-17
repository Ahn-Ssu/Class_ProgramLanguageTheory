#lang plai
; L 17-20, Mutable
; <BFAE> ::= <num>
;          | {+ <BFAE> <BFAE>}
;          | {- <BFAE> <BFAE>}
;          | <id>
;          | {fun {<id>} <BFAE>}   _function definition
;          | {<BFAE><BFAE>}        _function call
;          | {newbox <BFAE>}
;          | {setbox <BFAE> <BFAE>}
;          | {openbox <BFAE>}
;          | {seqn <BFAE> <BFAE>}
; -------------------------------------------------------------
(define-type BFAE
  [num (n number?)]
  [add (lhs BFAE?)(rhs BFAE?)]
  [sub (lhs BFAE?)(rhs BFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BFAE?)]
  [newbox (v BFAE?)]
  [setbox (bn BFAE?)(v BFAE?)]
  [openbox (v BFAE?)]
  [seqn (ex1 BFAE?)(ex2 BFAE?)]
  [app (ftn BFAE?)(arg BFAE?)]
  )

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list 'newbox exp) (newbox (parse exp))]
    [(list 'setbox exp1 exp2) (setbox (parse exp1)(parse exp2))]
    [(list 'openbox exp) (openbox (parse exp))]
    [(list 'seqn exp1 exp2) (seqn (parse exp1) (parse exp2))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )
(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV (container (box/c BFAE-Value?))]
  )

;(define-type Store
;  [mtSto]
;  [aSto (address integer?) (value BFAE-Value?) (rest Store?)]
;  )

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(address BFAE-Value?)(ds DefrdSub?)]
  )

;(define-type Value*Store
;  [v*s (value BFAE-Value?)(store Store?)]
;  )

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "Free identifier")]
    [aSub (i adr saved) (if(symbol=? i name)
                           adr
                           (lookup name saved))]
    )
  )
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))
  )


(define num+ (num-op +))
(define num- (num-op -))

;(define (store-lookup address sto)
;  (type-case Store sto
;    [mtSto () (error 'store-lookup "No value at address")]
;    [aSto (location value rest-store) (if(= location address)
;                                         value
;                                         (store-lookup address rest-store))]
;    )
;  )
;
;(define malloc
;  (local ([define max-address (box -1)])
;    (lambda (store)
;      (begin
;        (set-box! max-address (+ 1 (unbox max-address)))
;        (unbox max-address)))
;    )
;  )


;interp : BFAE DefrdSub Store -> Value*Store
(define (interp expr ds)
  (type-case BFAE expr
  [num (n) (numV n)]
  [add (l r) (num+ (interp l ds) (interp r ds))]
  [sub (l r) (num- (interp l ds) (interp r ds))]
  [id (s) (lookup s ds)]
  [fun (p b) (closureV p b ds)]
  [app (f a) (local [(define ftn (interp f ds))
                     (define a-val (interp a ds))]
                   (interp (closureV-body ftn)
                         (aSub (closureV-param ftn)
                               a-val
                               (closureV-ds ftn))))]
;  [newbox (val) (type-case Value*Store (interp val ds st)
;                  [v*s (vl st1) (local [(define a (malloc st1))]
;                                  (v*s (boxV a)
;                                       (aSto a vl st1)))])]
  [newbox (val-expr) (boxV (box (interp val-expr ds)))]
;  [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st)
;                       [v*s (bx-val st1) (v*s (store-lookup (boxV-address bx-val) st1)
;                                              st1)])]
  [openbox (box-expr) (unbox (boxV-container (interp box-expr ds)))]
;  [setbox (bx-expr val-expr) (type-case Value*Store (interp bx-expr ds st)
;                               [v*s (bx-val st2)
;                                    (tpye-case Value*Store (interp val-expr ds st2)
;                                               [v*s (val st3) (v*s val
;                                                                   (aSto (boxV-address bx-val) val st3))])])]
  [setbox (box-expr val-expr) (set-box! (boxV-container (interp box-expr ds))
                                       (interp val-expr ds))]
  [seqn (a b) (local [(define firstExp (interp a ds))]
                (interp b ds))]
  )
  )


(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         (type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3) (handle val1 val2 st3)])]
    )
  )

;----------------------------------------------------------
(require racket/trace)
(trace interp)
( parse '(with (b (newbox 7))
              (seqn (setbox b 10) 
                    (openbox b))))
(parse '(newbox 7))
(interp (parse '(newbox 7)) (mtSub))
(interp (parse '(with (b (newbox 7))
              (seqn (setbox b 10) 
                    (openbox b)))) (mtSub))