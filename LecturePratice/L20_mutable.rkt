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

(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV (container (box/c BFAE-Value?))]
  )

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BFAE-Value?) (rest Store?)]
  )

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(address integer?)(ds DefrdSub?)]
  )

(define-type Value*Store
  [v*s (value BFAE-Value?)(store Store?)]
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


;interp : BFAE DefrdSub Store -> Value*Store
(define (interp expr ds st)
  [num (n) (v*s (numV n) st)]
  [add (l r) (type-case Value*Store (interp l ds st)
               [v*s (l-value l-store)
                    (type-case Value*Store (interp r ds l-store)
                      [v*s (r-value r-store)
                           (v*s (num+ l-value r-value) r-store)])])]
  [id (s) (v*s (store-lookup (lookup s ds) st) st)]
  [fun (p b) (v*s *(closureV p b ds) st)]
  [app (f a) (type-case Value*Store (interp f ds st)
               [v*s (f-value f-store)
                    (type-case Value*Store (interp a ds f-store)
                      [v*s (a-value a-store
                                    (local ([define new-address (malloc a-stre)])
                                      (interp (closureV-body f-value)
                                              (aSub (closureV-param f-value) new-address (closureV-ds f-value))
                                              (aSto new-address a-value a-store))))])])]
  [newbox (val) (type-case Value*Store (interp val ds st)
                  [v*s (vl st1) (local [(define a (malloc st1))]
                                  (v*s (boxV a)
                                       (aSto a vl st1)))])]
  [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st)
                       [v*s (bx-val st1) (v*s (store-lookup (boxV-address bx-val) st1)
                                              st1)])]
  [setbox (bx-expr val-expr) (type-case Value*Store (interp bx-expr ds st)
                               [v*s (bx-val st2)
                                    (tpye-case Value*Store (interp val-expr ds st2)
                                               [v*s (val st3) (v*s val
                                                                   (aSto (boxV-address bx-val) val st3))])])]
  )


(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         (type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3) (handle val1 val2 st3)])]
    )
  )

;----------------------------------------------------------

(define (interpp expr ds st)
  [add (l r) (tinerp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]_