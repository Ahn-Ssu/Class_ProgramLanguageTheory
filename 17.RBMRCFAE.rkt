#lang plai

; type definition for abstract syntax tree of RBMRCFAE
(define-type RBMRCFAE
    [num         (n number?)]
    [add          (lhs RBMRCFAE?) (rhs RBMRCFAE?)]
    [sub          (lhs RBMRCFAE?) (rhs RBMRCFAE?)]
    [id             (name symbol?)]
    [fun          (param symbol?) (body RBMRCFAE?)]
    [refun       (param symbol?) (body RBMRCFAE?)]
    [setvar     (name symbol?) (v RBMRCFAE?)]
    [newbox  (v RBMRCFAE?)]
    [setbox     (bn RBMRCFAE?) (v RBMRCFAE?)]
    [openbox (v RBMRCFAE?)]
    [seqn       (ex1 RBMRCFAE?) (ex2 RBMRCFAE?)]
    [app         (ftn RBMRCFAE?) (arg RBMRCFAE?)]
    [if0           (test-expr RBMRCFAE?)
                    (then-expr RBMRCFAE?) (else-expr RBMRCFAE?)]
    [rec          (name symbol?) (named-expr RBMRCFAE?) (body RBMRCFAE?)]
  )

       
; parse : sexp -> RBMRCFAE
(define (parse sexp)
   (match sexp
        [(? number?)             (num sexp)]
        [(list '+ l r)                  (add (parse l) (parse r))]
        [(list '- l r)                   (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)             (id sexp)]
        [(list 'newbox v)        (newbox (parse v))]
        [(list 'setbox i v)        (setbox (parse i) (parse v))]
        [(list 'openbox i)        (openbox (parse i))]
        [(list 'seqn ex1 ex2)  (seqn (parse ex1) (parse ex2))]
        [(list 'fun (list p) b)    (fun p (parse b))]
        [(list 'refun (list p) b) (refun p (parse b))]
        [(list f a)                    (app (parse f) (parse a))]
        [(list 'setvar n v)        (setvar n (parse v))]
        [(list 'if0 te th el)       (if0 (parse te) (parse th)  (parse el))]
        [(list 'rec (list rfn ne) body)    (rec rfn (parse ne) (parse body))]
        [else                        (error 'parse "bad syntax: ~a" sexp)]))


(define-type RBMRCFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?) (body RBMRCFAE?) (ds DefrdSub?)] ; for call-by-value
  [refclosV  (param symbol?) (body RBMRCFAE?) (ds DefrdSub?)] ; for call-by-refrence
  [boxV      (address integer?)])


; num-op: operator -> (numV numV -> numV) ;  
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)]
  [aRecSub (name symbol?)
           (value-box (box/c Value*Store?))
           (ds DefrdSub?)])

(define-type Store ; (3) This interpreter produces new store in each computation step.
  [mtSto]                ;        What do we call this kind of interpreters? (0.5P) ⇒  
  [aSto   (address integer?) (value RBMRCFAE-Value?)
          (rest Store?)])

(define-type Value*Store
  [v*s (value RBMRCFAE-Value?) (store Store?)])

;lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i adr saved) (if(symbol=? i name)
                                adr
                                (lookup name saved))]
    [aRecSub (id val-box rest-ds)
             (if (symbol=? id name)
                 (unbox val-box)
                 (lookup name rest-ds))]
    ))

;store-lookup: (address or Value*Store) Store -> RBMRCFAE-Value
(define (store-lookup arg sto)
  (if (integer? arg)  ; (4) Why do we need this logic using 'if' block here? (3P) ⇒ 
      (type-case Store sto
         [mtSto ()           (error 'store-lookup "No value at address")]
         [aSto  (location value rest-store)
                 (if(= location arg)
                     value
                    (store-lookup arg rest-store))])
      (type-case Value*Store arg
        [v*s (v s) v])
  ))

; memory-alloc: Store -> Integer
; purpose: to get new address
(define (memory-alloc st)
  (+ 1 (max-address st)))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))
; numzero? :  RBMRCFAE-Value -> boolean
(define (numzero? n)
  (type-case Value*Store n
    [v*s (v s) (zero? (numV-n v))]))

; interp: RBMRCFAE DefrdSub -> Value*Store
(define (interp rbmrcfae ds st)
  (type-case RBMRCFAE rbmrcfae
    [num    (n)    (v*s (numV n) st)]
    [add    (l r)    (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub    (l r)    (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id     (s)       (v*s (store-lookup (lookup s ds) st) st)]
    [fun    (p b)   (v*s (closureV p b ds) st)]
    [refun  (p b)  (v*s (refclosV p b ds) st)]
    [app    (f a)    (type-case Value*Store (interp f ds st)
                           [v*s (f-value f-store)
                             (type-case RBMRCFAE-Value f-value
                                ;[refclosV (rc-param rc-body rc-ds)
                                 ;      (local ([define address1 (5.1.5P)_________________________])
                                  ;       (interp rc-body
                                   ;              (aSub (6.0.5P)____________
                                    ;                       (7.0.5P)____________
                                     ;                       rc-ds)
                                      ;           f-store))]
                               [refclosV (rc-param rc-body rc-ds)
                                (local ([define address (lookup (id-name a) ds)])
                                  (interp rc-body
                                          (aSub rc-param address rc-ds)
                                          f-store))]
                                ;[closureV (c-param c-body c-ds)
                                 ;     (type-case Value*Store (interp a ds f-store)
                                  ;       [v*s (a-value a-store)
                                   ;         (local ([define address2 (8.1.5P)________________________])
                                    ;              (interp c-body
                                     ;                     (aSub c-param
                                      ;                          address2
                                       ;                         c-ds)
                                        ;                   (aSto address2
                                         ;                        a-value
                                          ;                       a-store)))])]
                               [closureV (c-param c-body c-ds)
                                (type-case Value*Store (interp a ds f-store)
                                  [v*s (a-value a-store)
                                                (local ([define new-address (memory-alloc a-store)])
                                                  (interp c-body
                                                          (aSub c-param new-address c-ds)
                                                          (aSto new-address a-value a-store)))])]
                             [else (error interp "trying to apply a number")])])]
    [newbox  (val)    (type-case Value*Store (interp val ds st)
                     [v*s (vl st1)
                                   (local [(define a (memory-alloc st1))]
                                          (v*s (boxV a)
                                          (aSto a vl st1)))])]
    [setbox  (bx-expr val-expr)
                   (interp-two bx-expr val-expr ds st
                               (lambda (bx-val val st1)
                                       (v*s val
                                       (aSto (boxV-address bx-val)
                                             val
                                             st1))))]
    [openbox (bx-expr)
                   (type-case Value*Store (interp bx-expr ds st)
                     [v*s (bx-val st1)
                                       (v*s (store-lookup (boxV-address bx-val)
                                                           st1)
                                             st1)])]
    [seqn    (a b) (interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]
    [setvar  (id val-expr) (local [(define a (lookup id ds))]
                           (type-case Value*Store (interp val-expr ds st)
                             [v*s (val st)
                                  (v*s val
                                       (aSto a val st))]))]
    [if0 (test-expr then-expr else-expr)
                   (if(numzero? (interp test-expr ds st))
                           (interp then-expr ds st)
                           (interp else-expr ds st))]
    [rec (bound-id named-expr first-call)
                       ; (9) What do we call (numV 198) in our context? (0.5P) ⇒ 
                       (local [(define value-holder (box (v*s (numV 198) (mtSto))))
                               (define new-ds (aRecSub bound-id
                                                       value-holder
                                                       ds))]
                              (begin
                                (set-box! value-holder (interp named-expr new-ds st))
                                (interp first-call new-ds st)))]
    ))

;interp-two: RBMRCFAE RBMRCFAE DefrdSub Store
;            (Value Value Store -> Value*Store)
;            -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))
