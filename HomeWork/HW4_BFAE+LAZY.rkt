#lang plai
;HW4

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

; [Description]
; Implement BFAE without State by using “Interp-two” as we learned in L20.
; Apply “interp-two” for setbox, seqn, add, and sub branches.

; BFAE AST
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

; For result
(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV (address integer?)]
  [exprV (expr BFAE?) (ds DefrdSub?) (st Store?) (value (box/c (or/c false BFAE-Value?)))]
  )

; for memory, mapping address and Value 
(define-type Store
  [mtSto]
  [aSto (address integer?) (value BFAE-Value?) (rest Store?)]
  )

; for deferred substitution
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(address integer?)(ds DefrdSub?)]
  )

; for mutable concept, To use in seqn 
(define-type Value*Store
  [v*s (value BFAE-Value?)(store Store?)]
  )

; Solved by myself: Y
; Time taken: 10m
; [contract] parse: sexp -> BFAE
; [purpose] Consume the concrete code to abstract syntax, SDFAE
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

; [contract] num-op : op -> function
; [purpose] to make the functions that had same format
;           actually to do computation number value 
(define (num-op op)
  (lambda(x y)
    (numV (op (numV-n x) (numV-n y)))
    )
  )
(define num+ (num-op +))
(define num- (num-op -))


; [contract] lookup : symbol, DefrdSub -> address
; [purpose] to fine address of binding id
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "Free identifier")]
    [aSub (i adr saved) (if(symbol=? i name)
                           adr
                           (lookup name saved))]
    )
  )


; [contract]  store-lookup: address, Store -> BFAE-Value
; [purpose] to fine the value matched address
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "No value at address")]
    [aSto (location value rest-store) (if(= location address)
                                         value
                                         (store-lookup address rest-store))]
    )
  )

; [contract] malloc : (void) -> integer(address)
; [purpose] to generate new address number 
; malloc : Store -> Integer
(define (malloc st)
(+ 1 (max-address st))) ; what will be the first address?
; max-address: Store -> Integer
(define (max-address st) (type-case Store st
[mtSto () 0] [aSto (n v st)
(max n (max-address st))]))

; [contract] interp-two : BFAE BFAE DefrdSub Store function(lambda) -> Value*Store
; [purpose] to perform the interp twice and produce Value*Store type
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         (type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3) (handle val1 val2 st3)])]
    )
  )

; TimeTake 40m'
; [contract] interp : BFAE DefrdSub Store -> Value*Store
; [purpose] to perform the interp twice and produce Value*Store type
; Solved by myself: Y
; Time taken: 40m'
; [contract] interp: BFAE -> Value*Store
; [purpose] interprete BFAE to Value*Store type
(define (interp expr ds st)
  (type-case BFAE expr
  [num (n) (v*s (numV n) st)]
  [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
  [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
  [id (s) (v*s (strict (store-lookup (lookup s ds) st)) st)]

;      (local ([define strict-result (strict (store-lookup (lookup s ds) st))])
 ;           (if (newbox? strict-result)
  ;              (interp strict-result ds st)
   ;             (v*s strict-result st)))]

  [fun (p b) (v*s (closureV p b ds) st)]
  [app (f a) (if (newbox? a)
                 (interp-two f a ds st (lambda (v1 v2 aSt) (local ([define new-address (malloc aSt)])
                                                         (interp (closureV-body v1)
                                                                 (aSub (closureV-param v1) new-address (closureV-ds v1))
                                                                 (aSto new-address v2 aSt)))))
                 (type-case Value*Store (interp f ds st)
                   [v*s (vl st2)  (local ([define new-address (malloc st2)]
                                      [define ftn-v (strict vl)]
                                      [define arg-v (exprV a ds st2 (box #f))])
                                (interp (closureV-body ftn-v)
                                        (aSub (closureV-param ftn-v) new-address (closureV-ds ftn-v))
                                        (aSto new-address arg-v st2)))]))]
  [newbox (val); (type-case Value*Store (interp val ds st)
                ;  [v*s (vl st1)
                       (local [(define a (malloc st))]
                                  (v*s (boxV a)
                                       (aSto a (exprV val ds st (box #f)) st)))];)]
  [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st)
                       [v*s (bx-val st1) (v*s (strict (store-lookup (boxV-address bx-val) st1))
                                              st1)])]
  [setbox (bx-expr val-expr) (interp-two bx-expr val-expr ds st (lambda (v1 v2 st3) (v*s v2 (aSto (boxV-address v1) v2 st3))))]
  [seqn (a b) (type-case Value*Store (interp a ds st)
                [v*s (a-value a-store) (interp b ds a-store)])]
  )
  )


(define (strict v)
         (type-case BFAE-Value v
           [exprV (expr ds st v-box)
                  ;(if (newbox? expr) expr
                  (if (not (unbox v-box)) (local [(define val (strict (v*s-value (interp expr ds st))))]
                                                             (begin (set-box! v-box val)
                                                                    (strict val)))
                                                             (unbox v-box))]
           [else v])
  )

(define (run sexp ds st)
  (interp (parse sexp) ds st)
  )

(test (run '{with {b {newbox 7}}
          {setbox b 10}} (mtSub) (mtSto)) (v*s  (numV 10) (aSto 1 (numV 10) (aSto 2 (boxV 1) (aSto 1 (exprV (num 7) (mtSub) (mtSto) '#&#f) (mtSto))))))

(test (run '{{fun {x} {+ 1 1}} {with {b {newbox 7}}
          {seqn {setbox b 10}
                     {openbox b}}}} (mtSub) (mtSto))  (v*s (numV 2) (aSto 1
  (exprV (app
    (fun 'b (seqn (setbox (id 'b) (num 10)) (openbox (id 'b))))
    (newbox (num 7)))
   (mtSub)
   (mtSto)
   '#&#f)
  (mtSto)))
)

(test (run '{{fun {x} {+ 1 x}} 10} (mtSub) (mtSto)) (v*s (numV 11) (aSto 1 (exprV (num 10) (mtSub) (mtSto) (box (numV 10))) (mtSto))))

(test (run '7 (mtSub) (mtSto)) (v*s (numV 7) (mtSto)))
(test (run '{+ 7 6} (mtSub) (mtSto)) (v*s (numV 13) (mtSto)))
(test (run '{newbox 1} (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (exprV (num 1) (mtSub) (mtSto) '#&#f) (mtSto))))
(test (run '{with {b {newbox {+ 2 3}}} {openbox b}} (mtSub) (mtSto)) (v*s (numV 5) (aSto 2
  (boxV 1) (aSto 1
   (exprV (add (num 2) (num 3)) (mtSub) (mtSto) (box (numV 5)))
   (mtSto))))
)
(test (run '{with {b {newbox 7}} {openbox b}} (mtSub) (mtSto)) (v*s
 (numV 7) (aSto 2 (boxV 1)  (aSto 1 (exprV (num 7) (mtSub) (mtSto) (box (numV 7))) (mtSto))))
)

(test (run '{with {b {newbox 7}}
          {seqn {setbox b 10}
                     {openbox b}}} (mtSub) (mtSto))
      (v*s (numV 10) (aSto 1 (numV 10) (aSto 2 (boxV 1)
   (aSto 1 (exprV (num 7) (mtSub) (mtSto) '#&#f) (mtSto)))))
)

(test (run '{with {b {newbox 7}}
          {seqn {openbox b}
                     {openbox b}}} (mtSub) (mtSto))
(v*s (numV 7) (aSto 2 (boxV 1)
  (aSto 1 (exprV (num 7) (mtSub) (mtSto) (box (numV 7))) (mtSto))))
)

(test (run '{+ {with {b {newbox 10}}
                    {seqn {setbox b 7}
                              {openbox b}}}     {with {b {newbox 10}}
                                                                  {seqn {setbox b 5}
                                                                  {openbox b}}}} (mtSub) (mtSto))
      (v*s (numV 12) (aSto 3 (numV 5) (aSto 4 (boxV 3) (aSto 3
    (exprV (num 10) (mtSub) (aSto 1 (numV 7) (aSto 2 (boxV 1)
       (aSto 1 (exprV (num 10) (mtSub) (mtSto) '#&#f) (mtSto))))
     '#&#f) (aSto 1 (numV 7) (aSto 2 (boxV 1) (aSto 1 (exprV (num 10) (mtSub) (mtSto) '#&#f) (mtSto))))))))
)


