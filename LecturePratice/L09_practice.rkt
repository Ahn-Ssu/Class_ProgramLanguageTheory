#lang plai
(define-type DefrdSub
  [mtSub] ; Link(null)처럼 사용하는 것, cache에 저장된 것이 아무것도 없음 혹은 끝에 도달 했음을 알려줌 
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)]
  )
; [ x = 1 | y = 4 | k = 2 ]
(test (DefrdSub? (aSub 'x 1 (aSub 'y 4 (aSub 'k 2 (mtSub))))) #t)
(test (aSub? (aSub 'x 1 (aSub 'y 4 (aSub 'k 2 (mtSub))))) #t)
(test (mtSub? (aSub 'x 1 (aSub 'y 4 (aSub 'k 2 (mtSub))))) #f)
(test (mtSub? (mtSub)) #t)


; -------------------------------------------------------------
; 이전의 WAE type : id에서 free id를 처리함 
;(define (interp wae)
;  (type-case WAE wae 
;    [num (n) n]
;    [add (l r) (+ (interp l) (interp r))]
;    [sub (l r) (- (interp l) (interp r))]
;    [with (i v e) (interp (subst e i (interp v)))]
;    [id (s)     (error 'interp "free identifier ~a" s)]
;    )
;  )
; -------------------------------------------------------------

(define (interp wae ds)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l ds) (interp r ds))]
    [sub (l r) (- (interp l ds) (interp r ds))]
    [with (i v e) (interp e (aSub i (interp v ds) ds))]
    [id (s) (lookup s ds)]
    )
  )

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]
    )
  )

; -------------------------------------------------------------
; -------------------------------------------------------------
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
; -------------------------------------------------------------
; 이전의 WAE type : id에서 free id를 처리함 
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
; -------------------------------------------------------------
(define (interp f1wae fundefs ds)
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [with (x i b) (interp e fundefs (aSub i (interp v fundefs ds) ds))]
    [id (s) (lookup s ds)]
    [app (f a)
         (local
           [(define a-fundef (lookup-fundef f fundefs))]
           (interp (fundef-body a-fundef) ; 이 부분이 함수 몸체 -> body expression
                   fundefs
                   (aSub (fundef-arg-name a-fundef)
                         (interp a fundefs ds) ; a가 num value 일수도 있고 subst가 일어나야 할 수도 있기 때문에 rec
                         (mtSub))) 
           )
         ]
    )
  )
