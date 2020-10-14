#lang plai
(define-type DefrdSub
  [mtSub] ; Link(null)처럼 사용하는 것, cache에 저장된 것이 아무것도 없음 혹은 끝에 도달 했음을 알려줌 
  [aSub (name symbol?)
        (value number?)
        (saved DefredSub?)]
  )

(define (interp wae ds)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l ds) (interp r ds))]
    [sub (l r) (- (interp l ds) (interp r ds))]
    [with (i v e) (interp e (aSub i (interp v ds) ds))]
    [id (s) (lookup s ds)]
    [app (f a)
         (local
           [(define a_fundef (lookup-fundef f fundefs))]
           (interp (fundef-body a_fundef)
                          fundefs
                          (aSub (fundef-arg-name a_fundef)
                          (interp a fundefs)
                          (mtSub)
                   fundefs)
           )
         ]
    )
  )
    
     