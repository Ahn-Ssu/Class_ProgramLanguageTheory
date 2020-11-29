#lang plai
(parse '{with {x 3}{with {f {fun {y}{+ x y}}} {with {x 5} {f 4}}}})
(app (fun 'x (app (fun 'f(app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3))
;interp app (f a)
; f = (fun 'x(app (fun 'f(app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))))
; a = (num 3)
; ----------------------- Local
; f-val <- (interp f ds)
; interp f ds
; interp fun ( p b)
(closureV 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y)))) (mtSub))
; a-val <- (interp a ds)
; interp a ds
; interp num (n)
(numV 3)

; f-val = (closureV 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add  (id 'x) (id 'y)))) (mtSub))
; a-val = (numV 3)
; interp (closureV-body f-val) (aSub  (closureV-param f-val) a-val (closureV-ds f-val)))
(interp (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add  (id 'x) (id 'y))))
        (aSub 'x (numV 3) (mtSub))
        )

;interp app (f a)
; f = (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)))
; a = (fun 'y (add (id 'x) (id 'y)))
; ----------------------- Local
; f-val <- (interp f ds)
; interp f ds
; interp fun (p b)
(closureV 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)) (aSub 'x (numV 3) (mtSub)))

; a-val <- (interp a ds)
; interp a ds
; interp fun (p b)
(closureV 'y (add  (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))

; f-val = (closureV 'f (app (fun 'x (app (id 'f) (num 4))) (num 5)) (aSub 'x (numV 3) (mtSub)))
; a-val = (closureV 'y (add  (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
; interp (closureV-body f-val) (aSub  (closureV-param f-val) a-val (closureV-ds f-val)))
(interp (app (fun'x (app (id 'f) (num 4))) (num 5))
        (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
              (aSub 'x (numV 3) (mtSub)))
        )

;interp app ( f a)
; f = (fun 'x (app (id 'f) (num 4))
; a = (num 5)
; ----------------------- Local
; f-val <- (interp f ds)
; interp f ds
; interp fun (p b)
(closureV 'x (app (id 'f) (num 4)) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
                                         (aSub 'x (numV 3)
                                               (mtSub)))
; a-val <- (interp a ds)
; interp a ds
; interp num (n)
(numV 5)

; f-val = (closureV 'x (app (id 'f) (num 4)) (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
;                                         (aSub 'x (numV 3)
;                                               (mtSub)))
; a-val = (numV 5)
; interp (closureV-body f-val) (aSub  (closureV-param f-val) a-val (closureV-ds f-val)))
(interp (app (id 'f) (num 4)) (aSub 'x (numV 5)
                                    (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
                                         (aSub 'x (numV 3)
                                               (mtSub))))
        )
;interp app (f a)
; f = (id 'f)
; a = (num 4)
; ----------------------- Local
; f-val <- (interp f ds)
; interp f ds
; interp id (s), (lookup s ds)
(lookup 'f (aSub 'x (numV 5)
                                    (aSub 'f (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))
                                         (aSub 'x (numV 3)
                                               (mtSub)))))
;> 'f = (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtsub)))

; a-val <- (interp a ds)
; interp a ds
; interp num (n)
(numV 4)

; f-val = (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtsub)))
; a-val = (numV 4)
; interp (closureV-body f-val) (aSub  (closureV-param f-val) a-val (closureV-ds f-val)))
(interp (add (id 'x) (id 'y)) (aSub 'y (numV 4) (aSub 'x (numV 3) (mtSub))))

; 'x = (numV 3)
; 'y = (numV 4)
(num 7)