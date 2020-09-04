#lang plai
;(cons 2 a) This typo is cuasing error!
(cons 2 3)       ; > '(2 . 3)
(cons 2 'a)       ; > '(2 . a)
(cons 'a 2)       ; > '(a . 2)
(cons 'a 'b)       ; > '(a . b)

(cons 'a empty)       ; > '(a)
(cons 2 empty)       ; > '(2)
(cons empty 'a)       ; > '(() . a)
(cons empty 2)       ; > '(() . 2)

(cons 'a null)       ; > '(a)
(cons 2 null)       ; > '(2)
(cons null 'a)       ; > '(() . a)
(cons null 2)       ; > '(() . a)
;(cons null)
;(cons 2) error -> expected : 2 / given : 1 

(rest (cons 2 null))
;(rest (cons null 2)) -> expected: (and/c list? (not/c empty?))
(+ 111111111111111111111111111110 1)
(list 2 3)       ; > '(2 3)
(list 2 'a)       ; > '(2 a)
(list 'a 2)       ; > '(a 2)
(list 'a 'b)       ; > '(a b)

(list 'a empty)       ; > '(a ())
(list 2 empty)       ; > '(2 ())
(list empty 'a)       ; > '(() a)
(list empty 2)       ; > '(() 2)

(list 'a null)       ; > '(a ())
(list 2 null)       ; > '(2 ())
(list null 2)       ; > '(() 2)
(list null 'a)       ; > '(() a)
(list 2)            ; > '(2)
(list '2)        ; > '(2)
(list 'a)       ; > '(a)
(list null)
'()

(rest (list null 2))
(rest (list 2 null))

(+ 111111111111111111111111111110 1)

(+ (first (list 1)) (first (list 1)))


(append (list 1 2 3 4) (list 5))