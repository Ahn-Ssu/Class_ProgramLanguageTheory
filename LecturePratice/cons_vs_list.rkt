#lang plai

(test (cons 1 empty) '(1))
(test (list (1 empty)) '(1) ); -> expection, not a procedure
(test (list '(1 empty)) '((1 empty)) )
(test (list 1 empty) '(1 ()) )

(test (cons empty 1) '( () . 1) )
(test (list (empty 1)) '(1)); -> expection, not a procedure
(test (list empty 1) '( () 1) )

(test (cons '(1 2 3) empty) '((1 2 3)) )
(test (list ('(1 2 3) empty)) '(1 2 3)); -> expection, not a procedure
(test (list '(1 2 3) empty) '((1 2 3) ()))

(test (cons empty '(1 2 3)) '(() 1 2 3) )
(test (list (empty '(1 2 3))) '(1 2 3) ); -> expection, not a procedure
(test (list empty '(1 2 3)) '(() (1 2 3)))

(test (cons '(1 2 3) '(4 5 6)) '((1 2 3) 4 5 6))
(test (list('(1 2 3) '(4 5 6))) '(1 2 3 4 5 6)); -> expection, not a procedure 
(test (list '(1 2 3) '(4 5 6)) '((1 2 3) (4 5 6)))
(test (list '(1 2 3) empty '(4 5 6)) '((1 2 3) () (4 5 6)))

(test (list empty) '(()))
(test (list) '())
(test (list empty (cons 1 2) '(l i s t)) '(() (1 . 2) (l i s t)) )
(test (list? '()) #t)

; 리스트는 null이거나 두번째 값이 list인 pair이다 
; 리스트 정리
; 리스트는 (list x1, x2, x3, ..., xn)의 요소들을 '(x1, x2, x3, ..., xn) 으로 리턴해준다.
; x1은 아무거나 들어올 수 있다. 심지어 리스트에 리스트가 들어올 수 있음 ( 그 머고, 파이썬 리스트 중첩처럼) 
