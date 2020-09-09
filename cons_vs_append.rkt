#lang plai
;cons : (-> (A : Type) (a : A) (-> (List A) (List A))) construct 
;첨언 - a가 A가 될수 있는 경우에 concat?? 
(test (cons 1 empty) '(1))
(test (cons 1 null) '(1))
(test (append 1 empty) ('1) ); -> expection, list?

(test (cons empty 1) '(() . 1) )
(test (rest (cons null 1)) '()  ); -> expection, list??
(test (append empty 1) 1)

(test (cons '(1 2 3) empty) '((1 2 3)) )
(test (append '(1 2 3) empty) '(1 2 3))

(test (cons empty '(1 2 3)) '(() 1 2 3) )
(test (append empty '(1 2 3)) '(1 2 3) )

(test (cons '(1 2 3) '(4 5 6)) '((1 2 3) 4 5 6))
(test (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(test (list? '(()) ) #t) ; empty가 선행인자일때 역할

; 리스트는 null이거나 두번째 값이 list인 pair이다 
; 콘스 정리, i) 뒤에 있는 element가 list인 경우 뒤에 있는 elements를 모두 앞에 있는 elements를 포함하여 새로운 리스트를 만들어 냄
; (cons empty '(1 2 3)) -> '( empty 1 2 3 ) -> '(() 1 2 3)
; ii) 뒤에 있는 element가 list가 아니면 pair를 생성함 (x . y)
; (cons 1 empty) -> '(1 empty) -> '(1)
; (cons empty 1) -> '(() . 1)
; iii) empty와 null은 동일한 역할이며, list로 취급됨 (list 상수)