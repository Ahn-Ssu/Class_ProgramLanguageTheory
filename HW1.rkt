#lang plai
;Problems 1
;Solved by myself: Y
;Time taken: 5
;[contract]get-average:number number-> number
;[purpose]to compute the average of two numbers
;[tests](test (get-average 3 5) 4)
;       (test (get-average 95 83) 89)
;       (test (get-average 4 9) 6.5)
(define (get-average n1 n2)
  (/(+ n1 n2) 2))
(test (get-average 3 5) 4)
(test (get-average 95 83) 89)
(test (get-average 4 9) 6.5)

;Problems 2
;Solved by myself: Y
;Time taken: 7
;[contract] inchworm-travel: number -> number
;[purpose] to calculate the inchworm's travel lenght
;[tests](test (inchworm-travel 1) 2.54)
;       (test (inchworm-travel 33) 83.82 )
;       (test (inchworm-travel 3.3) 8.382)
(define (inchworm-travel hour)
  (* hour 2.54))
(test (inchworm-travel 1) 2.54)
(test (inchworm-travel 33) 83.82 )
(test (inchworm-travel 3.3) 8.382)

;Problems 3
;Solved by myself: Y
;Time taken: 5
;[contract] volume-cube: number -> number
;[purpose] to calculate volume of a cube which has all same length of side
;[tests](test (volume-cube 3) 27)
;       (test (volume-cube 77) 456533)
;       (test (volume-cube 9.4) 830.584)
(define (volume-cube sideLenght)
  (* sideLenght (* sideLenght sideLenght)))
(test (volume-cube 3) 27)
(test (volume-cube 77) 456533)
(test (volume-cube 9.4) 830.584)

;Problems 4
;Solved by myself: Y
;Time taken: 13
;[contract] my-BMI: number number -> number
;[purpose] to calculate one's BMI from weight and height
;[tests](test (my-BMI 83 190) 23)
;       (test (my-BMI 45 1.59) 18)
;       (test (my-BMI 65 1.71) 22)
(define (my-BMI weight height)
  (round (/ weight (* height height ))))
(test(my-BMI 83 1.9) 23)
(test (my-BMI 45 1.59) 18)
(test (my-BMI 65 1.71) 22)

;Problems 5
;Solved by myself: Y
;Time taken: 9.06
;[contract] fib: number -> list
;[purpose] to show fibonacci list that is lenght input number
;[tests](test (fib 0) (list ))
;       (test (fib 1) (list 1))
;       (test (fib 2) (list 1 1))
;       (test (fib 5 ) '(1 1 2 3 5))
(define (fib n)
  (cond
    [(< n 1)'()]
    [(= n 1) (list 1)]
    [(= n 2) (list 1 1)]
    [else (append (fib (- n 1)) (+ (first(indexing (fib (- n 2)) (- n 2))) (first(indexing (fib (- n 1)) (- n 1)))))]
    )
  )

(define (indexing targetList index)
  (cond
    [(= index 1) targetList]
    [else (indexing (rest targetList) (- index 1))]
    )
  )

(test (fib 0) (list ))
(test (fib 1) (list 1))
(test (fib 2) (list 1 1))
(test (fib 3) (list 1 1 2))
