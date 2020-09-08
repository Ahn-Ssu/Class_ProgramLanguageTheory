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
;Time taken: 50
;[contract] fib: number -> list
;[purpose] to show fibonacci list that is lenght input number
;[tests](test (fib 0) (list ))
;       (test (fib 1) (list 1))
;       (test (fib 2) (list 1 1))
;       (test (fib 3) (list 1 1 2))
;       (test (fib 4) (list 1 1 2 3))
;       (test (fib 7) (list 1 1 2 3 5 8 13))
(define (fib n)
  (cond
    [(< n 1)'()]
    [(= n 1) (list 1)]
    [(= n 2) (list 1 1)]
    [else (append (fib (- n 1)) (list(+ (first(indexing (fib (- n 2)) (- n 2))) (first(indexing (fib (- n 1)) (- n 1))))))]
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
(test (fib 4) (list 1 1 2 3))
(test (fib 7) (list 1 1 2 3 5 8 13))

;Problems 6-a
;Solved by myself: Y 
;Time taken: 3
(define-type Vehicle
  [Bicycle (wheels integer?)]
  [Car (wheels integer?)
       (windows integer?)]
  [Airplane (wheels integer?)
            (windows integer?)
            (engines integer?)]
  )

;Problems 6-b
;Solved by myself: Y 
;Time taken: 6
;[contract] vehicle-tax: Vehicle -> number
;[purpose] to count the total amount of tax that should be paid the Vehicle
;[tests](test (vehicle-tax (Airplane 5 5 5))15)
;       (test (vehicle-tax (Bicycle 3)) 3)
;       (test (vehicle-tax (Car 4 2)) 6)
(define (vehicle-tax theVehicle)
  (type-case Vehicle theVehicle
    [Bicycle (wheels) wheels]
    [Car (wheels windows) (+ wheels windows)]
    [Airplane (wheels windows engines) (+ wheels (+ windows engines))]
    )
  )
(test (vehicle-tax (Airplane 5 5 5))15)
(test (vehicle-tax (Bicycle 3)) 3)
(test (vehicle-tax (Car 4 2)) 6)

;Problems 6-c
;Solved by myself: Y 
;Time taken: 11
;[contract] vehicle-tax: Vehicle -> string
;[purpose] to test safety of the Vehicle  
;[tests](test (is-vehicle-safe (Airplane 8 80 4)) "safe")
;       (test (is-vehicle-safe (Airplane 5 5 5)) "unsafe")
;       (test (is-vehicle-safe (Car 4 3)) "safe")
;       (test (is-vehicle-safe (Car 2 2)) "unsafe")
;       (test (is-vehicle-safe (Bicycle 1)) "safe")
;       (test (is-vehicle-safe (Bicycle 5)) "unsafe")
(define (is-vehicle-safe theVehicle)
  (cond
   {(type-case Vehicle theVehicle
    [Bicycle (wheels) (<= wheels 4)]
    [Car (wheels windows) (and (>= wheels 3) (>= windows 2))]
    [Airplane (wheels windows engines) (and (>= wheels 2) (and (>= windows 10) (>= engines 1)))]
    ) "safe"}
   {else "unsafe"}
   )
  )

(test (is-vehicle-safe (Airplane 8 80 4)) "safe")
(test (is-vehicle-safe (Airplane 5 5 5)) "unsafe")
(test (is-vehicle-safe (Car 4 3)) "safe")
(test (is-vehicle-safe (Car 2 2)) "unsafe")
(test (is-vehicle-safe (Bicycle 1)) "safe")
(test (is-vehicle-safe (Bicycle 5)) "unsafe")


;Problems 7
;Solved by myself: Y 
;Time taken: 22
;[contract] update-name: string string list -> list
;[purpose] to find and replace target in the list after concat target and source 
;[tests](test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
;       (test (update-name "ahnssu" " is nice" '("jc" "claire" "kate")) '("jc" "claire" "kate"))
;       (test (update-name "csee" " is my major" '("handong" "edu" "csee" "hisnet" "CSEE")) '("handong" "edu" "csee is my major" "hisnet" "CSEE"))
(define (update-name target source theList)
  (cond
    [(empty? theList) theList]
    [(not (index-of theList target)) theList]
    [(list-set theList (index-of theList target) (string-append target source))]
   )
  )
(test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
(test (update-name "ahnssu" " is nice" '("jc" "claire" "kate")) '("jc" "claire" "kate"))
(test (update-name "csee" " is my major" '("handong" "edu" "csee" "hisnet" "CSEE")) '("handong" "edu" "csee is my major" "hisnet" "CSEE"))

;Problems 8
;Solved by myself: Y 
;Time taken: 120
;[contract] binary-search: list -> list
;[purpose] to show traversal path of binary search
;[tests](test (binary-search '() 3) "the list is empty")
;       (test (binary-search '(1 2 3) 4) "the target is not exist")
;       (test (binary-search '(1) 1) '(1))
;       (test (binary-search '(1 2 3) 2) '(2))
;       (test (binary-search '(1 2 3) 3) '(2 3))
;       (test (binary-search '(1 2 3 4) 2) '(2))
;       (test (binary-search '(1 2 3 4) 4) '(2 3 4))
;       (test (binary-search '(1 2 3 4 5 6 7 8) 8) '(4 6 7 8))
;       (test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
;       (test (binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
(define (binary-search lst target)
  (cond
    [(empty? lst) "the list is empty"]
    [(not (index-of lst target)) "the target is not exist"]
    [(= (length lst) 1) lst]
    [(= target (list-ref lst (- (ceiling (/ (length lst) 2)) 1)))
     (list (list-ref lst (- (ceiling (/ (length lst) 2)) 1)))]

    [(> target (list-ref lst (- (ceiling (/ (length lst) 2)) 1)))
        (append
        (list (list-ref lst (- (ceiling (/ (length lst) 2)) 1)))
         (binary-search (rest (member (list-ref lst (- (ceiling (/ (length lst) 2)) 1)) lst)) target)
         )
        ]
    [(< target (list-ref lst (- (ceiling (/ (length lst) 2)) 1)))
        (append
        (list (list-ref lst (- (ceiling (/ (length lst) 2)) 1)))
         (binary-search (take lst (- (ceiling (/ (length lst) 2)) 1)) target)
         )
        ]
    )
  )

(test (binary-search '() 3) "the list is empty")
(test (binary-search '(1 2 3) 4) "the target is not exist")
(test (binary-search '(1) 1) '(1))
(test (binary-search '(1 2 3) 2) '(2))
(test (binary-search '(1 2 3) 3) '(2 3))
(test (binary-search '(1 2 3 4) 2) '(2))
(test (binary-search '(1 2 3 4) 4) '(2 3 4))
(test (binary-search '(1 2 3 4 5 6 7 8) 8) '(4 6 7 8))
(test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
(test (binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
 

