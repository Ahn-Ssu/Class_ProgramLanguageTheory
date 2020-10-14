#lang plai
(define-type Vehicle
  [Bicycle (wheels (and (integer?) (not (negative?))))]
  [Car (wheels integer?)
       (windows integer?)]
  [Airplane (wheels integer?)
            (windows integer?)
            (engines integer?)]
  )


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
