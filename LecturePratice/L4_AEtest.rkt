#lang plai
(define-type AE
  [num (n number?)]
  [add (ll number?)(rr number?)]
  [sub (ll number?)(rr number?)]
  )
(test (add (sub (num 3) (num 2)) (num 5)) 6) ; XXX ->  sub: contract violation