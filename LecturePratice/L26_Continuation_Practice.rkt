#lang plai
(display (call/cc (λ(k) k))) ; #<continuation>



(let ([x (call/cc (λ(k) k))])
  (display x)
  (x (λ(ignore) "hi")))

(let ([x (call/cc (λ(k) k "hi"))])
  (display x)
  x)
