#lang plai

(test (length '{+ 3 4 5})  4)
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)]
  )

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (= 3 (length sexp)) (eq? (first sexp) '+))
     (add (parse (second sexp)) (parse (third sexp)))]
    [(and (= 3 (length sexp)) (eq? (first sexp) '-))
     (sub (parse (second sexp)) (parse third sexp))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    )
  )
(test (parse '3) (num 3))
(test (parse '(+ 3 4)) (add (num 3) (num 4)))
(test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
(test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")

(define (interp an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    )
  )