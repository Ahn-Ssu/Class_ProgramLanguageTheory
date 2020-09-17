#lang plai
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)]
  )

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(eq? (first sexp) '+)
     (add (parse (second sexp))
          (parse (third sexp)))]
    [(eq? (first sexp) '-)
     (sub (parse (second sexp))
          (parse (third sexp)))]
    ;[else (error 'parse "bad syntax: ~a" sexp)]
    )
  )

(test (parse '3) (num 3))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{+ {- 5 4} 5}) (add (sub (num 5) (num 4)) (num 5)))
(parse '{+ 3 4 5}) ; (add (num 3) (num 4))
(test (first '(- 1 2)) '-)