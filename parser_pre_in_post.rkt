#lang plai
;; prefix (op lhs rhs); (+ 3 4)
(define (prefixParse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and
      (= 3 (length sexp))
      (eq? (first sexp) '+)) ; op location -> first!
     (add (prefixParse (second sexp)) (prefixParse (third sexp)))]
    [(and
      (= 3 (length sexp))
      (eq? (first sexp) '-))
     (sub (prefixParse (second sexp)) (prefixParse (third sexp)))]
    [else (error 'prefixParse "bad syntax: ~a" sexp)]
    )
  )
;; infix (lhs op rhs); (3 + 4)
(define (infix sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and
      (= 3 (length sexp))
      (eq? (second sexp) '+)) ; op location -> second!
     (add (infix (first sexp)) (infix (third sexp)))]
    [(and
      (= 3 (length sexp))
      (eq? (second sexp) '-))
     (sub (infix (first sexp)) (infix (third sexp)))]
    [else (error 'infix "bad syntax: ~a" sexp)]
    )
  )
;; postfix (lhs rhs op); (3 4 +)
(define (postfixParse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and
      (= 3 (length sexp))
      (eq? (third sexp) '+)) ; op lcoation -> third!
     (add (postfixParse (first sexp)) (postfixParse (second sexp)))]
    [(and
      (= 3 (length sexp))
      (eq? (third sexp) '-))
     (sub (postfixParse (first sexp)) (postfixParse (second sexp)))]
    [else (error 'postfixParse "bad syntax: ~a" sexp)]
    )
  )