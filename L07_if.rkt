#lang plai
;(if test-exp then-expr else-expr)
; if (조건식) (수행식) (부정수행식)
(test (if (positive? -5) "Yes positive" "No, not Positive") "No, not Positive")
(test (if (positive? 5) "Yes positive" "No, not Positive") "Yes positive")
(test (if 'we-have-popUp-Quiz "yes" "no") "yes")

