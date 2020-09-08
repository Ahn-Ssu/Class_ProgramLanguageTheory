#lang plai
(define (update-name target source theList)
  (cond
    [(empty? theList) theList]
    [(not (index-of theList target)) theList]
    [else
      (update-name target source (append (list-set theList (index-of theList target) (string-append target source))))
      ]
   ) 
  )

(test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
(test (update-name "ahnssu" " is nice" '("jc" "claire" "kate")) '("jc" "claire" "kate"))
(test (update-name "csee" " is my major" '("handong" "edu" "csee" "hisnet" "CSEE")) '("handong" "edu" "csee is my major" "hisnet" "CSEE"))
(test (update-name "jc" " is cool" '("jc" "jc" "kate" "jc" "ahnssu" "jc")) '("jc is cool" "jc is cool" "kate" "jc is cool" "ahnssu" "jc is cool"))
