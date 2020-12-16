(app
 (fun 'fac (app (id 'fac) (num 1)))
 (app 
 (fun 'facX (app (id 'facX) (id 'facX)))
 (fun
  'facY 
  (fun 
  'n 
  (if0 
   (id 'n)
   (num 1) 
   (mul 
    (id 'n) 
    (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))))))

>(interp
  (app
   (fun 'fac (app (id 'fac) (num 1)))
   (app
    (fun 'facX (app (id 'facX) (id 'facX)))
    (fun
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul
        (id 'n)
        (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))))))
  (mtSub))
> (interp (fun 'fac (app (id 'fac) (num 1))) (mtSub))
< (closureV 'fac (app (id 'fac) (num 1)) (mtSub))
> (interp
   (app
    (fun 'facX (app (id 'facX) (id 'facX)))
    (fun
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul
        (id 'n)
        (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))))
   (mtSub))
> >(interp (fun 'facX (app (id 'facX) (id 'facX))) (mtSub))
< <(closureV 'facX (app (id 'facX) (id 'facX)) (mtSub))
> >(interp
    (fun
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))))
    (mtSub))
< <(closureV
    'facY
    (fun
     'n
     (if0
      (id 'n)
      (num 1)
      (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
    (mtSub))
> (interp
   (app (id 'facX) (id 'facX))
   (aSub
    'facX
    (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
    (mtSub)))
> >(interp
    (id 'facX)
    (aSub
     'facX
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub)))
< <(closureV
    'facY
    (fun
     'n
     (if0
      (id 'n)
      (num 1)
      (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
    (mtSub))
> >(interp
    (id 'facX)
    (aSub
     'facX
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub)))
< <(closureV
    'facY
    (fun
     'n
     (if0
      (id 'n)
      (num 1)
      (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
    (mtSub))
> (interp
   (fun
    'n
    (if0
     (id 'n)
     (num 1)
     (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
   (aSub
    'facY
    (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
    (mtSub)))
< (closureV
   'n
   (if0
    (id 'n)
    (num 1)
    (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
   (aSub
    'facY
    (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
    (mtSub)))
>(interp
  (app (id 'fac) (num 1))
  (aSub
   'fac
   (closureV
    'n
    (if0
     (id 'n)
     (num 1)
     (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub)))
   (mtSub)))
> (interp
   (id 'fac)
   (aSub
    'fac
    (closureV
     'n
     (if0
      (id 'n)
      (num 1)
      (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
     (aSub
      'facY
      (closureV
       'facY
       (fun
        'n
        (if0
         (id 'n)
         (num 1)
         (mul
          (id 'n)
          (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
       (mtSub))
      (mtSub)))
    (mtSub)))
< (closureV
   'n
   (if0
    (id 'n)
    (num 1)
    (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
   (aSub
    'facY
    (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
    (mtSub)))
> (interp
   (num 1)
   (aSub
    'fac
    (closureV
     'n
     (if0
      (id 'n)
      (num 1)
      (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
     (aSub
      'facY
      (closureV
       'facY
       (fun
        'n
        (if0
         (id 'n)
         (num 1)
         (mul
          (id 'n)
          (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
       (mtSub))
      (mtSub)))
    (mtSub)))
< (numV 1)
>(interp
  (if0
   (id 'n)
   (num 1)
   (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
  (aSub
   'n
   (numV 1)
   (aSub
    'facY
    (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
    (mtSub))))
> (interp
   (id 'n)
   (aSub
    'n
    (numV 1)
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub))))
< (numV 1)
>(interp
  (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))
  (aSub
   'n
   (numV 1)
   (aSub
    'facY
    (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
    (mtSub))))
> (interp
   (id 'n)
   (aSub
    'n
    (numV 1)
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub))))
< (numV 1)
> (interp
   (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))
   (aSub
    'n
    (numV 1)
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub))))
> >(interp
    (app (id 'facY) (id 'facY))
    (aSub
     'n
     (numV 1)
     (aSub
      'facY
      (closureV
       'facY
       (fun
        'n
        (if0
         (id 'n)
         (num 1)
         (mul
          (id 'n)
          (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
       (mtSub))
      (mtSub))))
> > (interp
     (id 'facY)
     (aSub
      'n
      (numV 1)
      (aSub
       'facY
       (closureV
        'facY
        (fun
         'n
         (if0
          (id 'n)
          (num 1)
          (mul
           (id 'n)
           (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
        (mtSub))
       (mtSub))))
< < (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
> > (interp
     (id 'facY)
     (aSub
      'n
      (numV 1)
      (aSub
       'facY
       (closureV
        'facY
        (fun
         'n
         (if0
          (id 'n)
          (num 1)
          (mul
           (id 'n)
           (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
        (mtSub))
       (mtSub))))
< < (closureV
     'facY
     (fun
      'n
      (if0
       (id 'n)
       (num 1)
       (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
     (mtSub))
> >(interp
    (fun
     'n
     (if0
      (id 'n)
      (num 1)
      (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub)))
< <(closureV
    'n
    (if0
     (id 'n)
     (num 1)
     (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub)))
> >(interp
    (sub (id 'n) (num 1))
    (aSub
     'n
     (numV 1)
     (aSub
      'facY
      (closureV
       'facY
       (fun
        'n
        (if0
         (id 'n)
         (num 1)
         (mul
          (id 'n)
          (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
       (mtSub))
      (mtSub))))
> > (interp
     (id 'n)
     (aSub
      'n
      (numV 1)
      (aSub
       'facY
       (closureV
        'facY
        (fun
         'n
         (if0
          (id 'n)
          (num 1)
          (mul
           (id 'n)
           (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
        (mtSub))
       (mtSub))))
< < (numV 1)
> > (interp
     (num 1)
     (aSub
      'n
      (numV 1)
      (aSub
       'facY
       (closureV
        'facY
        (fun
         'n
         (if0
          (id 'n)
          (num 1)
          (mul
           (id 'n)
           (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
        (mtSub))
       (mtSub))))
< < (numV 1)
< <(numV 0)
> (interp
   (if0
    (id 'n)
    (num 1)
    (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1)))))
   (aSub
    'n
    (numV 0)
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub))))
> >(interp
    (id 'n)
    (aSub
     'n
     (numV 0)
     (aSub
      'facY
      (closureV
       'facY
       (fun
        'n
        (if0
         (id 'n)
         (num 1)
         (mul
          (id 'n)
          (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
       (mtSub))
      (mtSub))))
< <(numV 0)
> (interp
   (num 1)
   (aSub
    'n
    (numV 0)
    (aSub
     'facY
     (closureV
      'facY
      (fun
       'n
       (if0
        (id 'n)
        (num 1)
        (mul (id 'n) (app (app (id 'facY) (id 'facY)) (sub (id 'n) (num 1))))))
      (mtSub))
     (mtSub))))
< (numV 1)
<(numV 1)
(numV 1)