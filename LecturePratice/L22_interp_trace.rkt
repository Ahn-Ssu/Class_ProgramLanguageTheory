
'(app (fun 'swap (app (fun 'a (app (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))) (num 20))) (num 10))) 
      (fun 'x (fun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))))

>(interp
  (app
   (fun
    'swap
    (app
     (fun
      'a
      (app
       (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
       (num 20)))
     (num 10)))
   (fun
    'x
    (fun
     'y
     (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))))
  (mtSub)
  (mtSto))
> (interp
   (fun
    'swap
    (app
     (fun
      'a
      (app
       (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
       (num 20)))
     (num 10)))
   (mtSub)
   (mtSto))
< (v*s
   (closureV
    'swap
    (app
     (fun
      'a
      (app
       (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
       (num 20)))
     (num 10))
    (mtSub))
   (mtSto))
> (interp
   (fun
    'x
    (fun
     'y
     (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))))
   (mtSub)
   (mtSto))
< (v*s
   (closureV
    'x
    (fun
     'y
     (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
    (mtSub))
   (mtSto))
>(interp
  (app
   (fun
    'a
    (app
     (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
     (num 20)))
   (num 10))
  (aSub 'swap 0 (mtSub))
  (aSto
   0
   (closureV
    'x
    (fun
     'y
     (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
    (mtSub))
   (mtSto)))
> (interp
   (fun
    'a
    (app
     (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
     (num 20)))
   (aSub 'swap 0 (mtSub))
   (aSto
    0
    (closureV
     'x
     (fun
      'y
      (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
     (mtSub))
    (mtSto)))
< (v*s
   (closureV
    'a
    (app
     (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
     (num 20))
    (aSub 'swap 0 (mtSub)))
   (aSto
    0
    (closureV
     'x
     (fun
      'y
      (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
     (mtSub))
    (mtSto)))
> (interp
   (num 10)
   (aSub 'swap 0 (mtSub))
   (aSto
    0
    (closureV
     'x
     (fun
      'y
      (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
     (mtSub))
    (mtSto)))
< (v*s
   (numV 10)
   (aSto
    0
    (closureV
     'x
     (fun
      'y
      (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
     (mtSub))
    (mtSto)))
>(interp
  (app (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))) (num 20))
  (aSub 'a 1 (aSub 'swap 0 (mtSub)))
  (aSto
   1
   (numV 10)
   (aSto
    0
    (closureV
     'x
     (fun
      'y
      (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
     (mtSub))
    (mtSto))))
> (interp
   (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a)))
   (aSub 'a 1 (aSub 'swap 0 (mtSub)))
   (aSto
    1
    (numV 10)
    (aSto
     0
     (closureV
      'x
      (fun
       'y
       (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
      (mtSub))
     (mtSto))))
< (v*s
   (closureV
    'b
    (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))
    (aSub 'a 1 (aSub 'swap 0 (mtSub))))
   (aSto
    1
    (numV 10)
    (aSto
     0
     (closureV
      'x
      (fun
       'y
       (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
      (mtSub))
     (mtSto))))
> (interp
   (num 20)
   (aSub 'a 1 (aSub 'swap 0 (mtSub)))
   (aSto
    1
    (numV 10)
    (aSto
     0
     (closureV
      'x
      (fun
       'y
       (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
      (mtSub))
     (mtSto))))
< (v*s
   (numV 20)
   (aSto
    1
    (numV 10)
    (aSto
     0
     (closureV
      'x
      (fun
       'y
       (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
      (mtSub))
     (mtSto))))
>(interp
  (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))
  (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
  (aSto
   2
   (numV 20)
   (aSto
    1
    (numV 10)
    (aSto
     0
     (closureV
      'x
      (fun
       'y
       (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
      (mtSub))
     (mtSto)))))
> (interp
   (app (app (id 'swap) (id 'a)) (id 'b))
   (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
   (aSto
    2
    (numV 20)
    (aSto
     1
     (numV 10)
     (aSto
      0
      (closureV
       'x
       (fun
        'y
        (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
       (mtSub))
      (mtSto)))))
> >(interp
    (app (id 'swap) (id 'a))
    (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
    (aSto
     2
     (numV 20)
     (aSto
      1
      (numV 10)
      (aSto
       0
       (closureV
        'x
        (fun
         'y
         (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
        (mtSub))
       (mtSto)))))
> > (interp
     (id 'swap)
     (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto)))))
< < (v*s
     (closureV
      'x
      (fun
       'y
       (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
      (mtSub))
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto)))))
> > (interp
     (id 'a)
     (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto)))))
< < (v*s
     (numV 10)
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto)))))
> >(interp
    (fun
     'y
     (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))
    (aSub 'x 3 (mtSub))
    (aSto
     3
     (numV 10)
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto))))))
< <(v*s
    (closureV
     'y
     (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))
     (aSub 'x 3 (mtSub)))
    (aSto
     3
     (numV 10)
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto))))))
> >(interp
    (id 'b)
    (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
    (aSto
     3
     (numV 10)
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto))))))
< <(v*s
    (numV 20)
    (aSto
     3
     (numV 10)
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto))))))
> (interp
   (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))
   (aSub 'y 4 (aSub 'x 3 (mtSub)))
   (aSto
    4
    (numV 20)
    (aSto
     3
     (numV 10)
     (aSto
      2
      (numV 20)
      (aSto
       1
       (numV 10)
       (aSto
        0
        (closureV
         'x
         (fun
          'y
          (app
           (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
           (id 'x)))
         (mtSub))
        (mtSto)))))))
> >(interp
    (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
    (aSub 'y 4 (aSub 'x 3 (mtSub)))
    (aSto
     4
     (numV 20)
     (aSto
      3
      (numV 10)
      (aSto
       2
       (numV 20)
       (aSto
        1
        (numV 10)
        (aSto
         0
         (closureV
          'x
          (fun
           'y
           (app
            (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
            (id 'x)))
          (mtSub))
         (mtSto)))))))
< <(v*s
    (closureV
     'z
     (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))
     (aSub 'y 4 (aSub 'x 3 (mtSub))))
    (aSto
     4
     (numV 20)
     (aSto
      3
      (numV 10)
      (aSto
       2
       (numV 20)
       (aSto
        1
        (numV 10)
        (aSto
         0
         (closureV
          'x
          (fun
           'y
           (app
            (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
            (id 'x)))
          (mtSub))
         (mtSto)))))))
> >(interp
    (id 'x)
    (aSub 'y 4 (aSub 'x 3 (mtSub)))
    (aSto
     4
     (numV 20)
     (aSto
      3
      (numV 10)
      (aSto
       2
       (numV 20)
       (aSto
        1
        (numV 10)
        (aSto
         0
         (closureV
          'x
          (fun
           'y
           (app
            (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
            (id 'x)))
          (mtSub))
         (mtSto)))))))
< <(v*s
    (numV 10)
    (aSto
     4
     (numV 20)
     (aSto
      3
      (numV 10)
      (aSto
       2
       (numV 20)
       (aSto
        1
        (numV 10)
        (aSto
         0
         (closureV
          'x
          (fun
           'y
           (app
            (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
            (id 'x)))
          (mtSub))
         (mtSto)))))))
> (interp
   (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))
   (aSub 'z 5 (aSub 'y 4 (aSub 'x 3 (mtSub))))
   (aSto
    5
    (numV 10)
    (aSto
     4
     (numV 20)
     (aSto
      3
      (numV 10)
      (aSto
       2
       (numV 20)
       (aSto
        1
        (numV 10)
        (aSto
         0
         (closureV
          'x
          (fun
           'y
           (app
            (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
            (id 'x)))
          (mtSub))
         (mtSto))))))))
> >(interp
    (setvar 'x (id 'y))
    (aSub 'z 5 (aSub 'y 4 (aSub 'x 3 (mtSub))))
    (aSto
     5
     (numV 10)
     (aSto
      4
      (numV 20)
      (aSto
       3
       (numV 10)
       (aSto
        2
        (numV 20)
        (aSto
         1
         (numV 10)
         (aSto
          0
          (closureV
           'x
           (fun
            'y
            (app
             (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
             (id 'x)))
           (mtSub))
          (mtSto))))))))
> > (interp
     (id 'y)
     (aSub 'z 5 (aSub 'y 4 (aSub 'x 3 (mtSub))))
     (aSto
      5
      (numV 10)
      (aSto
       4
       (numV 20)
       (aSto
        3
        (numV 10)
        (aSto
         2
         (numV 20)
         (aSto
          1
          (numV 10)
          (aSto
           0
           (closureV
            'x
            (fun
             'y
             (app
              (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
              (id 'x)))
            (mtSub))
           (mtSto))))))))
< < (v*s
     (numV 20)
     (aSto
      5
      (numV 10)
      (aSto
       4
       (numV 20)
       (aSto
        3
        (numV 10)
        (aSto
         2
         (numV 20)
         (aSto
          1
          (numV 10)
          (aSto
           0
           (closureV
            'x
            (fun
             'y
             (app
              (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
              (id 'x)))
            (mtSub))
           (mtSto))))))))
< <(v*s
    (numV 20)
    (aSto
     3
     (numV 20)
     (aSto
      5
      (numV 10)
      (aSto
       4
       (numV 20)
       (aSto
        3
        (numV 10)
        (aSto
         2
         (numV 20)
         (aSto
          1
          (numV 10)
          (aSto
           0
           (closureV
            'x
            (fun
             'y
             (app
              (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
              (id 'x)))
            (mtSub))
           (mtSto)))))))))
> (interp
   (setvar 'y (id 'z))
   (aSub 'z 5 (aSub 'y 4 (aSub 'x 3 (mtSub))))
   (aSto
    3
    (numV 20)
    (aSto
     5
     (numV 10)
     (aSto
      4
      (numV 20)
      (aSto
       3
       (numV 10)
       (aSto
        2
        (numV 20)
        (aSto
         1
         (numV 10)
         (aSto
          0
          (closureV
           'x
           (fun
            'y
            (app
             (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
             (id 'x)))
           (mtSub))
          (mtSto)))))))))
> >(interp
    (id 'z)
    (aSub 'z 5 (aSub 'y 4 (aSub 'x 3 (mtSub))))
    (aSto
     3
     (numV 20)
     (aSto
      5
      (numV 10)
      (aSto
       4
       (numV 20)
       (aSto
        3
        (numV 10)
        (aSto
         2
         (numV 20)
         (aSto
          1
          (numV 10)
          (aSto
           0
           (closureV
            'x
            (fun
             'y
             (app
              (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
              (id 'x)))
            (mtSub))
           (mtSto)))))))))
< <(v*s
    (numV 10)
    (aSto
     3
     (numV 20)
     (aSto
      5
      (numV 10)
      (aSto
       4
       (numV 20)
       (aSto
        3
        (numV 10)
        (aSto
         2
         (numV 20)
         (aSto
          1
          (numV 10)
          (aSto
           0
           (closureV
            'x
            (fun
             'y
             (app
              (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
              (id 'x)))
            (mtSub))
           (mtSto)))))))))
< (v*s
   (numV 10)
   (aSto
    4
    (numV 10)
    (aSto
     3
     (numV 20)
     (aSto
      5
      (numV 10)
      (aSto
       4
       (numV 20)
       (aSto
        3
        (numV 10)
        (aSto
         2
         (numV 20)
         (aSto
          1
          (numV 10)
          (aSto
           0
           (closureV
            'x
            (fun
             'y
             (app
              (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
              (id 'x)))
            (mtSub))
           (mtSto))))))))))
>(interp
  (id 'a)
  (aSub 'b 2 (aSub 'a 1 (aSub 'swap 0 (mtSub))))
  (aSto
   4
   (numV 10)
   (aSto
    3
    (numV 20)
    (aSto
     5
     (numV 10)
     (aSto
      4
      (numV 20)
      (aSto
       3
       (numV 10)
       (aSto
        2
        (numV 20)
        (aSto
         1
         (numV 10)
         (aSto
          0
          (closureV
           'x
           (fun
            'y
            (app
             (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
             (id 'x)))
           (mtSub))
          (mtSto))))))))))
<(v*s
  (numV 10)
  (aSto
   4
   (numV 10)
   (aSto
    3
    (numV 20)
    (aSto
     5
     (numV 10)
     (aSto
      4
      (numV 20)
      (aSto
       3
       (numV 10)
       (aSto
        2
        (numV 20)
        (aSto
         1
         (numV 10)
         (aSto
          0
          (closureV
           'x
           (fun
            'y
            (app
             (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z))))
             (id 'x)))
           (mtSub))
          (mtSto))))))))))
(v*s
 (numV 10)
 (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 5 (numV 10) (aSto 4 (numV 20) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 1 (numV 10) (aSto 0 (closureV 'x (fun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto))))))))))
> 