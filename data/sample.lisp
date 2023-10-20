(display (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(nl)


(setq! union2q
 (lambda (lst1 lst2)
  (union2 memql? lst1 lst2)))

(setq! unionq
 (reduced union2q))


(display (unionq'(1 2 3 4) '(4 5 2 2) '(5 6 7 8)))
(nl)


(write (letrec
 ((factorial
   (lambda (n)
    (if (< n 2)
     1
     (* n (factorial (- n 1)))))))
 (factorial 5)))

(nl)
