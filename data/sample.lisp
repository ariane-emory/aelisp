(display (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(nl)








(write (letrec
 ((factorial
   (lambda (n)
    (if (< n 2)
     1
     (* n (factorial (- n 1)))))))
 (factorial 5)))

(nl)
