(display (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(nl)

(defun curry1 (fun arg)
 (lambda (x)
  (apply fun (cons arg x))))

(setq! 2+ (curry1 + 2))

(write (2+ 3)) (nl)

(setq! union2
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
