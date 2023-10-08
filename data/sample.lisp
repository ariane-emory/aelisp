(setq transform!
      (lambda (pred fun lst)
        (cond 
          ((nil? lst) lst)
          ((pred (car lst)) 
           (rplaca lst (fun (car lst)))
           (transform! pred fun (cdr lst)))
          ((eq :CONS (type (car lst)))
           (transform! pred fun (car lst))
           (transform! pred fun (cdr lst)))
          (t (transform! pred fun (cdr lst))))
        lst))

(setq l '(2 (4  8)))

(transform!
 (lambda (x) (eq :INTEGER (type x)))
 (lambda (x) (* 2 x))
 l)

(print l)
