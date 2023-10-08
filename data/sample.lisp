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

(setq l '(2 (4 8)))
(transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) l)
(print l) ;; sucessfully prints (4 (8 16)).

(setq l (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) '(2 (4 8))))
(print l) ;; successfully prints (4 (8 16)).

(setq l '(2 (4 . 8)))
(transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) l)
(print l) ;; fails to print (4 (8 . 16))!

(setq l (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) '(2 (4 . 8))))
(print l) ;; fails to print (4 (8 . 16))!
