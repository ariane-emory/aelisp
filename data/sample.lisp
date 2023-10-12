(setq! odd?
 (lambda (n) 
  (not (== 0 (% n 2)))))

;; (filter odd? '(1 2 3 4 5 6 7 8 9 10))   ; Expected result: (1 3 5 7 9)

;; (write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d)))   ; Expected result: (a b c d)

(setq! lst '(1 2 3 4))
(setq! lst2 '(6 7 8 9))
(setq! lst3 '(10 11 12 13))

(setq! last
  (lambda (lst)
    (if (or (nil? lst) (nil? (cdr lst)))
        lst
        (last (cdr lst)))))

(setq! nconc!
  (lambda (lists)
    (nconc-helper! (car lists) (cdr lists))))

(setq! nconc-helper!
  (lambda (result remaining)
    (princ "Inside nconc-helper!, result: ") (write result) (nl)
    (princ "remaining: ") (write remaining) (nl)
    (if (nil? remaining)
        result
        (nconc-helper! (nconc2! result (car remaining)) (cdr remaining)))))

(setq! nconc2!
  (lambda (lst1 lst2)
    (princ "Inside nconc2!, lst1: ") (write lst1) (nl)
    (princ "lst2: ") (write lst2) (nl)
    (if (nil? lst1)
        lst2
        (rplacd! (last lst1) lst2)
        lst1)))

(princ "one:   ") (write (push-back! lst 5))         (nl)
(princ "two:   ") (write (push!      0   lst))       (nl)
(princ "three: ") (write (nconc!     lst lst2 lst3)) (nl)

(while (t)
 (print "hello")
 (nl)
 (sleep 1000))
