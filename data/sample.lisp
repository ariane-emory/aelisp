(setq! ct 8)

(while (> ct 0)
 (write ct) (nl)
 (setq! ct (- ct 1)))

(write (filter (lambda (x) (not? (nil? x))) '(a nil b c nil d))) (nl)

(setq! lst  '(1 2 3 4))
(setq! lst2 '(6 7 8 9))

(princ "one:   ") (write (push-back! lst 5))    (nl)
(princ "two:   ") (write (push!      0   lst))  (nl)
(princ "three: ") (write (nconc!     lst lst2)) (nl)

(princ "four:  ") (write (mapconcat         (lambda (x) x) '("a" "b" "c") " "))  (nl)
(princ "five:  ") (write (apply mapconcat '((lambda (x) x) '("a" "b" "c") " "))) (nl)

(equal? '(1) '(1))

(setq! l '(1 2 (3 4)))
(transform! l integer? double)
(write l) (nl) ;; (2 4 (6 8))

(setq! l '(1 2 (3 4)))
(setq! new-l (transform l integer? double))
(write l) (nl)     ;; (1 2 (3 4)) remains unchanged
(write new-l) (nl) ;; (2 4 (6 8)) the transformed list

(setq! replicate-or-ignore
 (lambda (x)
  (if (integer? x)
   (list x x)
   nil)))

(setq! mylist '(1 "a" 2 3 "b" 4)) 
(write (mapcan replicate-or-ignore mylist)) (nl)
(write (mapcan replicate-or-ignore mylist)) (nl)
(write mylist) (nl)

(setq! lst1 '(1 2))
(setq! lst2 '(3 4))
(setq! lst3 '(5 6))

(write (nconc! lst1 lst2 lst3)) (nl)

(write lst1) (nl)

(nl)
(princ "state eval: ") (write (log-eval)) (nl)
(princ "state core: ") (write (log-core)) (nl)

(write (apply + '(1 2))) (nl)
(write (apply + 1 2 '(3 4))) (nl)
(write (apply + 1 2 '(* 3 4) '(5 6))) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (log-macro t)
;; (log-core  t)
;; (log-eval  t)

(write (zip '(1 2 3) '(a b c) '(7 8 9) '(x y z) '(p q r))) (nl)
(write (zip '(1 2 3) '(a b c) '(7 8 9) '(x y z))) (nl)
(write (zip '(1 2 3) '(a b c) '(7 8 9))) (nl)
(write (zip '(1 2 3) '(a b c))) (nl)
(write (zip '(1 2 3))) (nl)

(setq! x 66)
(write (list 22 44 x)) (nl)
(write #(22 44 x)) (nl)
