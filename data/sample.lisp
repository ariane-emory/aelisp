(setq! ct 8)

(while (> ct 0)
  (write ct) (nl)
  (setq! ct (- ct 1)))

o;; (filter odd? '(1 2 3 4 5 6 7 8 9 10))   ; Expected result: (1 3 5 7 9)

;; (write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d)))   ; Expected result: (a b c d)
;; (princ "one:   ") (write (push-back! lst 5))    (nl)
;; (princ "two:   ") (write (push!      0   lst))  (nl)
;; (princ "three: ") (write (nconc!     lst lst2)) (nl)

;; I added while and apply, they seem to work:


;; (write (apply + '(1 2 3)))

;; ;; I'm trying this nconc!:


(setq! lst '(1 2))
(setq! lst2 '(3 4))
(setq! lst3 '(5 6))

(write (nconc! lst lst2 lst3))

(write lst) (nl)

(write (mapconcat (lambda (x) x) '("a" "b" "c") " ")) (nl)
(write (apply mapconcat '((lambda (x) x) '("a" "b" "c") " "))) (nl)




;; (write (equal? '(a (1 2)) '(a (1 2)))) (nl)

;;(write ) (nl)

(equal? '(1) '(1))


(setq! transform!
 (lambda (obj pred fun)
  (if (atom? obj)
   (error "obj must be a list")
   (cond
    ((pred obj) (set! obj (fun obj)))
    ((cons? obj)
     (let ((head (car obj))
           (tail (cdr obj)))
      (cond
       ((pred head)  (rplaca! obj (fun head)))
       ((cons? head) (transform! head pred fun)))
      (cond
       ((pred tail)  (rplacd! obj (fun tail)))
       ((cons? tail) (rplacd! obj (transform! tail pred fun))))))
    (t obj))
   obj)))

(setq! l '(1 2 (3 4)))
(transform! l integer? double)
(write l) (nl) ;; (2 4 (6 8))
