(setq! odd?
 (lambda (n) 
  (not (== 0 (% n 2)))))

;; (filter odd? '(1 2 3 4 5 6 7 8 9 10))   ; Expected result: (1 3 5 7 9)

;; (write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d)))   ; Expected result: (a b c d)

(setq! lst '(1 2 3 4))
(setq! lst2 '(6 7 8 9))
(setq! lst3 '(10 11 12 13))

(princ "one:   ") (write (push-back! lst 5))    (nl)
(princ "two:   ") (write (push!      0   lst))  (nl)
;; (princ "three: ") (write (nconc!     lst lst2)) (nl)

(setq! ct 8)

;; (while (not (== 0 ct))
;;  (print "hello")
;;  (nl)
;;  (sleep 250)
;;  (setq! ct (- ct 1)))


(setq! nconc!
  (lambda (lists)
    (let ((result (car lists))
          (remaining (cdr lists)))
      (while (not (nil? remaining))
        (setq! result (nconc2! result (car remaining)))
        (setq! remaining (cdr remaining)))
      result)))

(setq! nconc2!
  (lambda (lst1 lst2)
    (if (nil? lst1)
        lst2
        (rplacd! (last lst1) lst2)
        lst1)))

;; (write (nconc! lst lst2 lst3))
;; (write lst)

(write (apply + '(1 2 3)))

(nconc! lst lst2 lst3)
