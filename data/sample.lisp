;; (setq! odd?
;;  (lambda (n) 
;;   (not (== 0 (% n 2)))))

;; (filter odd? '(1 2 3 4 5 6 7 8 9 10))   ; Expected result: (1 3 5 7 9)

;; (write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d)))   ; Expected result: (a b c d)

(setq! lst '(1 2 3 4))
(setq! lst2 '(6 7 8 9))

(write (push-back! lst 5))    (nl)
(write (push!      0 lst))    (nl)
(write (nconc!     lst lst2)) (nl)

