(setq! ct 8)

(while (> ct 0)
  (write ct) (nl)
  (setq! ct (- ct 1)))

(write (filter odd? '(1 2 3 4 5 6 7 8 9 10))) (nl)

(write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d)))

(setq lst '(1 2 3 4))
(princ "one:   ") (write (push-back! lst 5))    (nl)
;; (princ "two:   ") (write (push!      0   lst))  (nl)
;; (princ "three: ") (write (nconc!     lst lst2)) (nl)



;; (setq! lst '(1 2))
;; (setq! lst2 '(3 4))
;; (setq! lst3 '(5 6))

;; (write (nconc! lst lst2 lst3)) (nl)

;; (write lst) (nl)

;; (write (mapconcat         (lambda (x) x) '("a" "b" "c") " "))  (nl)
;; (write (apply mapconcat '((lambda (x) x) '("a" "b" "c") " "))) (nl)

;; ;; (write (equal? '(a (1 2)) '(a (1 2)))) (nl)

;; (equal? '(1) '(1))

;; (setq! l '(1 2 (3 4)))
;; (transform! l integer? double)
;; (write l) (nl) ;; (2 4 (6 8))

;; (setq! l '(1 2 (3 4)))
;; (setq! new-l (transform l integer? double))
;; (write l) (nl)     ;; (1 2 (3 4)) remains unchanged
;; (write new-l) (nl) ;; (2 4 (6 8)) the transformed list
