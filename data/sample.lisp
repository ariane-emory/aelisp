;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }

;; (setq naive-fib (lambda (n)
;;   (if (<= n 2)
;;     1
;;     (+ (fib (- n 1)) (fib (- n 2))))))

;; (let* ((𝑛 30)
;;        (∨ ∨)
;;        (*memo* '((2 . 1) (1 . 1)))
;;        (memoize (λ (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
;;        (𝑓       (λ (𝑥)
;;                   (let  ((memoized (aget *memo*  𝑥)))
;;                     (∨    memoized
;;                           (memoize  𝑥 (+ (𝑓 (- 𝑥 1))
;;                                          (𝑓 (- 𝑥 2)))))))))
;;   (𝑓 𝑛))


(setq double (lambda (x) (* 2 x)))
      
(setq map
      (lambda (func lst)
        (if (nil? lst)
            nil
            (cons (func (car lst))
                  (map func (cdr lst))))))

(print (map double '(3 6 9)))

(setq preload
      (lambda (lst)
        (cond ((nil? lst) nil)
              ((integer? (car lst)) (progn
                                      (rplaca (car lst) (* 2 (car lst)))
                                      lst
                                      ))
              (t         (cons (car lst)
                               (preload (cdr lst)))))))

(print (map double '(3 6 9)))

(sets l '(1 2 3))
(rplaca l 4)
;(print l)

;; (print (preload '(car (4 8))))
