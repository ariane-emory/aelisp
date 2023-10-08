;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }

;; (setq naive-fib (lambda (n)
;;   (if (<= n 2)
;;     1
;;     (+ (fib (- n 1)) (fib (- n 2))))))

(let* ((𝑛 30)
       (car car)
       (cdr cdr)
       (+ +)
       (- -)
       (*memo* '((2 . 1) (1 . 1)))
       (memoize (λ (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
       (𝑓       (λ (𝑥)
                  (let  ((memoized (aget *memo*  𝑥)))
                    (∨    memoized
                          (memoize  𝑥 (+ (𝑓 (- 𝑥 1))
                                         (𝑓 (- 𝑥 2)))))))))
  (𝑓 𝑛))

;; (print (<< 10))

;; (print (¬ t))
