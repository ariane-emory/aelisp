;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }

;; (setq naive-fib (lambda (n)
;;   (if (<= n 2)
;;     1
;;     (+ (fib (- n 1)) (fib (- n 2))))))

;; (print
;; (let* ((n 30)
;;        (*memo*  '(2 1 1 1))
;;        (memoize  (lambda (k v)
;;                    (let ((second (car (cdr (setq *memo* (pset *memo* k v))))))
;;                      second)))
;;        (fib      (lambda (n)
;;                    (let ((memoized (pget *memo* n)))
;;                      (cond
;;                        (memoized memoized)
;;                        ;; ((<= n 2) 1)
;;                        (t (memoize n (+ (fib (- n 1)) (fib (- n 2)))))))))
;;        (result  (fib n)))
;;   result)
;; )

(let* ((𝑛 30) lisp-indent-offset
       (*memo* '((2 . 1) (1 . 1)))
       (memoize (λ (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
       (𝑓       (λ (𝑥)
                  (let  ((memoized (aget *memo*  𝑥)))
                    (∨    memoized
                         (memoize   𝑥 (+ (𝑓 (- 𝑥 1))
                                         (𝑓 (- 𝑥 2))))))))
       (car car)
       (cdr cdr)
       (aset aset)
       (aget aget)
       (+ +)
       (- -)
)
  (𝑓 𝑛))

;; (print (<< 10))

;; (print (¬ t))
