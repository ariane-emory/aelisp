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

(let* ((n 30)
       (*memo* '((2 . 1) (1 . 1)))
       (memoize (λ (k v) (cdr (car  (≔    *memo* (aset *memo* k v))))))
       (𝑓       (λ (n)
                   (let  ((memoized (aget *memo*  n)))
                     (∨    memoized
                          (memoize   n (+ (𝑓 (- n 1))
                                          (𝑓 (- n 2)))))))))
  (𝑓 n))

;; (print (<< 10))

;; (print (¬ t))
