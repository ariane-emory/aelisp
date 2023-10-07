;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }

;; (setq naive-fib (lambda (n)
;;   (if (<= n 2)
;;     1
;;     (+ (fib (- n 1)) (fib (- n 2))))))

;; (setq cond-fib (lambda (n)
;;             (cond
;;               ((<= n 2) 1)
;;               (t (+ (fib (- n 1)) (fib (- n 2)))))))

;; (setq cadr     (lambda (x)   (car  (cdr x))))

(print
(let ((n 30)
      (*memo* '(2 1 1 1))
      (memoize (lambda (k v) (car (cdr (setq *memo* (pset *memo* k v))))))
      (fib     (lambda (n)
                 (let ((memoized (pget *memo* n)))
                   (cond
                     (memoized memoized)
                     ;; ((<= n 2) 1)
                     (t (memoize n (+ (fib (- n 1)) (fib (- n 2))))))))))
  (fib n))
)

;;(fib 20)

