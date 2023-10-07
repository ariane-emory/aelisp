(setq naive-fib (lambda (n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2))))))

(setq cond-fib (lambda (n)
            (cond
              ((<= n 2) 1)
              (t (+ (fib (- n 1)) (fib (- n 2)))))))

(setq cadr (lambda (x) (car (cdr x))))

(setq memoize  (lambda (k v) (cadr (setq *memo* (pset *memo* k v)))))
(setq memo-fib (lambda (n)
                 (let ((memoized (pget *memo* n)))
                   (cond
                     (memoized  memoized)
                     ((<= n 2)  1)
                     (t (memoize n (+ (fib (- n 1)) (fib (- n 2)))))))))

(setq memo-fib-2 (lambda (n)
                 (let ((memoized (pget *memo* n)))
                   (cond
                     (memoized  memoized)
                     ((<= n 2)  1)
                     (t (let ((x (memoize n (+ (fib (- n 1)) (fib (- n 2))))))
                          (pset *memo* n x)
                                ))))))



(setq fib memo-fib-2)

(fib 30)

;;(fib 20)

