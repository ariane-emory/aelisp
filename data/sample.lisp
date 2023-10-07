;; mcm; time { for i in {1..20000}; do ./bin/ae; done; }

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

;; (print
(let* ((n 30)
       (*memo* '((2 . 1) (1 . 1)))
       (memoize (lambda (k v) (cdr (car (setq *memo* (aset *memo* k v))))))
       (fib     (lambda (n)
                  (let ((memoized (aget *memo* n)))
                    (cond
                      (memoized memoized)
                      ;; ((<= n 2) 1)
                      (t (memoize n (+ (fib (- n 1)) (fib (- n 2)))))))))
       (result  (fib n)))
  result)
;; )



