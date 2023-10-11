;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq memo-fib
 (let* ((*memo* '((2 . 1) (1 . 1)))
       (memoize (lambda (kkk vvv) (cdr (car (setq *memo* (aset *memo* kkk vvv)))))))
  (lambda (nth) 
   (let* ((fib (lambda (xxx)
                (let ((memoized (aget *memo*  xxx)))
                 (or memoized (memoize  xxx (+ (fib (- xxx 1)) (fib (- xxx 2)))))))))
   (fib nth)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq memo-fib-2
  (lambda (nth) 
   (let* ((*memo* '((2 . 1) (1 . 1)))
          (memoize (lambda (k vvv) (cdr (car (setq *memo* (aset *memo* k vvv))))))
          (fib     (lambda (xxx)
                    (let ((memoized (aget *memo*  xxx)))
                     (let (result (or memoized (memoize  xxx (+ (fib (- xxx 1)) (fib (- xxx 2)))))))
                     (when (not result) (stop))
                     result
                     ))))
   (fib nth))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq naive-fib
 (lambda (n)
  (if (<= n 2)
   1
   (+ (naive-fib (- n 1)) (naive-fib (- n 2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cond-fib
 (lambda (n)
  (cond
   ((<= n 2) 1)
   (t (+ (cond-fib (- n 1)) (cond-fib (- n 2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq double   (lambda (x) (* 2 x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq prefetch
 (lambda (expr)
  (transform! expr
   (lambda (x) (and (symbol? x) (bound? x)))
   (lambda (x) (eval x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l '(2 (4 8)))
;; (transform! l integer? double)
;; (print l) ;; case 1: sucessfully prints (4 (8 16)).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l (transform! '(2 (4 8)) integer? double))
;; (print l) ;; case 2: successfully prints (4 (8 16)).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l '(2 (4 . a)))
;; (transform! l (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)))
;; (print l) ;; case 3: successdully prints (4 (8 . a))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l (transform! '(2 (4 . 8)) integer? double))
;; (print l) ;; case 5: successfully prints (4 (8 . 16))!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case 6: prints (1 :REPLACED), but i want (1 :REPLACED (4 5 6)). Not sure if problem is with transform! or its arguments?
;; (print (transform! '(1 (2 3) (4 5 6))
;;         (lambda (x) (and (proper? x) (eql (length (car x)) 2)))
;;         (lambda (x) :REPLACED))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fun          memo-fib)
(setq num          30)
(setq reps         5000)
(setq prefetch-fun nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when prefetch-fun (prefetch (body fun)))
(print (body fun))
(benchmark reps 100 (list 'print (list fun num)))
;; (benchmark reps 100 (list fun num))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(memo-fib 30)
