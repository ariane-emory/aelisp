;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq memo-fib
 (let* ((*memo* '((2 . 1) (1 . 1)))
       (memoize (lambda (k v) (cdr (car (setq *memo* (aset *memo* k v)))))))
  (lambda (nth) 
   (let* ((fib (lambda (𝑥)
                (let ((memoized (aget *memo*  𝑥)))
                 (or memoized (memoize  𝑥 (+ (fib (- 𝑥 1)) (fib (- 𝑥 2)))))))))
   (fib nth)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq memo-fib-2
  (lambda (nth) 
   (let* ((*memo* '((2 . 1) (1 . 1)))
          (memoize (lambda (k v) (cdr (car (setq *memo* (aset *memo* k v))))))
          (fib     (lambda (𝑥)
                    (let ((memoized (aget *memo*  𝑥)))
                     (or memoized (memoize  𝑥 (+ (fib (- 𝑥 1)) (fib (- 𝑥 2)))))))))
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
(setq cons?    (lambda (x) (eq :CONS    (type x))))
(setq integer? (lambda (x) (eq :INTEGER (type x))))
(setq symbol?  (lambda (x) (eq :SYMBOL  (type x))))
(setq double   (lambda (x) (* 2 x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq transform!
 (lambda (expr pred fun)
  (if (not (eq :CONS (type expr)))
   (error "expr must be a list")
   (cond
    ((pred expr) (setf expr (fun expr)))
    ((eq :CONS (type expr))
     (let ((head (car expr))
           (tail (cdr expr)))
      (cond
       ((pred head) (rplaca expr (fun head)))
       ((eq :CONS (type head))  (transform! head pred fun)))
      (cond
       ((pred tail) (rplacd expr (fun tail)))
       ((eq :CONS (type tail))  (rplacd expr (transform! tail pred fun))))))
    (t expr))
   expr)))
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
(setq benchmark
 (lambda (repetitions print-interval qexpr)
  (nl)
  (let ((ctr 0 )
        (total 0))
   (repeat repetitions
    (setq ctr (+ 1 ctr))
    (let ((before (time)))
     (eval qexpr)
     (setq total (+ total (elapsed before))))
    (when (== 0 (% ctr print-interval))
     (nl)
     (princ "Iteration #")
     (princ ctr)
     (princ ", ")
     (princ (/ total 1000))
     (princ " ms so far.")))
   (nl)
   (princ "total ums: ")
   (princ total) (nl)
   (princ "total ms: ")
   (princ (/ total 1000)) (nl)
   (princ "total s: ")
   (princ (/ total 1000000)) (nl)
   (princ "each ms: ")
   (princ (/ total repetitions 1000)) (nl))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fun          memo-fib)
(setq num          30)
(setq reps         5000)
(setq prefetch-fun nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when prefetch-fun (prefetch (body fun)))
(print (body fun))
;; (benchmark reps 100 (list 'print (list fun num)))
(benchmark reps 100 (list fun num))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
