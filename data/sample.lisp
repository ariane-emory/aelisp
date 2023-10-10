;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cons?    (lambda (x) (eq :CONS    (type x))))
(setq integer? (lambda (x) (eq :INTEGER (type x))))
(setq symbol?  (lambda (x) (eq :SYMBOL  (type x))))
(setq double (lambda (x) (* 2 x)))
(setq stop     (lambda ()  (exit 0)))
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq now (time))
;; (memo-fib)
;; (print (- (time) now))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq now (time))
;; (repeat 1000
;;  (memo-fib 30))
;; (print (- (time) now))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq memo-fib
;;  (lambda (n) 
;;   (let* ((nth 30)
;;          (*memo* '((2 . 1) (1 . 1)))
;;          (memoize (lambda (k v) (cdr (car (setq *memo* (aset *memo* k v))))))
;;          (memo-fib       (lambda (𝑥)
;;                           (let ((memoized (aget *memo*  𝑥)))
;;                            (or memoized
;;                               (memoize  𝑥 (+ (memo-fib (- 𝑥 1))
;;                                              (memo-fib (- 𝑥 2)))))))))
;;    (memo-fib nth))))

;; (memo-fib 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq prefetch
 (lambda (expr)
  (transform! expr
    (lambda (x) (and (symbol? x) (bound? x)))
    (lambda (x) (eval x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (setq prefetch-fib (prefetch
;;                      '(lambda (nth)
;;                        (let  ((memoized (aget *memo* nth))
;;                               (memoize (lambda (k v) (cdr (car (setq *memo* (aset *memo* k v)))))))
;;                         (or memoized
;;                          (memoize  nth (+ (prefetch-fib (- nth 1))
;;                                           (prefetch-fib (- nth 2)))))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq *memo* '((2 . 1) (1 . 1)))
;; (prefetch-fib 30)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq describe-elapsed
 (lambda (total repetitions)
  (nl)
  (princ "total ums: ")
  (princ total) (nl)
  (princ "total ms: ")
  (princ (/ total 1000)) (nl)
  (princ "total s: ")
  (princ (/ total 1000000)) (nl)
  (princ "each ms: ")
  (princ (/ total repetitions 1000)) (nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq benchmark
 (lambda (repetitions qexpr)
  (nl)
  (let ((ctr 0 )
        (total 0))
   (repeat repetitions
    (setq ctr (+ 1 ctr))
    (let ((before (time)))
     (eval qexpr)
     (setq total (+ total (elapsed before))))
     (when t ;; (== 0 (% ctr 10))
      (nl) (princ "Iteration #") (princ ctr) (princ ", ") (princ (/ total 1000)) (princ " ms so far.")))
   (describe-elapsed total repetitions))))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fun naive-fib)
(setq repetitions 100)
(prefetch (body fun))
(print (body fun))
(benchmark repetitions (list 'print (list fun 10)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
