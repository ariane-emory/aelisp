;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }
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
;; (transform! integer? double l)
;; (print l) ;; case 1: sucessfully prints (4 (8 16)).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l (transform! integer? double '(2 (4 8))))
;; (print l) ;; case 2: successfully prints (4 (8 16)).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l '(2 (4 . a)))
;; (transform! (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)) l)
;; (print l) ;; case 3: successdully prints (4 (8 . a))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l (transform! integer? double '(2 (4 . 8))))
;; (print l) ;; case 5: successfully prints (4 (8 . 16))!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq l 2)
;; (transform! integer? double l)
;; (print l) ;; correctly returns an :ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (print (transform!
;;         (lambda (x) (and (proper? x) (eql (length (car x)) 2)))
;;         (lambda (x) :REPLACED)
;;         '(1 (2 3) (4 5 6)))) ;; case 6: prints (1 :REPLACED), but i want (1 :REPLACED (4 5 6)). Not sure if problem is with transform! or its arguments?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq memo-fib
;;  (lambda (n)
;;   (let*
;;    ((*memo* '((2 . 1) (1 . 1)))
;;     (memoize (lambda (k v) (cdr (car (setq *memo* (aset *memo* k v))))))
;;     (fib      (lambda (x)
;;                (let ((memoized (aget       *memo*  x))
;;                 (∨    memoized
;;                      (memoize  x (+ (fib (- x 1))
;;                                     (fib (- x 2))))))))))
;;   (fib n))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq now (time))
;; (memo-fib)
;; (print (- (time) now))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq now (time))
;; (repeat 1000
;;  (memo-fib 30))
;; (print (- (time) now))
(print
(let* ((𝑛 30)
       (*memo* '((2 . 1) (1 . 1)))
       (memoize (lambda (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
       (fib       (lambda (𝑥)
                   (let ((memoized (aget *memo*  𝑥)))
                    (∨    memoized
                         (memoize  𝑥 (+ (fib (- 𝑥 1))
                                        (fib (- 𝑥 2)))))))))
 (fib 𝑛))
)
