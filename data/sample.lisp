;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- lisp-indent-function: (put 'setq 'lisp-indent-function 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when nil
;;   (let* ((𝑛 30)
;;          (∨ ∨)
;;          (*memo* '((2 . 1) (1 . 1)))
;;          (memoize (lambda (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
;;          (𝑓       (lambda (𝑥)
;;                     (let  ((memoized (aget *memo*  𝑥)))
;;                       (∨    memoized
;;                             (memoize  𝑥 (+ (𝑓 (- 𝑥 1))
;;                                            (𝑓 (- 𝑥 2)))))))))
;;     (𝑓 𝑛)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq double   (lambda (x) (* 2 x)))
(setq integer? (lambda (x) (eq :INTEGER (type x))))
(setq symbol?  (lambda (x) (eq :SYMBOL  (type x))))
(setq cons?    (lambda (x) (eq :CONS    (type x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lisp-indent-function
(setq transform!
      (lambda (pred fun obj)
        (if (not (eq :CONS (type obj)))
            (error "obj must be a list")
            (cond
              ((pred obj) (setf obj (fun obj)))
              ((eq :CONS (type obj))
               (let ((head (car obj))
                     (tail (cdr obj)))
                 (cond
                   ((pred head) (rplaca obj (fun head)))
                   ((eq :CONS (type head))  (transform! pred fun head)))
                 (cond
                   ((pred tail) (rplacd obj (fun tail)))
                   ((eq :CONS (type tail))  (rplacd obj (transform! pred fun tail))))))
              (t obj))
            obj)))

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

;; (print (transform!
;;         (lambda (x) (and (symbol? x) (bound? x)))
;;         (lambda (x) (eval x))
;;         '(cons (1 x))))

(setq memoize (lambda (k v)
                (cdr (car (setq *memo* (aset *memo* k v))))))

(setq fib
      (eval 
       (transform!
        (lambda (x) (and (symbol? x) (bound? x)))
        (lambda (x) (eval x))
        '(lambda (z)
          (let  ((memoized (aget *memo*  z)))
            (∨    memoized
                  (memoize  𝑥 (+ (fib (- z 1))
                                 (fib (- z 2))))))))))

;; (setq *memo* '((2 . 1) (1 . 1)))

;; (print prefetched)
;; ;;(setq fib (eval prefetched))
;; ;;(print fib)
;; ;; (stop)
;; (print (fib 30))

(setq prefetch (lambda (expr) 
                 (eval (transform!
                        (lambda (x) (and (symbol? x) (bound? x)))
                        (lambda (x) (eval x))
                        expr))))

(setq double (prefetch '(lambda (x) (* 2 x))))
(print (double 333))

