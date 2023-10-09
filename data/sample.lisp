;; -*-lisp-indent-function: 1; -*-

(put 'setq 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq double   (lambda (x) (* 2 x)))
(setq integer? (lambda (x) (eq :INTEGER (type x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq l '(2 (4 8)))
(transform! integer? double l)
(print l) ;; sucessfully prints (4 (8 16)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l (transform! integer? double '(2 (4 8))))
(print l) ;; successfully prints (4 (8 16)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l '(2 (4 . a)))
(transform! (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)) l)
(print l) ;; successdully prints (4 (8 . a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l (transform! integer? double '(2 (4 . 8))))
(print l) ;; successfully prints (4 (8 . 16))!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l 2)
(transform! integer? double l)
(print l) ;; should return an :ERROR

