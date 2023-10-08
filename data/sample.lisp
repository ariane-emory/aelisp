
(setq double-in-place
      (lambda (lst)
        (cond 
          ((nil? lst) nil)
          ((eq :INTEGER (type (car lst)))
           (rplaca lst (* 2 (car lst))))
          ((eq :CONS (type (car lst)))
           (double-in-place (car lst))))
        (if (not (nil? (cdr lst)))
            (double-in-place (cdr lst)))))

(setq transform!
      (lambda (pred fun lst)
        (cond 
          ((nil? lst) nil)
          ((eq :INTEGER ((car lst)))
           (rplaca lst (* 2 (car lst))))
          ((eq :CONS (type (car lst)))
           (transform! pred fun (car lst))))
        (if (not (nil? (cdr lst)))
            (transform! pred fun (cdr lst)))))

(setq l '(4 8))

(double-in-place l)
(print l)


;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }

;; (setq naive-fib (lambda (n)
;;   (if (<= n 2)
;;     1
;;     (+ (fib (- n 1)) (fib (- n 2))))))

;; (let* ((𝑛 30)
;;        (∨ ∨)
;;        (*memo* '((2 . 1) (1 . 1)))
;;        (memoize (λ (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
;;        (𝑓       (λ (𝑥)
;;                   (let  ((memoized (aget *memo*  𝑥)))
;;                     (∨    memoized
;;                           (memoize  𝑥 (+ (𝑓 (- 𝑥 1))
;;                                          (𝑓 (- 𝑥 2)))))))))
;;   (𝑓 𝑛))


;; (setq double (lambda (x) (* 2 x)))
      
;; (setq map
;;       (lambda (func lst)
;;         (if (nil? lst)
;;             nil
;;             (cons (func (car lst))
;;                   (map func (cdr lst))))))

;; (print (map double '(3 6 9)))


;; (setq l '(1 2 3))
;; (rplaca l 4)
;; (print l)

;; (let ((lst '(1 (2 3))))
;;   (dolist (item lst)
;;     (when (consp item)
;;       (rplaca item (* 2 (car item)))
;;       (rplacd item (* 2 (cadr item))))
;;     (when (numberp item)
;;       (setf lst (cons (* 2 item) (cdr lst)))))
;;   lst)
