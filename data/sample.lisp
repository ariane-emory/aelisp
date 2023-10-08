(setq transform!
      (lambda (pred fun obj)
        (if (not (eq :CONS (type obj))) 
            (if (pred obj) 
                (setf obj (fun obj)))
          (let ((head (car obj))
                (tail (cdr obj)))
            (cond 
              ((nil? obj) obj)
              ((pred head)
               (rplaca obj (fun head)))
              ((eq :CONS (type head))
               (transform! pred fun head)))
            ;; Handle the rest of the list
            (if tail
                (rplacd obj (transform! pred fun tail)))
            ;; Special case for the end of an improper list
            (if (and (not (eq :CONS (type tail))) (pred tail))
                (rplacd obj (fun tail)))))
        obj)) ; Moved outside the if condition but still within the function body.



;; (setq l '(2 (4 8)))
;; (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) l)
;; (print l) ;; sucessfully prints (4 (8 16)).

;; (setq l (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) '(2 (4 8))))
;; (print l) ;; successfully prints (4 (8 16)).

(setq l '(2 (4 . a)))
(transform! (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)) l)
(print l) ;; fails to print (4 (8 . 16)), object pool is filled up somehow!

;; (setq l (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) '(2 (4 . 8))))
;; (print l) ;; fails to print (4 (8 . 16))!
