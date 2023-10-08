(setq transform!
      (lambda (pred fun lst)
        (let ((head (car lst))
              (tail (cdr lst)))
          (cond 
            ((nil? lst) lst)
            ((pred head) 
             (rplaca lst (fun head))
             (transform! pred fun tail))
            ((eq :CONS (type head))
             (transform! pred fun head)
             (transform! pred fun tail))
            (t (transform! pred fun tail)))
          ;; Additional check for the cdr of the list.
          (if (and lst (pred tail))
              (rplacd lst (fun tail))))
        lst))

;; (setq l '(2 (4 8)))
;; (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) l)
;; (print l) ;; sucessfully prints (4 (8 16)).

;; (setq l (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) '(2 (4 8))))
;; (print l) ;; successfully prints (4 (8 16)).

(setq l '(2 (4 . 8)))
(transform! (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)) l)
(print l) ;; fails to print (4 (8 . 16)), object pool is filled up somehow!

;; (setq l (transform! (lambda (x) (eq :INTEGER (type x))) (lambda (x) (* 2 x)) '(2 (4 . 8))))
;; (print l) ;; fails to print (4 (8 . 16))!
