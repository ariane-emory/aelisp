;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-chase-fun (pred? . cond-clauses)
 `(lambda (x lst)
   (letrec
    ((chase
      (lambda (lst)
       (cond
        ,@cond-clauses))))
    (chase lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-remove-fun (pred?)
 `(make-chase-fun ,pred?
   ((,pred? (car lst) x) (cdr lst))
   (lst (cons (car lst) (chase (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-pred (pred?)
 `(make-chase-fun ,pred?
   ((,pred? x (car lst)) t)
   (lst (chase (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! removeq  (make-remove-fun eq?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! removeql (make-remove-fun eql?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! memql? (make-member-pred eql?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! memq?  (make-member-pred eq?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;i


(write (removeql 4 (union2 memql? '(1 2 3 4) '(4 5 2 2))))
(nl)

(write (indexql 2 (removeql 4 (union2 memql? '(1 2 3 4) '(4 5 2 2)))))
(nl)

(write (unionq'(1 2 3 4) '(4 5 2 2) '(5 6 7 8)))
(nl)

;; (write
;;  (letrec
;;   ((factorial
;;     (lambda (n)
;;      (if (< n 2)
;;       1
;;       (* n (factorial (- n 1)))))))
;;   (factorial 5)))
;; (nl)

;; (write (memql? 4 '(1 2 3 4 5 6 7)))
;; (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(exit)

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

;; (log-eval t)
;; (write (2+ 3))
;; (nl)

