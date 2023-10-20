(when t
 (setq! lst (removeql 4 (union2 memql? '(1 2 3 4) '(4 5 2 2))))
 (write lst)
 (nl)

 (write (indexql 1 lst))
 (nl)

 (write (unionq '(1 2 3 4) '(4 5 2 2) '(5 6 7 8)))
 (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst (make-list 6 1))

(when nil
 (log-eval t)
 (log-core t)
 (log-macro t))

(list-set! lst 0 2)
(list-set! lst 4 8)
(list-set! lst 5 10)

(princ "here ")
(write lst)
(nl)

(write (list-ref lst 4))
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

;; (log-eval t)
;; (write (2+ 3))
;; (nl)


(defmacro test-it (args . cond-clauses)
 (write (car args)) (nl)
 (write (cadr args)) (nl)
 (write (eq? 'lst (cadr args))) (nl)
 (cond
  ((not (or (eq? 'lst (first args)) (eq? 'lst (second args))))
   (error "one of the args must be the symbol 'lst"))
  (t (error "should not happen"))))

(log-eval t)

(test-it (obj lst))

