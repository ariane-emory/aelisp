
;; (log-eval t) (log-core t)
;; (setq! lst (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(setq! lst '(1 2 3 4 5 6 7 8 9 10))

(princ "initial lst: ") (write lst)              (nl)
(princ "memql? 2:    ") (write (memql?   3 lst)) (nl)
(princ "removeql 4:  ") (write (removeql 4 lst)) (nl)
(princ "indesql 2:   ") (write (indexql  2 lst)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst (make-list 6 1))
(princ "make-list:   ") (write lst) (nl)

(list-set! lst 0 100)
(princ "list-set! 0 100: ") (write lst) (nl)

(list-set! lst 1 101)
(princ "list-set! 1 101: ") (write lst) (nl)

(list-set! lst 2 102)
(princ "list-set! 2 102: ") (write lst) (nl)

(list-set! lst 3 103)
(princ "list-set! 3 103: ") (write lst) (nl)

(list-set! lst 4 104)
(princ "list-set! 4 104: ") (write lst) (nl)

(list-set! lst 5 105)
(princ "list-set! 5 105: ") (write lst) (nl)

(exit)

;;(write (list-ref lst 4))

(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

;; (log-eval t)
;; (write (2+ 3))
;; (nl)

;; (defmacro test-it (args . cond-clauses)
;;  (write (car args)) (nl)
;;  (write (cadr args)) (nl)
;;  (write (eq? 'lst (cadr args))) (nl)
;;  (cond
;;   ((not (or (eq? 'lst (first args)) (eq? 'lst (second args))))
;;    (error "one of the args must be the symbol 'lst"))
;;   (t (error "found lst"))))

;; (test-it (obj j))

