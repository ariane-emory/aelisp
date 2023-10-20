(when nil
 (setq! lst (removeql 4 (union2 memql? '(1 2 3 4) '(4 5 2 2))))
 (write lst)
 (nl)

 (write (indexql 1 lst))
 (nl)

 (write (unionq'(1 2 3 4) '(4 5 2 2) '(5 6 7 8)))
 (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst (make-list 6 1))

(list-set! lst 0 2)
(list-set! lst 4 8)
(list-set! lst 5 10)

(write lst)
(nl)

(write (list-ref lst 4))
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(exit)

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

;; (log-eval t)
;; (write (2+ 3))
;; (nl)

