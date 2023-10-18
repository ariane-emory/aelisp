;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lists (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(log-macro t)
(log-macro nil)

(nl)
(write `,1)

(nl)
(eval `(mapcar princ ',lists))

(nl)
(write (zip '(a b c) '(1 2 3)))

;; (nl)
;; (old-zip '(a b c) '(1 2 3))

(nl)
(princ "Done.")

(setq! lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(print (first lst))
(print (second lst))
(print (third lst))
(print (fourth lst))
(print (fifth lst))
(print (sixth lst))
(print (seventh lst))
(print (eighth lst))
(print (ninth lst))
(print (tenth lst))
(print (eleventh lst))
(print (twelfth lst))


(defun list* args
 (let*
  ((chase
	  (lambda (args)
		 (cond
      ((nil? args) nil)
		  ((nil?       (cdr args)) (car args))
		  (t           (cons (car args) (chase (cdr args))))))))
  (chase args)))

(log-eval t)
(list* lst)

