;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! else          t)
(setq! #t            t)
(setq! #f            nil)
(setq! assq          aget)
(setq! define        setq!)
;; ^ should be a macro that avoids re-definining what-scheme-implementation
(setq! null?         nil?)
(setq! set!          setq!)
(setq! ???           'unspecified-result)
(setq! letrec        let*)
(setq! list*         list) ;; might need support.scm's version
(setq! map           mapcar)
(setq! position-of   indexq)
(setq! make-vector   make-list)
(setq! vector-set!   list-set!)
(setq! vector-ref    list-ref)
(setq! vector-length length)
(defun gsort (predicate lst)
 (sort lst predicate))
(defun %allocate-instance-internal (head . tail)
 head) ;; FAKE PLACEHOLDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! v (make-list 10 0))

(when t
 (setq! ix 0)
 
 (until  (== ix 10)
  (princ "setting ") (princ ix) (nl)
  (list-set! v ix (* 2 ix))
  (write v)
  (nl)
  (setq! ix (1+ ix))))




(setq! l '(1 2 a (3 b)))

;;(log-eval t)

(write (transform integer? double l))

(define list*
 (lambda args
  (let*
   ((chase
	   (lambda (args)
		  (cond
       ((nil? args)       nil)
		   ((nil? (cdr args)) (car args))
		   (t                 (cons (car args) (chase (cdr args))))))))
   (chase args))))

(write (list* 'a 'b '(c d)))

(nl)
