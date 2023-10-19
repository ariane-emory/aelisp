;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combined-comparator (x y)
 (cond 
  ((and (even? x) (even? y)) (< x y))  ; both even, compare values
  ((even? x) t)                        ; x is even, y is odd, x comes first
  ((even? y) nil)                      ; y is even, x is odd, y comes first
  (t (< x y))))                       ; both odd, compare values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! lst '(3 1 13 2 8 4 5 12 7 11 9 6 10 15 14))
(write (syms (env))) (nl)
(write (sort lst combined-comparator)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! #t t)
(setq! define setq!)
;; ^ should be a macro to avoid redefinining what-scheme-implementation
(defun gsort (lambda (predicate lst) (sort lst predicate)))
(setq! null? nil?)
(setq! set! setq!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %allocate-instance
 (lambda (class nfields)
  (%allocate-instance-internal
   class
   #t
   (lambda args
	  (error "An instance isn't a procedure -- can't apply it."))
   nfields)))

(write %allocate-instance)
