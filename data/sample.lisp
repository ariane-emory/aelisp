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
(setq! #t          t)
(setq! #t          nil)
(setq! define      setq!)
;; ^ should be a macro to avoid redefinining what-scheme-implementation
(setq! null?       nil?)
(setq! set!        setq!)
(setq! ???         'unspecified-result)
(setq! letrec      let*)
(setq! list*       list) ; might need support.scm's version
;; vector-set! macro
(defun gsort (predicate lst)
 (sort lst predicate)))
(defun vector-ref (lst n)
(nth n lst)))
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

(define %allocate-entity
 (lambda (class nfields)
  (%allocate-instance-internal
   class
   #f
   (lambda args
	  (error "Tried to call an entity before its proc is set."))
   nfields)))

(define %allocate-instance-internal ???)
(define %instance?                  ???)
(define %instance-class             ???)
(define %set-instance-class-to-self ???)   ;This is used only once
                                        ;as part of bootstrapping
                                        ;the braid.
(define %set-instance-proc!  ???)
(define %instance-ref        ???)
(define %instance-set!       ???)

(letrec ((instances '())
	       (get-vector
	        (lambda (closure)
	         (let ((cell (assq closure instances)))
	          (if cell (cdr cell) #f)))))

 (set! %allocate-instance-internal
	(lambda (class lock proc nfields)
	 (letrec ((vector (make-vector (+ nfields 3) #f))
		        (closure (lambda args
			                (apply (vector-ref vector 0) args))))
	  (vector-set! vector 0 proc)
	  (vector-set! vector 1 lock)
	  (vector-set! vector 2 class)
	  (set! instances (cons (cons closure vector) instances))
	  closure)))

 (set! %instance?
  (lambda (x) (not (null? (get-vector x)))))

 (set! %instance-class
	(lambda (closure)
	 (let ((vector (get-vector closure)))
	  (vector-ref vector 2))))

 (set! %set-instance-class-to-self
	(lambda (closure)
	 (let ((vector (get-vector closure)))
	  (vector-set! vector 2 closure))))

   (set! %set-instance-proc!
        (lambda (closure proc)
	  (let ((vector (get-vector closure)))
	    (if (vector-ref vector 1)
		(error "Can't set procedure of instance.")
		(vector-set! vector 0 proc)))))

  (set! %instance-ref
        (lambda (closure index)
	  (let ((vector (get-vector closure)))
	    (vector-ref vector (+ index 3)))))
   
  (set! %instance-set!
        (lambda (closure index new-value)
	  (let ((vector (get-vector closure)))
	    (vector-set! vector (+ index 3) new-value))))
  )
