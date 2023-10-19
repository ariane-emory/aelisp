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
(setq! else         t)
(setq! #t           t)
(setq! #f           nil)
(setq! assq         aget)
(setq! define       setq!)
;; ^ should be a macro to avoid redefinining what-scheme-implementation
(setq! null?        nil?)
(setq! set!         setq!)
(setq! ???          'unspecified-result)
(setq! letrec       let*)
(setq! list*        list) ;; might need support.scm's version
(setq! map          mapcar)
(setq! position-of  indexq)
(defun gsort (predicate lst)
 (sort lst predicate))
(defun vector-ref   (lst n)
 (nth n lst))
(defun %allocate-instance-internal (head . tail)
 head) ;; FAKE PLACEHOLDER
(defun make-vector  (head . tail)
 head) ;; FAKE PLACEHOLDER
(defun vector-set!  (head . tail)
 head) ;; FAKE PLACEHOLDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos suppor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define getl
    (lambda (initargs name . not-found)
      (letrec ((scan (lambda (tail)
		       (cond ((null? tail)
			      (if (pair? not-found)
				  (car not-found)
				  (error "GETL couldn't find" name)))
			     ((eq? (car tail) name) (cadr tail))
			     (else (scan (cddr tail)))))))
	(scan initargs))))

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


                                        ;
                                        ; %allocate-instance, %allocate-entity, %instance-ref, %instance-set!
                                        ; and class-of are the normal interface, from the rest of the code, to
                                        ; the low-level memory system.  One thing to take note of is that the
                                        ; protocol does not allow the user to add low-level instance
                                        ; representations.  I have never seen a way to make that work.
                                        ;
                                        ; Note that this implementation of class-of assumes the name of a the
                                        ; primitive classes that are set up later.
                                        ; 
(define class-of
 (lambda (x)
  (cond ((%instance? x)  (%instance-class x))

	 ((pair? x)        <pair>)         ;If all Schemes were IEEE 
	 ((null? x)        <null>)         ;compliant, the order of
	 ((boolean? x)     <boolean>)      ;these wouldn't matter?
	 ((symbol? x)      <symbol>)
	 ((procedure? x)   <procedure>)
	 ((number? x)      <number>)
	 ((vector? x)      <vector>)
	 ((char? x)        <char>)
	 ((string? x)      <string>)
	 (( input-port? x)  <input-port>)
	 ((output-port? x) <output-port>)
	 

	 )))


                                        ;
                                        ; Now we can get down to business.  First, we initialize the braid.
                                        ;
                                        ; For Bootstrapping, we define an early version of MAKE.  It will be
                                        ; changed to the real version later on.  String search for ``set! make''.
                                        ;

(define make
 (lambda (class . initargs)
  (cond ((or (eq? class <class>)
		      (eq? class <entity-class>))
	       (let* ((new (%allocate-instance
			                class
			                (length the-slots-of-a-class)))
		            (dsupers (getl initargs 'direct-supers '()))
		            (dslots  (map list
				                  (getl initargs 'direct-slots  '())))
		            (cpl     (let loop ((sups dsupers)
					                          (so-far (list new)))
				                  (if (null? sups)
				                   (reverse so-far)
				                   (loop (class-direct-supers
					                        (car sups))
					                  (cons (car sups)
						                 so-far)))))
		            (slots (apply append
				                (cons dslots
					               (map class-direct-slots
					                (cdr cpl)))))
		            (nfields 0)
		            (field-initializers '())
		            (allocator
		             (lambda (init)
			            (let ((f nfields))
			             (set! nfields (+ nfields 1))
			             (set! field-initializers
				            (cons init field-initializers))
			             (list (lambda (o)   (%instance-ref  o f))
				            (lambda (o n) (%instance-set! o f n))))))
		            (getters-n-setters
		             (map (lambda (s)
			                 (cons (car s)
				                (allocator (lambda () '()))))
			            slots)))

	        (slot-set! new 'direct-supers      dsupers)
	        (slot-set! new 'direct-slots       dslots)
	        (slot-set! new 'cpl                cpl)
	        (slot-set! new 'slots              slots)
	        (slot-set! new 'nfields            nfields)
	        (slot-set! new 'field-initializers (reverse
						                                  field-initializers))
	        (slot-set! new 'getters-n-setters  getters-n-setters)
	        new))
	 ((eq? class <generic>)
	  (let ((new (%allocate-entity class
					      (length (class-slots class)))))
	   (slot-set! new 'methods '())
	   new))
	 ((eq? class <method>)
	  (let ((new (%allocate-instance
			          class
			          (length (class-slots class)))))
	   (slot-set! new
			'specializers
			(getl initargs 'specializers))
	   (slot-set! new
			'procedure
			(getl initargs 'procedure))
	   new)))))


                                        ;
                                        ; These are the real versions of slot-ref and slot-set!.  Because of the
                                        ; way the new slot access protocol works, with no generic call in line,
                                        ; they can be defined up front like this.  Cool eh?
                                        ;
                                        ;
(define slot-ref
 (lambda (object slot-name)
  (let* ((info   (lookup-slot-info (class-of object) slot-name))
	       (getter (list-ref info 0)))
	 (getter object))))

(define slot-set!
 (lambda (object slot-name new-value)
  (let* ((info   (lookup-slot-info (class-of object) slot-name))
	       (setter (list-ref info 1)))
	 (setter object new-value))))

(define lookup-slot-info
 (lambda (class slot-name)
  (let* ((getters-n-setters
	        (if (eq? class <class>)           ;* This grounds out
		       getters-n-setters-for-class   ;* the slot-ref tower.
		       (slot-ref class 'getters-n-setters)))
	       (entry (assq slot-name getters-n-setters)))
	 (if entry
	  (cdr entry)
	  (error "No slot" slot-name "in instances of" class)))))



                                        ;
                                        ; Given that the early version of MAKE is allowed to call accessors on
                                        ; class metaobjects, the definitions for them come here, before the
                                        ; actual class definitions, which are coming up right afterwards.
                                        ;
                                        ;
(define class-direct-slots
 (lambda (class) (slot-ref class 'direct-slots)))
(define class-direct-supers
 (lambda (class) (slot-ref class 'direct-supers)))
(define class-slots
 (lambda (class) (slot-ref class 'slots)))
(define class-cpl
 (lambda (class) (slot-ref class 'cpl)))

(define generic-methods
 (lambda (generic) (slot-ref generic 'methods)))

(define method-specializers
 (lambda (method) (slot-ref method 'specializers)))
(define method-procedure
 (lambda (method) (slot-ref method 'procedure)))


                                        ;
                                        ; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
                                        ; because the first and fourth both contribute to <class>.
                                        ;
(define the-slots-of-a-class     ;
 '(direct-supers              ;(class ...)        
   direct-slots               ;((name . options) ...)
   cpl                        ;(class ...) 
   slots                      ;((name . options) ...) 
   nfields                    ;an integer
   field-initializers         ;(proc ...)
   getters-n-setters))        ;((slot-name getter setter) ...)
                                        ;
(define getters-n-setters-for-class      ;see lookup-slot-info
                                        ;
                                        ; I know this seems like a silly way to write this.  The
                                        ; problem is that the obvious way to write it seems to
                                        ; tickle a bug in MIT Scheme!
                                        ;
 (let ((make-em (lambda (s f)
		             (list s
			            (lambda (o)   (%instance-ref  o f))
			            (lambda (o n) (%instance-set! o f n))))))
  (map (lambda (s)
	      (make-em s (position-of s the-slots-of-a-class)))
	 the-slots-of-a-class)))
(define <class> (%allocate-instance #f (length the-slots-of-a-class)))
(%set-instance-class-to-self <class>)

(define <top>          (make <class>
			                  'direct-supers (list)
			                  'direct-slots  (list)))

(define <object>       (make <class>
			                  'direct-supers (list <top>)
			                  'direct-slots  (list)))

                                        ;
                                        ; This cluster, together with the first cluster above that defines
                                        ; <class> and sets its class, have the effect of:
                                        ;
                                        ;   (define <class>
                                        ;     (make <class>
                                        ;           'direct-supers (list <object>)
                                        ;           'direct-slots  (list 'direct-supers ...)))
                                        ;
(slot-set! <class> 'direct-supers      (list <object>))
(slot-set! <class> 'direct-slots       (map list the-slots-of-a-class))
(slot-set! <class> 'cpl                (list <class> <object> <top>))
(slot-set! <class> 'slots              (map list the-slots-of-a-class))
(slot-set! <class> 'nfields            (length the-slots-of-a-class))
(slot-set! <class> 'field-initializers (map (lambda (s)
					                                   (lambda () '()))
					                              the-slots-of-a-class))
(slot-set! <class> 'getters-n-setters  '())


(define <procedure-class> (make <class>
				                   'direct-supers (list <class>)
				                   'direct-slots  (list)))

(define <entity-class>    (make <class>
			                     'direct-supers (list <procedure-class>)
			                     'direct-slots  (list)))

(define <generic>         (make <entity-class>
			                     'direct-supers (list <object>)
			                     'direct-slots  (list 'methods)))

(define <method>          (make <class>
			                     'direct-supers (list <object>)
			                     'direct-slots  (list 'specializers
						                               'procedure)))



                                        ;
                                        ; These are the convenient syntax we expose to the base-level user.
                                        ;
                                        ;
(define make-class
 (lambda (direct-supers direct-slots)
  (make <class>
	 'direct-supers direct-supers
	 'direct-slots  direct-slots)))

(define make-generic
 (lambda ()
  (make <generic>)))

(define make-method
 (lambda (specializers procedure)
  (make <method>
	 'specializers specializers
	 'procedure    procedure)))




                                        ;
                                        ; The initialization protocol
                                        ;
(define initialize (make-generic))


                                        ;
                                        ; The instance structure protocol.
                                        ;
(define allocate-instance (make-generic))
(define compute-getter-and-setter (make-generic))


                                        ;
                                        ; The class initialization protocol.
                                        ;
(define compute-cpl   (make-generic))
(define compute-slots (make-generic))

                                        ;
                                        ; The generic invocation protocol.
                                        ;
(define compute-apply-generic         (make-generic))
(define compute-methods               (make-generic))
(define compute-method-more-specific? (make-generic))
(define compute-apply-methods         (make-generic))




                                        ;
                                        ; The next thing to do is bootstrap generic functions.
                                        ; 
(define generic-invocation-generics (list compute-apply-generic
					                           compute-methods
					                           compute-method-more-specific?
					                           compute-apply-methods))

(define add-method
 (lambda (generic method)
  (slot-set! generic
	 'methods
	 (cons method
		(collect-if
		 (lambda (m)
			(not (every eq?
				    (method-specializers m)
				    (method-specializers method))))
		 (slot-ref generic 'methods))))
  (%set-instance-proc! generic (compute-apply-generic generic))))

                                        ;
                                        ; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
                                        ; the other generics in the generic invocation protocol.  Two, related,
                                        ; problems come up.  A chicken and egg problem and a infinite regress
                                        ; problem.
                                        ;
                                        ; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
                                        ; something sitting there, so it can be called.  The first definition
                                        ; below does that.
                                        ; 
                                        ; Then, the second definition solves both the infinite regress and the
                                        ; not having enough of the protocol around to build itself problem the
                                        ; same way: it special cases invocation of generics in the invocation
                                        ; protocol.
                                        ;
                                        ;
(%set-instance-proc! compute-apply-generic
 (lambda (generic)
  (let ((method (car (generic-methods generic))))
	 ((method-procedure method) #f generic))))

(add-method compute-apply-generic
 (make-method (list <generic>)
  (lambda (call-next-method generic)
	 (lambda args
	  (if (and (memq generic generic-invocation-generics)     ;* G  c
		     (memq (car args) generic-invocation-generics)) ;* r  a
	   (apply (method-procedure                            ;* o  s
		         (last (generic-methods generic)))           ;* u  e
		  (cons #f args))                              ;* n
                                        ;* d
	   ((compute-apply-methods generic)
	    ((compute-methods generic) args)
	    args))))))


(add-method compute-methods
 (make-method (list <generic>)
  (lambda (call-next-method generic)
	 (lambda (args)
	  (let ((applicable
		       (collect-if (lambda (method)
                                        ;
                                        ; Note that every only goes as far as the
                                        ; shortest list!
                                        ;
			                  (every applicable?
				                 (method-specializers method)
				                 args))
			      (generic-methods generic))))
	   (gsort (lambda (m1 m2)
		         ((compute-method-more-specific? generic)
		          m1
		          m2
		          args))
		  applicable))))))


(add-method compute-method-more-specific?
 (make-method (list <generic>)
  (lambda (call-next-method generic)
	 (lambda (m1 m2 args)
	  (let loop ((specls1 (method-specializers m1))
		           (specls2 (method-specializers m2))
		           (args args))
	   (cond ((and (null? specls1) (null? specls2))
            (error
             "Two methods are equally specific."))
      ((or  (null? specls1) (null? specls2))
       (error
        "Two methods have a different number of specializers."))
		  ((null? args)
		   (error
        "Fewer arguments than specializers."))
		  (else
		   (let ((c1  (car specls1))
			       (c2  (car specls2))
			       (arg (car args)))
		    (if (eq? c1 c2)
			   (loop (cdr specls1)
			    (cdr specls2)
			    (cdr args))
			   (more-specific? c1 c2 arg))))))))))


(add-method compute-apply-methods
 (make-method (list <generic>)
  (lambda (call-next-method generic)
	 (lambda (methods args)
	  (letrec ((one-step
		          (lambda (tail)
		           (lambda ()
			          (if (null? tail)
			           (error "No applicable methods/next methods.")
			           (apply (method-procedure (car tail))
				          (cons (one-step (cdr tail)) args)))))))
	   ((one-step methods)))))))

(define applicable?
 (lambda (c arg)
  (memq c (class-cpl (class-of arg)))))

(define more-specific?
 (lambda (c1 c2 arg)
  (memq c2 (memq c1 (class-cpl (class-of arg))))))



(add-method initialize
 (make-method (list <object>)
  (lambda (call-next-method object initargs) object)))

(add-method initialize
 (make-method (list <class>)
  (lambda (call-next-method class initargs)
	 (call-next-method)
	 (slot-set! class
		'direct-supers
		(getl initargs 'direct-supers '()))
	 (slot-set! class
		'direct-slots
		(map (lambda (s)
			    (if (pair? s) s (list s)))
		 (getl initargs 'direct-slots  '())))
	 (slot-set! class 'cpl   (compute-cpl   class))
	 (slot-set! class 'slots (compute-slots class))
	 (let* ((nfields 0)
	        (field-initializers '())
	        (allocator
		       (lambda (init)
		        (let ((f nfields))
		         (set! nfields (+ nfields 1))
		         (set! field-initializers
			        (cons init field-initializers))
		         (list (lambda (o)   (%instance-ref  o f))
			        (lambda (o n) (%instance-set! o f n))))))
	        (getters-n-setters
		       (map (lambda (slot)
		             (cons (car slot)
			            (compute-getter-and-setter class
							     slot
							     allocator)))
		        (slot-ref class 'slots))))
	  (slot-set! class 'nfields nfields)
	  (slot-set! class 'field-initializers field-initializers)
	  (slot-set! class 'getters-n-setters getters-n-setters)))))

(add-method initialize
 (make-method (list <generic>)
  (lambda (call-next-method generic initargs)
	 (call-next-method)
	 (slot-set! generic 'methods '())
	 (%set-instance-proc! generic
		(lambda args (error "Has no methods."))))))

(add-method initialize
 (make-method (list <method>)
  (lambda (call-next-method method initargs)
	 (call-next-method)
	 (slot-set! method 'specializers (getl initargs 'specializers))
	 (slot-set! method 'procedure    (getl initargs 'procedure)))))



(add-method allocate-instance
 (make-method (list <class>)
  (lambda (call-next-method class)
	 (let* ((field-initializers (slot-ref class 'field-initializers))
	        (new (%allocate-instance
		            class
		            (length field-initializers))))
	  (let loop ((n 0)
		           (inits field-initializers))
	   (if (pair? inits)
		  (begin
		   (%instance-set! new n ((car inits)))
		   (loop (+ n 1)
		    (cdr inits)))
		  new))))))

(add-method allocate-instance
 (make-method (list <entity-class>)
  (lambda (call-next-method class)
	 (let* ((field-initializers (slot-ref class 'field-initializers))
	        (new (%allocate-entity
		            class
		            (length field-initializers))))
	  (let loop ((n 0)
		           (inits field-initializers))
	   (if (pair? inits)
		  (begin
		   (%instance-set! new n ((car inits)))
		   (loop (+ n 1)
		    (cdr inits)))
		  new))))))


(add-method compute-cpl
 (make-method (list <class>)
  (lambda (call-next-method class)
	 (compute-std-cpl class class-direct-supers))))


(add-method compute-slots
 (make-method (list <class>)
  (lambda (call-next-method class)
	 (let collect ((to-process (apply append
					                    (map class-direct-slots
					                     (class-cpl class))))
		             (result '()))
	  (if (null? to-process)
	   (reverse result)
	   (let* ((current (car to-process))
		        (name (car current))
		        (others '())
		        (remaining-to-process
		         (collect-if (lambda (o)
				                  (if (eq? (car o) name)
					                 (begin
					                  (set! others (cons o others))
					                  #f)
					                 #t))
				      (cdr to-process))))
		  (collect remaining-to-process
			 (cons (append current
				      (apply append (map cdr others)))
			  result))))))))


(add-method compute-getter-and-setter
 (make-method (list <class>)
  (lambda (call-next-method class slot allocator)
	 (allocator (lambda () '())))))



                                        ;
                                        ; Now everything works, both generic functions and classes, so we can
                                        ; turn on the real MAKE.
                                        ;
                                        ;
(set! make
 (lambda (class . initargs)
	(let ((instance (allocate-instance class)))
	 (initialize instance initargs)
	 instance)))

                                        ;
                                        ; Now define what CLOS calls `built in' classes.
                                        ;
                                        ;
(define <primitive-class>
 (make <class>
	'direct-supers (list <class>)
	'direct-slots  (list)))

(define make-primitive-class
 (lambda class
  (make (if (null? class) <primitive-class> (car class))
	 'direct-supers (list <top>)
	 'direct-slots  (list))))


(define <pair>        (make-primitive-class))
(define <null>        (make-primitive-class))
(define <symbol>      (make-primitive-class))
(define <boolean>     (make-primitive-class))
(define <procedure>   (make-primitive-class <procedure-class>))
(define <number>      (make-primitive-class))
(define <vector>      (make-primitive-class))
(define <char>        (make-primitive-class))
(define <string>      (make-primitive-class))
(define  <input-port> (make-primitive-class))
(define <output-port> (make-primitive-class))


                                        ;
                                        ; All done.
                                        ;
                                        ;

'tiny-clos-up-and-running
