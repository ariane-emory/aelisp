(require 'std-minimal)
(require 'measure-time)

(report-time-us "def tail-chaser macros         "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (tail chaser macros):                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro make-chase-fun (params . cond-clauses)
  "Creates a function for recursive traversal and processing of lists.
  
  This macro is a generalized version that allows customization of 
  the parameter order through the PARAMS parameter.
  
  PARAMS:       A list of length 2 that specifies the parameter order.
                One parameter must be the symbol 'lst.
  COND-CLAUSES: The conditions to process the list."
  (unless (cons? params)        (error "PARAMS must be a list"))
  (unless (cons? cond-clauses)  (error "COND-clauses must be a list"))
  (unless (= 2 (length params)) (error "PARAMS needs length 2"))
  (let* ((lst-is-first? (eq? 'lst (first  params)))
         (user-param    (if lst-is-first? (second params) (first params)))
         (lambda-params (cons (first params) (cons (second params) 'rest))))
   (unless (or lst-is-first? (eq? 'lst (second params)))
    (error "one of the PARAMS must be the symbol 'lst"))
   `(lambda ,lambda-params
     (let ((position lst))
      (letrec
       ((chase-internal
         (lambda (,user-param lst . rest)
          (setq! position lst)
          (let ((head (car position))
                (tail (cdr position)))
           (cond ,@cond-clauses))))
        (chase
         (lambda (,user-param . rest)
          (chase-internal ,user-param (cdr position) . rest))))
       (chase-internal ,user-param position . rest))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'tail-chaser-macros)
