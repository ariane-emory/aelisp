;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; std config:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *use-safe-provide*        nil)
(setq! *microbench-defmacros*    t)
(setq! *microbench-defuns*       t)
(setq! *microbench-provides*     t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simpler-version of std load time measuerement:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *time-before-loading-std* (now-us))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crucial macros, without which nothing else in std will even work:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! defmacro
 (macro (name params . body)
  (unless (eq? :SYMBOL (type name))
   (error "NAME must be a symbol"))
  (unless (or (eq? :CONS (type params)) (eq? :SYMBOL (type params)))
   (error "PARAMS must be a list or symbol"))
  (unless (eq? :CONS (type body))
   (error "BODY must be a cons"))
  $('setq! name $('macro params . body))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun (name params . body)
 (unless (eq? :SYMBOL (type name))
  (error "NAME must be a symbol"))
 (unless (or (eq? :CONS (type params)) (eq? :SYMBOL (type params)))
  (error "PARAMS must be a list or symbol"))
 (unless (eq? :CONS (type body))
  (error "BODY must be a cons"))
 $('setq! name $('lambda params . body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; feature? and provide:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let
 ((feature-memq?
   (if (bound? 'memq?)
    memq?
    (lambda (elem lst)
     (let ((found nil))
      (while (and lst (not found))
       (if (eq? elem (car lst))
        (setq! found t)
        (setq! lst (cdr lst))))
      found)))))
 (defun feature? (feature)
  "t if FEATURE is present in *features*."
  (unless (and (eq? :SYMBOL (type feature)) (not (keyword? feature)))
   (error "FEATURE must be a non-keyword symbol"))
  (feature-memq? feature *features*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun safe-provide (feature)
 "Add FEATURE to *features* if it is not already present."
 (unless (and (eq? :SYMBOL (type feature)) (not (keyword? feature)))
  (error "FEATURE must be a non-keyword symbol"))
 (unless (feature? feature) (push! feature *features*))
 feature)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun provide! (feature)
 "Add FEATURE to *features*."
 (push! feature *features*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! provide (if *use-safe-provide* safe-provide provide!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; microbenchmark:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and *microbench-enabled* *microbench-defmacros*)
 (setq! defmacro-base defmacro)
 (setq! defmacro
  (macro (name params . body)
   $('progn
     $('setq! '*microbench-before* $('now-us))
     $('let $($('microbenced-macro's-result $('defmacro-base name params . body)))
       $('princ "defmacrod  " $('quote name) " in "
         $('elapsed-us '*microbench-before*) " us.")
       $('nl)
       $('put! $('quote name) ':last-bound-to 'microbenched-fun's-result)
       'microbenced-macro's-result)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and *microbench-enabled* *microbench-defuns*)
 (setq! defun-base defun)
 (defmacro defun (name params . body)
  $('progn
    $('setq! '*microbench-before* $('now-us))
    $('let $($('microbenched-fun's-result $('defun-base name params . body)))
      $('princ "defunned   " $('quote name) " in "
        $('elapsed-us '*microbench-before*) " us.")
      $('nl)
      $('put! $('quote name) ':last-bound-to 'microbenched-fun's-result)
      'microbenched-fun's-result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and *microbench-enabled* *microbench-provides*)
 (setq! provide-base provide)
 (defun provide (feature)
  (setq! *microbench-before* (now-us))
  (let ((result (provide-base feature)))
   (princ "provide    '" feature " in " (elapsed-us *microbench-before*) " us.")
   (nl)
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; already defined above:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'features)
(provide 'microbench)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! type?     (lambda (typ o) (eq? typ       (type           o))))
(setq! atom?     (lambda (o)     (not (type?    :CONS           o))))
(setq! char?     (lambda (o)          (type?    :CHAR           o)))
(setq! cons?     (lambda (o)          (type?    :CONS           o)))
(setq! core?     (lambda (o)          (type?    :CORE           o)))
(setq! env?      (lambda (o)          (type?    :ENV            o)))
(setq! error?    (lambda (o)          (type?    :ERROR          o)))
(setq! float?    (lambda (o)          (type?    :FLOAT          o)))
(setq! integer?  (lambda (o)          (type?    :INTEGER        o)))
(setq! lambda?   (lambda (o)          (type?    :LAMBDA         o)))
(setq! λ?        (lambda (o)          (type?    :LAMBDA         o)))
(setq! macro?    (lambda (o)          (type?    :MACRO          o)))
(setq! rational? (lambda (o)          (type?    :RATIONAL       o)))
(setq! string?   (lambda (o)          (type?    :STRING         o)))
(setq! symbol?   (lambda (o)          (type?    :SYMBOL         o)))
(setq! !nil?     (lambda (o)     (not (nil?  o))))
(setq! improper? (lambda (o)     (and (list? o) (not (proper? o)))))
(setq! fun?      (lambda (o)     (or  (core? o) (lambda? o) (macro? o))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list?     tail?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! zero?     (lambda (n)   (= n 0)))
(setq! one?      (lambda (n)   (= n 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when *log-loading-std-enabled*
;;  (log-eval t)
;;  (log-core t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'std-fundamental)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
