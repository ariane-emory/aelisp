;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crucial macros, without which nothing else in std will even work:                     ;;
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
;; type predicates:                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type?     (typ o) (eq? typ       (type           o)))
(defun atom?     (o)     (not (type?    :CONS           o)))
(defun char?     (o)          (type?    :CHAR           o))
(defun cons?     (o)          (type?    :CONS           o))
(defun core?     (o)          (type?    :CORE           o))
(defun env?      (o)          (type?    :ENV            o))
(defun error?    (o)          (type?    :ERROR          o))
(defun float?    (o)          (type?    :FLOAT          o))
(defun integer?  (o)          (type?    :INTEGER        o))
(defun lambda?   (o)          (type?    :LAMBDA         o))
(defun λ?        (o)          (type?    :LAMBDA         o))
(defun macro?    (o)          (type?    :MACRO          o))
(defun rational? (o)          (type?    :RATIONAL       o))
(defun string?   (o)          (type?    :STRING         o))
(defun symbol?   (o)          (type?    :SYMBOL         o))
(defun improper? (o)     (and (list? o) (not (proper?   o))))
(defun fun?      (o)     (or  (core? o) (lambda? o) (macro? o)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! pair? cons?) ;; scheme compatability
(setq! null? nil?)  ;; scheme compatability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! memq?
 (lambda (elem lst)
  (cond
    ((eq? elem (car lst)) t)
    (lst (memq? elem (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun feature? (feature)
 "t if FEATURE is present in *features*."
 (unless (and (symbol? feature) (not (keyword? feature)))
  (error "FEATURE must be a non-keyword symbol"))
 (letrec
  ((private-memq?
    (lambda (elem lst)
     (cond
      ((eq? elem (car lst)) t)
      (lst (private-memq? elem (cdr lst))))))
   (memq? private-memq?)
   )
 (memq? feature *features*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun provide (feature)
 "Add FEATURE to *features* if it is not already present"
 (unless (and (symbol? feature) (not (keyword? feature)))
  (error "FEATURE must be a non-keyword symbol"))
 (unless (feature? feature)  (push! feature *features*))
 feature)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'std-minimal)
