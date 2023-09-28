;; `(1 ,@some-list 4 ,@some-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nl        (lambda ()     (princ "
")))
(setq sleep     (lambda (s)    (msleep (* 1000  s  ))))
(setq stop      (lambda ()     (nl)    (exit    0   )))
;; (setq list   (lambda args   args                  ))
(setq list      (lambda (h . t) (cons h t)))

(setq atom?     atomp)
(setq proper?   properp)
(setq tail?     tailp)
(setq err-msg   errmsg)
(setq err-obj   errobj)

(setq cons?     (lambda (o)    (not    (atom?   o  ))))
(setq error?    (lambda (o)    (type?  :ERROR   o   )))
(setq improper? (lambda (o)    (not    (proper? o  )))) ;; this also needs to check if it's arg is tail?.
(setq nil?      (lambda (o)    (eq     o        nil )))
(setq type?     (lambda (t o)  (eq     (type o) t   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test some random stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "env:                         ") (princ           (env))          (nl)
(princ "type:                        ") (princ (type     (env)))         (nl)
(princ "parent:                      ") (princ (parent   (env)))         (nl)
(princ "syms:                        ") (princ (syms     (env)))         (nl)
(princ "vals:                        ") (princ (vals     (env)))         (nl)
(nl)

(princ "core lambda:                 ") (princ           lambda)         (nl)
(princ "type:                        ") (princ (type     lambda))        (nl)
(nl)

(princ "lambda sleep:                ") (princ           sleep)          (nl)
(princ "type:                        ") (princ (type     sleep))         (nl)
(princ "parent:                      ") (princ (parent   sleep))         (nl)
(princ "params:                      ") (princ (params   sleep))         (nl)
(princ "body:                        ") (princ (body     sleep))         (nl)
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x (quote (1 2 . nil)))

(princ "(1 2 . nil) is proper:       ") (princ         (properp   x ))   (nl)
(princ "(1 2 . nil) length:          ") (princ         (length    x ))   (nl)
(princ "(1 2 . nil) length errors:   ") (princ (error? (length    x)))   (nl)
(princ "(1 2 . nil) is:              ") (princ                    x  )   (nl)
(princ "(car (1 2 . nil)) is:        ") (princ               (car x ))   (nl)
(princ "(cdr (1 2 . nil)) is:        ") (princ               (cdr x ))   (nl)
(nl)
 
(setq x (quote (1 2 . 333)))

(princ "(1 2 . 333) is improper:     ") (princ       (improper?   x ))   (nl)
(princ "(1 2 . 333) length:          ") (princ         (length    x ))   (nl)
(princ "(1 2 . 333) length errors:   ") (princ (error? (length    x)))   (nl)
(princ "(1 2 . 333) is:              ") (princ                    x  )   (nl)
(princ "(car (1 2 . 333)) is:        ") (princ               (car x ))   (nl)
(princ "(cdr (1 2 . 333)) is:        ") (princ               (cdr x ))   (nl)
(nl)

(setq x (quote (1 2 3 . nil)))

(princ "(1 2 3 . nil) is proper:     ") (princ         (properp   x ))   (nl)
(princ "(1 2 3 . nil) length:        ") (princ         (length    x ))   (nl)
(princ "(1 2 3 . nil) length errors: ") (princ (error? (length    x)))   (nl)
(princ "(1 2 3 . nil) is:            ") (princ                    x  )   (nl)
(princ "(car (1 2 3 . nil)) is:      ") (princ               (car x ))   (nl)
(princ "(cdr (1 2 3 . nil)) is:      ") (princ               (cdr x ))   (nl)
(nl)
 
(setq x (quote (1 2 3 . 333)))

(princ "(1 2 3 . 333) is improper:   ") (princ       (improper?   x ))   (nl)
(princ "(1 2 3 . 333) length:        ") (princ         (length    x ))   (nl)
(princ "(1 2 3 . 333) length errors: ") (princ (error? (length    x)))   (nl)
(princ "(1 2 3 . 333) is:            ") (princ                    x  )   (nl)
(princ "(car (1 2 3 . 333)) is:      ") (princ               (car x ))   (nl)
(princ "(cdr (1 2 3 . 333)) is:      ") (princ               (cdr x ))   (nl)
(nl)

(princ "int:                         ") (princ  (type? :INTEGER    3))   (nl)
(princ "! int:                       ") (princ  (type? :FLOAT      3))   (nl)
(princ "! float:                     ") (princ  (type? :INTEGER  3.0))   (nl)
(princ "float:                       ") (princ  (type? :FLOAT    3.0))   (nl)
(princ "float:                       ") (princ  (type? :FLOAT    3. ))   (nl)
(princ "float:                       ") (princ  (type? :FLOAT     .3))   (nl)
(princ "rational:                    ") (princ  (type? :RATIONAL 3/4))   (nl)
(princ "string:                      ") (princ  (type? :STRING   "3"))   (nl)
(nl)

(setq err (length '(1 2 . 333)))

(princ "This error:                  ") (princ  err)                     (nl)
(princ "This error's obj:            ") (write  (errobj err))            (nl)
(princ "This error's message:        ") (write  (errmsg err))            (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(stop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq test (lambda (first . rest)
             (princ "first:  ")  (write first)
             (princ " rest:   ") (write rest)
             first))
  
(test 1 2 3 4)

(nl)

(setq test (macro (first . rest)
             (princ "first:  ")  (write first)
             (princ " rest:   ") (write rest)
             first))

(write (test 1 2 3 4))
(nl)

(eval (test 1 2 3 4))

(stop)

(princ "numer:               ") (princ (numer 3/4))         (nl)
(princ "denom:               ") (princ (denom 3/4))         (nl)
(nl)

(princ "BEFORE add") (nl)
(write (setq add (macro (name params) (list (quote +) name params)))) (nl)
(princ "Params:              ") (write (params add))        (nl)
(princ "Body:                ") (write (body add))          (nl)
(princ "Expanded             ") (write (add 7 8))           (nl)
(princ "Evaled:              ") (write (eval (add 7 8)))    (nl)
(princ "AFTER") (nl)
(nl)

(princ "BEFORE defmacro") (nl)
;;     (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))
(write (setq defmacro (macro (name params body) (list (quote setq) name (list (quote macro) params body))))) (nl)
(princ "Params:              ") (write (params defmacro))                                                                                       (nl)
(princ "Body:                ") (write (body   defmacro))                                                                                       (nl)
(princ "Expanded             ") (write        (defmacro blam (x y) (list (+ x y))))                                                             (nl)
(princ "Evaled:              ") (write (eval  (defmacro blam (x y) (list (+ x y)))))                                                            (nl)
(princ "Set:                 ") (write                  blam)                                                                                   (nl)
(princ "Ev. params:          ") (write (params          blam))                                                                                  (nl)
(princ "Ev. body:            ") (write (body            blam))                                                                                  (nl)
(princ "2 Expanded           ") (write        (defmacro defun (name params body) (list (quote setq) name (list (quote lambda) params body))))   (nl)
(princ "2 Evaled:            ") (write (eval  (defmacro defun (name params body) (list (quote setq) name (list (quote lambda) params body)))))  (nl)
(princ "2 Set:               ") (write                  defun)                                                                                  (nl)
(princ "2 Ev. params:        ") (write (params          defun))                                                                                 (nl)
(princ "2 Ev. body  :        ") (write (body            defun))                                                                                 (nl)
;(princ "3 Called:            ") (write (               (defun hello (str) (list (quote (print "Hello "))))))                                    (nl)
(princ "3 Called:            ") (write                  (defun x (y) (quote z) (quote y)))                                                      (nl)
(princ "AFTER") (nl)
(nl)

;; (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))

(stop)

(princ "BEFORE madd2") (nl)
(write (eval (defmacro madd (xxx yyy) (list (quote +) xxx yyy))))
(princ "Params: ") (write (params madd)) (nl)
(princ "Body:   ") (write (body madd)) (nl)
(princ "AFTER") (nl)
(nl)

(princ "BEFORE madd1") (nl)
(write (setq madd (macro (xxx yyy) (list (quote +) xxx yyy)))) (nl)
(princ "Params: ") (write (params madd)) (nl)
(princ "Body:   ") (write (body madd)) (nl)
(princ "AFTER") (nl)
(nl)

(princ "BEFORE defun") (nl)
(setq defun (macro (name params . body) (list (quote setq) name (list (quote lambda) params . body))))
(defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))
(write defun) (nl)
(princ "Params: ") (write (params defun)) (nl)
(princ "Body:   ") (write (body defun)) (nl)
(princ "AFTER") ; (nl)
(nl)
m
