(setq nl     (lambda ()  (princ "
")))
(setq stop   (lambda ()  (nl) (exit 0)))
(setq sleep  (lambda (x) (msleep (* 1000 x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "env:                     ") (princ           (env))               (nl)
(princ "type:                    ") (princ (type     (env)))              (nl)
(princ "parent:                  ") (princ (parent   (env)))              (nl)
(princ "syms:                    ") (princ (syms     (env)))              (nl)
(princ "vals:                    ") (princ (vals     (env)))              (nl)
(nl)

(princ "core lambda:             ") (princ           lambda)              (nl)
(princ "type:                    ") (princ (type     lambda))             (nl)
(nl)

(princ "lambda sleep:            ") (princ           sleep)               (nl)
(princ "type:                    ") (princ (type     sleep))              (nl)
(princ "parent:                  ") (princ (parent   sleep))              (nl)
(princ "params:                  ") (princ (params   sleep))              (nl)
(princ "body:                    ") (princ (body     sleep))              (nl)
(nl)

(princ "(1 . nil) is proper:     ") (princ      (properp  (cons 1 nil)))  (nl)
(princ "(1 . 2) is improper:     ") (princ (not (properp  (cons 1 2  )))) (nl)
(princ "(1 . nil) length:        ") (princ      (length   (cons 1 nil)))  (nl)
(princ "(1 . 2) length:          ") (princ (not (length   (cons 1 2  )))) (nl)
(nl)

(princ "(1 2 . nil) is proper:   ") (princ      (properp  (cons 1 2 . nil)))  (nl)
(princ "(1 2 . 3) is improper:   ") (princ (not (properp  (cons 1 2 . 3  )))) (nl)
(princ "(1 2 . nil) length:      ") (princ      (length   (cons 1 2 . nil)))  (nl)
(princ "(1 2 . 3) length:        ") (princ (not (length   (cons 1 2 . 3  )))) (nl)
(nl)

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


