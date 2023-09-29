;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq atom?     atomp  )
(setq proper?   properp)
(setq tail?     tailp  )
(setq err-msg   errmsg )
(setq err-obj   errobj )
(setq assq      aget   )
(setq assq?     ahas   )

(setq nl        (lambda ()      (princ "
")))

;; (setq list   (lambda args   args                       ))
(setq list      (lambda (h . t) (cons    h       t       )))
(setq stop      (lambda ()      (nl)    (exit    0       )))
(setq sleep     (lambda (s)     (msleep (* 1000  s      ))))

(setq type?     (lambda (t o)   (eq      t      (type o ))))
(setq cons?     (lambda (o)     (not    (atom?   o      ))))
(setq error?    (lambda (o)     (type?  :ERROR   o       )))
(setq improper? (lambda (o)     (not    (proper? o      )))) ;; this also needs to check if it's arg is tail?.
(setq nil?      (lambda (o)     (eq      o       nil     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq log
  (lambda (string obj)
    (princ string)
    (princ " ")
    (princ obj)
    (nl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test some random stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log   "aenv:                       " (env                )) 
(log   "atype:                      " (type     (env     ))) 
(log   "aparent:                    " (parent   (env     ))) 
(log   "asyms:                      " (syms     (env     ))) 
(log   "avals:                      " (vals     (env     )))
(nl)

(log   "core lambda:                "  lambda              ) 
(log   "type:                       " (type      lambda   )) 
(nl)

(log   "lambda sleep:               "  sleep               ) 
(log   "type:                       " (type      sleep    )) 
(log   "parent:                     " (parent    sleep    )) 
(log   "params:                     " (params    sleep    )) 
(log   "body:                       " (body      sleep    ))
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq   x (quote (1 2 . nil)))

(log   "(1 2 . nil) is proper:      " (properp   x        ))
(log   "(1 2 . nil) length:         " (length    x        ))
(log   "(1 2 . nil) length errors:  " (error?   (length x)))
(log   "(1 2 . nil) is:             "  x                   ) 
(log   "(car (1 2 . nil)) is:       " (car       x        )) 
(log   "(cdr (1 2 . nil)) is:       " (cdr       x        )) 
(nl)
 
(setq   x (quote (1 2 . 333)))

(log   "(1 2 . 333) is improper:    " (improper? x        )) 
(log   "(1 2 . 333) length:         " (length    x        )) 
(log   "(1 2 . 333) length errors:  " (error?   (length x)))
(log   "(1 2 . 333) is:             "  x                   ) 
(log   "(car (1 2 . 333)) is:       " (car       x        )) 
(log   "(cdr (1 2 . 333)) is:       " (cdr       x        )) 
(nl)

(setq   x (quote (1 2 3 . nil)))

(log   "(1 2 3 . nil) is proper:    " (properp   x        )) 
(log   "(1 2 3 . nil) length:       " (length    x        )) 
(log   "(1 2 3 . nil) length errors:" (error?   (length x))) 
(log   "(1 2 3 . nil) is:           "  x                   ) 
(log   "(car (1 2 3 . nil)) is:     " (car       x        )) 
(log   "(cdr (1 2 3 . nil)) is:     " (cdr       x        )) 
(nl)
 
(setq   x (quote (1 2 3 . 333)))

(log   "(1 2 3 . 333) is improper:  " (improper? x        )) 
(log   "(1 2 3 . 333) length:       " (length    x        )) 
(log   "(1 2 3 . 333) length errors:" (error?   (length x))) 
(log   "(1 2 3 . 333) is:           "  x                   ) 
(log   "(car (1 2 3 . 333)) is:     " (car x              )) 
(log   "(cdr (1 2 3 . 333)) is:     " (cdr x              )) 
(nl)

(log   "int:                        " (type? :INTEGER    3)) 
(log   "! int:                      " (type? :FLOAT      3)) 
(log   "! float:                    " (type? :INTEGER  3.0)) 
(log   "float:                      " (type? :FLOAT    3.0)) 
(log   "float:                      " (type? :FLOAT    3. )) 
(log   "float:                      " (type? :FLOAT     .3)) 
(log   "rational:                   " (type? :RATIONAL 3/4)) 
(log   "string:                     " (type? :STRING   "3")) 
(nl)

(setq   err (length '(1 2 . 333)))

(log   "This error:                 "         err          ) 
(log   "This error's message:       " (errmsg err         ))
(log   "This error's obj:           " (errobj err         )) 
(nl)

(setq   alist  nil                     )
(setq   alist (aset alist 'name "Bob" ))
(setq   alist (aset alist 'age   24   ))
(setq   alist (aset alist 'type 'human))

;; This a-set doesn't work correctly. Looking/setting up in wrong env maybe?
(setq   a-set
  (lambda (al key val)
    (setq al (aset (eval al) key val))))

(princ "a-setted") (write (a-set 'alist 'hair 'red))    (nl)

(log   "alist:                      "  alist               )

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
