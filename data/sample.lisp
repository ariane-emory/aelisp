(setq nl (lambda ()  (princ "
")))
(setq stop   (lambda () (nl) (exit 0)))
(setq sleep  (lambda (x) (msleep (* 1000 x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (env :syms))
(nl)
(nl)

(princ "BEFORE add") (nl)
(write (setq add (macro (name params) (list (quote +) name params)))) (nl)
(princ "Params:   ") (write (params add)) (nl)
(princ "Body:     ") (write (body add)) (nl)
(princ "Expanded  ") (write (add 7 8)) (nl)
(princ "Evaled:   ") (write (eval (add 7 8))) (nl)
(princ "AFTER") (nl)
(nl)

(princ "BEFORE defmacro") (nl)
;;     (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))
(write (setq defmacro (macro (name params . body) (list name params . body))))
(nl)
(princ "Params:   ") (write (params defmacro)) (nl)
(princ "Body:     ") (write (body defmacro)) (nl)
(princ "Expanded  ") (write (defmacro moop (x y) (list 1)))
(nl)
(princ "AFTER") (nl)
(nl)




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


