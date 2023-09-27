(setq nl (lambda ()  (princ "
")))
(setq stop   (lambda ()  (terpri) (exit 0)))
(setq sleep  (lambda (x) (msleep (* 1000 x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (env :syms))
(nl)

(princ "BEFORE add") (nl)
(write (setq add   (macro (xxx yyy) (list (quote +) xxx yyy)))) (nl)
(princ "Params: ") (write (params add)) (nl)
(princ "Body:   ") (write (body add)) (nl)
(princ "AFTER") (nl)
(nl)
 
(princ "BEFORE defmacro") (nl)
(write (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))) (nl)
(princ "Params: ") (write (params defmacro)) (nl)
(princ "Body:   ") (write (body   defmacro))   (nl)
(princ "AFTER") (nl)
(nl)

(princ "BEFORE madd1") (nl)
(write (setq madd (macro (xxx yyy) (list (quote +) xxx yyy)))) (nl)
(princ "Params: ") (write (params madd)) (nl)
(princ "Body:   ") (write (body madd)) (nl)
(princ "AFTER") (nl)
(nl)

(princ "BEFORE madd2") (nl)
(write (eval (defmacro madd (xxx yyy) (list (quote +) xxx yyy))))
(princ "Params: ") (write (params madd)) (nl)
(princ "Body:   ") (write (body madd)) (nl)
(princ "AFTER") (nl)
(nl)



(stop)

(princ "BEFORE defun") (nl)
(setq defun (macro (name params . body) (list (quote setq) name (list (quote lambda) params . body))))
(defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))
(write defun) (nl)
(princ "Params: ") (write (params defun)) (nl)
(princ "Body:   ") (write (body defun)) (nl)
(princ "AFTER") ; (nl)
(nl)


